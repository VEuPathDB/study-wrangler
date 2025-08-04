import Docker from 'dockerode';
import * as path from 'path';
import * as fs from 'fs';
import * as net from 'net';
import { RServeClient } from './rserve-client';

interface SessionOptions {
  devMode?: boolean;
}

export class ContainerManager {
  private docker: Docker;
  private currentContainer: Docker.Container | null = null;

  constructor() {
    this.docker = new Docker();
  }

  async startSession(workingDirectory: string, options: SessionOptions = {}): Promise<void> {
    const { devMode = this.detectDevMode(workingDirectory) } = options;
    
    console.log(`Starting RServe container in ${devMode ? 'DEVELOPMENT' : 'PRODUCTION'} mode`);
    
    // Configure volume mounts
    const binds = [
      `${workingDirectory}:/data`  // Always mount user's working directory
    ];
    
    const environment: string[] = [];
    
    if (devMode) {
      // In dev mode, mount the study-wrangler R source
      const studyWranglerPath = this.findStudyWranglerSource(workingDirectory);
      if (studyWranglerPath) {
        binds.push(`${studyWranglerPath}:/study.wrangler.dev`);
        environment.push('DEV_MODE=true');
        console.log(`Mounting R source from: ${studyWranglerPath}`);
      } else {
        console.warn('Dev mode requested but study-wrangler source not found');
      }
    }

    // Start container
    const [container] = await this.docker.run(
      'veupathdb/study-wrangler:rserve',
      [],
      process.stdout,
      {
        HostConfig: {
          Binds: binds,
          PortBindings: { '6311/tcp': [{ HostPort: '6311' }] },
          AutoRemove: true
        },
        Env: environment,
        AttachStdout: true,
        AttachStderr: true
      }
    );

    this.currentContainer = container;

    // Wait for RServe to be ready
    await this.waitForRServe();
    console.log('RServe container ready');
  }

  private detectDevMode(workingDirectory: string): boolean {
    // Check if we're running from within a study-wrangler repository
    const possiblePaths = [
      workingDirectory,                    // Current directory
      path.join(workingDirectory, '..'),   // Parent directory
      path.dirname(workingDirectory)       // Directory containing current
    ];

    return possiblePaths.some(dir => {
      const descriptionPath = path.join(dir, 'DESCRIPTION');
      if (fs.existsSync(descriptionPath)) {
        try {
          const description = fs.readFileSync(descriptionPath, 'utf8');
          return description.includes('Package: study.wrangler');
        } catch (e) {
          return false;
        }
      }
      return false;
    });
  }

  private findStudyWranglerSource(workingDirectory: string): string | null {
    // Find the study-wrangler R package source directory
    const possiblePaths = [
      workingDirectory,
      path.join(workingDirectory, '..'),
      path.dirname(workingDirectory),
      path.join(workingDirectory, '../..') // For bio-wrangler-mcp/src development
    ];

    for (const dir of possiblePaths) {
      const descriptionPath = path.join(dir, 'DESCRIPTION');
      if (fs.existsSync(descriptionPath)) {
        try {
          const description = fs.readFileSync(descriptionPath, 'utf8');
          if (description.includes('Package: study.wrangler')) {
            return path.resolve(dir);
          }
        } catch (e) {
          continue;
        }
      }
    }
    return null;
  }

  private async waitForRServe(maxAttempts: number = 30, delayMs: number = 1000): Promise<void> {
    // Wait for RServe to be ready on port 6311
    for (let i = 0; i < maxAttempts; i++) {
      try {
        await new Promise<void>((resolve, reject) => {
          const socket = net.createConnection(6311, 'localhost');
          socket.on('connect', () => {
            socket.end();
            resolve();
          });
          socket.on('error', reject);
          setTimeout(() => reject(new Error('timeout')), 2000);
        });
        return; // Success
      } catch (e) {
        if (i === maxAttempts - 1) {
          throw new Error(`RServe not ready after ${maxAttempts} attempts`);
        }
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
    }
  }

  async reloadStudyWrangler(rserveClient: RServeClient): Promise<{ success: boolean; message: string }> {
    console.log('Reloading study.wrangler with devtools::load_all()');
    
    try {
      const result = await rserveClient.reloadStudyWrangler();
      if (result.success) {
        console.log('✓ Reload successful:', result.output);
        return { success: true, message: result.output };
      } else {
        console.error('✗ Reload failed:', result.error);
        return { success: false, message: result.error || 'Unknown error' };
      }
    } catch (error) {
      const message = `Failed to communicate with RServe: ${(error as Error).message}`;
      console.error('✗', message);
      return { success: false, message };
    }
  }

  async stopSession(): Promise<void> {
    if (this.currentContainer) {
      try {
        await this.currentContainer.kill();
        console.log('RServe container stopped');
      } catch (e) {
        console.warn('Error stopping container:', (e as Error).message);
      }
      this.currentContainer = null;
    }
  }
}
