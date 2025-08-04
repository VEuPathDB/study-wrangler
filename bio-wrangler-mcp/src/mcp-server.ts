import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import { ContainerManager } from './container-manager.js';
import { RServeClient } from './rserve-client.js';

class BioWranglerMCPServer {
  private server: Server;
  private containerManager: ContainerManager;
  private rserveClient: RServeClient | null = null;

  constructor() {
    this.server = new Server(
      {
        name: 'bio-wrangler-mcp',
        version: '0.1.0',
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.containerManager = new ContainerManager();
    this.setupToolHandlers();
  }

  private setupToolHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: [
          {
            name: 'start_wrangling_session',
            description: 'Start RServe container with study.wrangler loaded',
            inputSchema: {
              type: 'object',
              properties: {
                directory: {
                  type: 'string',
                  description: 'Working directory to mount (defaults to current directory)',
                },
                devMode: {
                  type: 'boolean',
                  description: 'Enable development mode with source mounting',
                },
              },
            },
          },
          {
            name: 'reload_study_wrangler',
            description: 'Reload study.wrangler package using devtools::load_all() (dev mode only)',
            inputSchema: {
              type: 'object',
              properties: {},
            },
          },
          {
            name: 'execute_r_code',
            description: 'Execute R code in the RServe session',
            inputSchema: {
              type: 'object',
              properties: {
                code: {
                  type: 'string',
                  description: 'R code to execute',
                },
              },
              required: ['code'],
            },
          },
          {
            name: 'inspect_file',
            description: 'Inspect a data file using study.wrangler',
            inputSchema: {
              type: 'object',
              properties: {
                filepath: {
                  type: 'string',
                  description: 'Path to the data file',
                },
                entityName: {
                  type: 'string',
                  description: 'Name to give the entity',
                },
              },
              required: ['filepath', 'entityName'],
            },
          },
          {
            name: 'validate_entity',
            description: 'Validate an entity that has been loaded into R',
            inputSchema: {
              type: 'object',
              properties: {
                entityVarName: {
                  type: 'string',
                  description: 'Name of the entity variable in R',
                },
              },
              required: ['entityVarName'],
            },
          },
          {
            name: 'create_study',
            description: 'Create a study from multiple validated entities',
            inputSchema: {
              type: 'object',
              properties: {
                entityNames: {
                  type: 'array',
                  items: { type: 'string' },
                  description: 'Names of entity variables in R',
                },
                studyName: {
                  type: 'string',
                  description: 'Name for the study',
                },
              },
              required: ['entityNames', 'studyName'],
            },
          },
          {
            name: 'export_stf',
            description: 'Export a study to Study Transfer Format (STF)',
            inputSchema: {
              type: 'object',
              properties: {
                studyVarName: {
                  type: 'string',
                  description: 'Name of the study variable in R',
                },
                outputDir: {
                  type: 'string',
                  description: 'Output directory for STF files',
                },
              },
              required: ['studyVarName', 'outputDir'],
            },
          },
        ],
      };
    });

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'start_wrangling_session':
            return await this.handleStartSession(args as any);

          case 'reload_study_wrangler':
            return await this.handleReloadStudyWrangler();

          case 'execute_r_code':
            return await this.handleExecuteR(args as any);

          case 'inspect_file':
            return await this.handleInspectFile(args as any);

          case 'validate_entity':
            return await this.handleValidateEntity(args as any);

          case 'create_study':
            return await this.handleCreateStudy(args as any);

          case 'export_stf':
            return await this.handleExportSTF(args as any);

          default:
            throw new Error(`Unknown tool: ${name}`);
        }
      } catch (error) {
        return {
          content: [
            {
              type: 'text',
              text: `Error: ${(error as Error).message}`,
            },
          ],
        };
      }
    });
  }

  private async handleStartSession(args: { directory?: string; devMode?: boolean }) {
    const directory = args.directory || process.cwd();
    
    await this.containerManager.startSession(directory, { devMode: args.devMode });
    this.rserveClient = new RServeClient();

    return {
      content: [
        {
          type: 'text',
          text: `✓ RServe session started in ${args.devMode ? 'development' : 'production'} mode\nWorking directory: ${directory}`,
        },
      ],
    };
  }

  private async handleReloadStudyWrangler() {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.containerManager.reloadStudyWrangler(this.rserveClient);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `✓ study.wrangler reloaded successfully\n${result.message}`
            : `✗ Failed to reload study.wrangler\n${result.message}`,
        },
      ],
    };
  }

  private async handleExecuteR(args: { code: string }) {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.rserveClient.executeR(args.code);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `R Output:\n${result.output}`
            : `R Error:\n${result.error}`,
        },
      ],
    };
  }

  private async handleInspectFile(args: { filepath: string; entityName: string }) {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.rserveClient.inspectFile(args.filepath, args.entityName);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `File inspection for ${args.filepath}:\n\n${result.output}`
            : `Error inspecting file:\n${result.error}`,
        },
      ],
    };
  }

  private async handleValidateEntity(args: { entityVarName: string }) {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.rserveClient.validateEntity(args.entityVarName);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `Entity validation for ${args.entityVarName}:\n\n${result.output}`
            : `Error validating entity:\n${result.error}`,
        },
      ],
    };
  }

  private async handleCreateStudy(args: { entityNames: string[]; studyName: string }) {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.rserveClient.createStudy(args.entityNames, args.studyName);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `Study created successfully:\n\n${result.output}`
            : `Error creating study:\n${result.error}`,
        },
      ],
    };
  }

  private async handleExportSTF(args: { studyVarName: string; outputDir: string }) {
    if (!this.rserveClient) {
      throw new Error('No active RServe session. Start a session first.');
    }

    const result = await this.rserveClient.exportSTF(args.studyVarName, args.outputDir);
    
    return {
      content: [
        {
          type: 'text',
          text: result.success 
            ? `STF export completed:\n\n${result.output}`
            : `Error exporting STF:\n${result.error}`,
        },
      ],
    };
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error('Bio-wrangler MCP server running on stdio');
  }
}

// Start the server
if (require.main === module) {
  const server = new BioWranglerMCPServer();
  server.run().catch(console.error);
}