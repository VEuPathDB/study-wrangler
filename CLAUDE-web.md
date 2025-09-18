# ClinEpiDB/VEuPathDB Web Integration Architecture

## Overview

Integration of AI-assisted data wrangling directly into ClinEpiDB.org and VEuPathDB.org upload workflows. Provides a chat-based interface for guiding researchers through data cleaning and validation before database submission.

## Integration Points

### Current VEuPathDB Upload Flow
```
Researcher logs in → Navigate to data submission → Upload files → Manual form completion → Submit for review
```

### Enhanced AI-Assisted Flow  
```
Researcher logs in → Navigate to data submission → Upload files → 
AI Chat: "I see you've uploaded 3 files. Can you describe your study design?" →
Iterative conversation to understand data structure →
AI generates and executes data wrangling steps →
Final validation and STF export → 
Auto-populated submission forms → Submit for review
```

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                 ClinEpiDB/VEuPathDB React App               │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Data Upload Page (Enhanced)                        │   │
│  │  ┌─────────────────┐  ┌─────────────────────────┐   │   │
│  │  │   File Upload   │  │    AI Chat Widget       │   │   │
│  │  │   Component     │  │  - Chat interface       │   │   │
│  │  │   (existing)    │  │  - Progress display     │   │   │
│  │  │                 │  │  - Generated R code     │   │   │
│  │  └─────────────────┘  │  - Validation results   │   │   │
│  │                       └─────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
│  API Routes:                                                │
│  - POST /api/wrangling/sessions/create                      │
│  - GET/WS /api/wrangling/sessions/{id}/chat                 │
│  - POST /api/wrangling/sessions/{id}/files                  │
│  - GET /api/wrangling/sessions/{id}/status                  │
└─────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────┐
│                 AI Wrangling Service                        │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              Queue Manager                          │   │
│  │  - Session prioritization                           │   │
│  │  - Resource allocation                              │   │
│  │  - Wait time estimation                             │   │
│  │  - User notifications                               │   │
│  └─────────────────────────────────────────────────────┘   │
│                                │                            │
│                                ▼                            │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           R Container Pool                          │   │
│  │                                                     │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │   │
│  │  │ R Instance  │  │ R Instance  │  │ R Instance  │ │   │
│  │  │ + RServe    │  │ + RServe    │  │ + RServe    │ │   │
│  │  │ Port 6311   │  │ Port 6312   │  │ Port 6313   │ │   │
│  │  └─────────────┘  └─────────────┘  └─────────────┘ │   │
│  │                                                     │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────┐
│                    Session Storage                          │
│                                                             │
│  ┌─────────────────┐              ┌─────────────────────┐   │
│  │     Redis       │              │   File Storage      │   │
│  │  - Session      │              │  - Uploaded files   │   │
│  │    metadata     │              │  - Generated R      │   │
│  │  - Chat history │              │    scripts          │   │
│  │  - Queue state  │              │  - STF output       │   │
│  └─────────────────┘              └─────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Scalability Strategy

### Resource Management Options

#### Option A: Small R Instance Pool (Recommended for Low Traffic)
```yaml
Configuration:
  r_instances: 3
  max_memory_per_instance: 4GB
  max_concurrent_sessions: 3
  queue_capacity: 50
  estimated_cost: ~$200/month
```

**Benefits:**
- Predictable costs
- Simple management
- Adequate for current traffic levels

**Limitations:**
- Users may wait during peak times
- Limited to small-medium datasets

#### Option B: Auto-Scaling Pool
```yaml
Configuration:
  min_instances: 2
  max_instances: 10
  scale_trigger: queue_length > 5
  memory_per_instance: 8GB
  estimated_cost: $150-800/month (variable)
```

**Benefits:**
- Handles traffic spikes
- Better user experience
- Supports larger datasets

**Limitations:**
- Variable costs
- Complex orchestration
- Cold start delays

#### Option C: Hybrid Approach (Recommended)
```yaml
Configuration:
  persistent_instances: 2
  burst_instances: 0-5 (on-demand)
  queue_with_notification: true
  max_wait_time: 10_minutes
  estimated_cost: $120-400/month
```

**Benefits:**
- Cost-effective baseline
- Handles moderate spikes
- Clear user expectations

## Queue Management & User Experience

### Queue States
```typescript
interface WranglingSession {
  id: string;
  user_id: string;
  status: 'queued' | 'active' | 'completed' | 'failed';
  estimated_wait: number; // minutes
  position_in_queue: number;
  files: UploadedFile[];
  chat_history: ChatMessage[];
  r_instance_id?: string;
  created_at: Date;
  started_at?: Date;
  completed_at?: Date;
}
```

### User Communication Strategy
```typescript
// When all R instances busy
const queueNotification = {
  type: 'queue_position',
  message: `You're #${position} in line. Estimated wait: ${wait_time} minutes.`,
  actions: [
    'Get notified when ready',
    'Continue later (we\'ll save your files)',
    'Upload smaller files first'
  ]
};

// Real-time updates
const statusUpdates = [
  'Moving up in queue... now #3',
  'Starting your wrangling session...',
  'AI is analyzing your first file...',
  'Entity validation complete. Moving to next file...'
];
```

## File Size & Memory Constraints

### Tiered File Handling
```typescript
interface FileSizePolicy {
  small: {
    max_size: '10MB';
    max_files: 10;
    processing: 'immediate';
    memory_limit: '2GB';
  };
  medium: {
    max_size: '50MB';
    max_files: 5;
    processing: 'queued_priority';
    memory_limit: '4GB';
  };
  large: {
    max_size: '200MB';
    max_files: 3;
    processing: 'queued_batch';
    memory_limit: '8GB';
    requires_confirmation: true;
  };
}
```

### Memory Management
- **Pre-processing validation**: Check file sizes before R session allocation
- **Progressive loading**: Load one entity at a time, not all files simultaneously
- **Memory monitoring**: Track R session memory usage, auto-restart if needed
- **Graceful degradation**: Offer manual upload option for oversized datasets

## Integration with Existing VEuPathDB Infrastructure

### Authentication & Authorization
```typescript
// Leverage existing VEuPathDB auth
interface UserSession {
  veupathdb_user_id: string;
  authentication_token: string;
  data_submission_permissions: string[];
  organization: string;
}

// Session creation endpoint
POST /api/wrangling/sessions/create
Headers: {
  'Authorization': 'Bearer ${veupathdb_token}',
  'X-User-ID': '${user_id}'
}
```

### File Storage Integration
```typescript
// Use existing VEuPathDB file management
interface FileIntegration {
  upload_endpoint: '/api/files/upload';
  storage_backend: 'existing_s3_bucket';
  retention_policy: '30_days_temp_processing';
  final_storage: 'user_submission_area';
}
```

### Submission Workflow Integration
```typescript
// Enhanced submission flow
interface SubmissionIntegration {
  pre_wrangling: 'upload_raw_files';
  during_wrangling: 'chat_assisted_validation';
  post_wrangling: {
    auto_populate_forms: true;
    attach_r_scripts: true;
    attach_stf_files: true;
    validation_summary: true;
  };
}
```

## Technical Implementation

### Backend Services
```yaml
ai_wrangling_service:
  language: 'Node.js/TypeScript'
  framework: 'Express + Socket.io'
  deployment: 'Docker containers'
  scaling: 'Horizontal (service replicas)'

r_container_pool:
  base_image: 'rocker/r-ver:4.3.0'
  additional_packages: ['study.wrangler', 'Rserve']
  orchestration: 'Docker Swarm or Kubernetes'
  networking: 'Internal service mesh'

queue_management:
  backend: 'Redis'
  features: ['Priority queues', 'TTL', 'Pub/sub']
  monitoring: 'Queue length, wait times, throughput'
```

### Frontend Integration
```typescript
// React component integration
const DataUploadWithAI: React.FC = () => {
  const [files, setFiles] = useState<File[]>([]);
  const [session, setSession] = useState<WranglingSession | null>(null);
  const [chatMessages, setChatMessages] = useState<ChatMessage[]>([]);
  
  // Existing file upload logic
  const handleFileUpload = async (uploadedFiles: File[]) => {
    setFiles(uploadedFiles);
    
    // Create AI wrangling session
    const response = await fetch('/api/wrangling/sessions/create', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${authToken}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        files: uploadedFiles.map(f => ({ name: f.name, size: f.size }))
      })
    });
    
    const newSession = await response.json();
    setSession(newSession);
    
    // Connect to WebSocket for real-time updates
    const ws = new WebSocket(`/api/wrangling/sessions/${newSession.id}/chat`);
    ws.onmessage = (event) => {
      const message = JSON.parse(event.data);
      setChatMessages(prev => [...prev, message]);
    };
  };
  
  return (
    <div className="data-upload-enhanced">
      <FileUploadComponent onUpload={handleFileUpload} />
      {session && (
        <AIChat 
          session={session} 
          messages={chatMessages}
          onUserMessage={sendChatMessage}
        />
      )}
    </div>
  );
};
```

## Deployment & Operations

### Infrastructure Requirements
```yaml
compute:
  ai_service: '2 CPU, 4GB RAM per replica'
  r_instances: '2 CPU, 4-8GB RAM per instance'
  redis: '1 CPU, 2GB RAM'
  file_storage: '100GB initial, auto-scaling'

monitoring:
  metrics: ['Queue length', 'Processing time', 'Success rate', 'Resource usage']
  alerts: ['Queue backup', 'R instance failures', 'High memory usage']
  logging: ['User sessions', 'AI interactions', 'Error tracking']
```

### Cost Analysis (Monthly Estimates)
```yaml
option_small_pool:
  r_instances: '$150 (3 x 4GB instances)'
  ai_service: '$50 (2 replicas)'
  storage: '$20 (temp files)'
  claude_api: '$200-500 (usage based)'
  total: '$420-720/month'

option_hybrid:
  base_instances: '$100 (2 persistent)'
  burst_capacity: '$0-200 (on-demand)'
  ai_service: '$50'
  storage: '$20'
  claude_api: '$200-500'
  total: '$370-870/month'
```

## Risk Mitigation

### Technical Risks
- **R instance crashes**: Auto-restart, session recovery from Redis
- **Memory exhaustion**: Monitoring, automatic limits, graceful degradation
- **Queue backup**: User notifications, priority handling, alternative workflows
- **Claude API limits**: Rate limiting, fallback to manual upload

### User Experience Risks
- **Long wait times**: Clear communication, alternative options, async notifications
- **Complex datasets**: Fallback to expert human review, partial automation
- **Integration friction**: Thorough testing, gradual rollout, user feedback loops

## Discussion Points for Planning

1. **R Instance Sizing**: Start with 3 x 4GB instances or 2 x 8GB instances?

2. **Queue Strategy**: Hard limits with user rejection vs. unlimited queue with long waits?

3. **File Size Limits**: Conservative (10MB) vs. aggressive (100MB) initial limits?

4. **Fallback Mechanisms**: What happens when AI fails? Manual review process?

5. **Beta Testing**: Which researcher groups for initial testing? How to gather feedback?

6. **Integration Depth**: Chat widget overlay vs. full page integration vs. separate page?

These architectural decisions will determine both the user experience and operational complexity. The hybrid approach with modest resource pools seems most appropriate for the initial deployment, with clear upgrade paths as usage grows.