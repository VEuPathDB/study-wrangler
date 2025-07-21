# Agentic Data Wrangler Architecture - Stepwise Approach

## Overview

An AI-powered tool that uses Claude to incrementally build R scripts using the `study.wrangler` package. The agent works step-by-step through the wrangler workflow, parsing text feedback to make informed decisions about the next action.

## High-Level Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                    Next.js App (Port 3000)                     │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Frontend (React Components)                            │   │
│  │  - File Upload UI                                       │   │
│  │  - Step Progress Tracker                                │   │
│  │  - Current State Display                                │   │
│  │  - R Code Viewer                                        │   │
│  │  - Wrangler Output Console                              │   │
│  └─────────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  API Routes (/app/api/*)                                │   │
│  │  - /api/jobs/create                                     │   │
│  │  - /api/jobs/[id]/status                                │   │
│  │  - /api/jobs/[id]/steps                                 │   │
│  └─────────────────────────────────────────────────────────┘   │
└────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌────────────────────────────────────────────────────────────────┐
│           Step Orchestrator Service (Port 4000)                │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Core Components:                                       │   │
│  │  - Workflow Engine (manages step progression)           │   │
│  │  - Claude Service (generates next steps)                │   │
│  │  - Rserve Client (executes R code)                      │   │
│  │  - WebSocket Server (real-time updates)                 │   │
│  └─────────────────────────────────────────────────────────┘   │
└────────────────────────────────────────────────────────────────┘
         │                              │
         ▼                              ▼
┌─────────────────────┐       ┌──────────────────────────┐
│   Redis (Port 6379) │       │  R Container (Port 6311) │
│   - Job State       │       │  - Rserve                │
│   - Step History    │       │  - study.wrangler        │
└─────────────────────┘       └──────────────────────────┘
```

## Wrangler Workflow Implementation

### Phase 1: Data Understanding
```
User uploads files + instructions
    ↓
For each file: entity_from_file() -> inspect() -> capture text output
    ↓
Send all inspect() outputs + user instructions to Claude
    ↓
Claude analyzes structure, relationships, and creates entity mapping plan
    ↓
Claude returns: entity names, file mappings, parent-child relationships, processing order
```

### Phase 2: Entity-by-Entity Processing
```
For each entity:
    ↓
Load file into entity object
    ↓
Inspect entity → capture text output
    ↓
Claude interprets issues
    ↓
Generate wrangling code
    ↓
Validate entity → capture text output
    ↓
Repeat until entity validates
```

### Phase 3: Study Creation & Validation
```
Create study from validated entities
    ↓
Define relationships
    ↓
Validate study → capture text output
    ↓
Claude interprets validation results
    ↓
Fix any cross-entity issues
    ↓
Export to STF format
```

## Project Structure (Preserves R Package Layout)

```
study-wrangler/                   # Root preserves R package structure
├── R/                           # EXISTING R package source
├── man/                         # EXISTING R documentation  
├── tests/                       # EXISTING R tests
├── DESCRIPTION                  # EXISTING R package metadata
├── NAMESPACE                    # EXISTING R exports
├── inst/                        # EXISTING R data/examples
│
├── apps/                        # NEW: Web applications
│   ├── web/                     # Next.js application (port 3000)
│   │   ├── app/
│   │   │   ├── layout.tsx
│   │   │   ├── page.tsx
│   │   │   ├── api/
│   │   │   │   ├── jobs/
│   │   │   │   │   ├── create/route.ts
│   │   │   │   │   └── [id]/
│   │   │   │   │       ├── status/route.ts
│   │   │   │   │       └── steps/route.ts
│   │   │   └── components/
│   │   │       ├── FileUploader.tsx
│   │   │       ├── WorkflowProgress.tsx
│   │   │       ├── StepDisplay.tsx
│   │   │       ├── CodeViewer.tsx
│   │   │       └── WranglerOutput.tsx
│   │   ├── lib/
│   │   │   ├── hooks/
│   │   │   │   └── useJobProgress.ts
│   │   │   └── api-client.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   └── orchestrator/            # Node.js orchestrator service (port 4000)
│       ├── src/
│       │   ├── server.ts
│       │   ├── services/
│       │   │   ├── claude.service.ts
│       │   │   ├── rserve.service.ts
│       │   │   └── workflow.service.ts
│       │   ├── workflows/
│       │   │   ├── data-understanding.workflow.ts
│       │   │   ├── entity-wrangling.workflow.ts
│       │   │   └── study-creation.workflow.ts
│       │   ├── prompts/
│       │   │   └── stepwise-wrangler.prompts.ts
│       │   └── types/
│       │       └── index.ts
│       ├── package.json
│       └── tsconfig.json
│
├── packages/                    # NEW: Shared code
│   └── shared/                  # Shared TypeScript types & utilities
│       ├── src/
│       │   └── types.ts         # WranglerStep, StepResult, JobUpdate interfaces
│       ├── package.json
│       └── tsconfig.json
│
├── docker/                      # NEW: Multi-service Docker setup
│   ├── docker-compose.yml       # Web + Orchestrator + R + Redis
│   └── r-service/              
│       └── Dockerfile          # Existing R + Rserve container
│
├── package.json                 # NEW: Root yarn workspace config
├── yarn.lock                    # NEW: Yarn lockfile
├── nx.json                      # NEW: Nx build orchestration
└── CLAUDE.md                    # This architecture document
```

**Key benefits of this structure:**
- `remotes::install_github('VEuPathDB/study-wrangler')` continues to work unchanged
- R package development workflow preserved
- Web app and services isolated in `apps/` directory
- Shared code prevents duplication between frontend/backend
- Docker compose coordinates all services for development
- Improvements to the `study.wrangler` R package can be made as needed
  (and easily) during development of the agentic tool.

## Actual study.wrangler API Patterns (Updated)

### Core Entity Workflow
Based on examination of the actual package, the typical entity wrangling workflow is:

```r
# 1. Load entity from file
entity <- entity_from_file("data.tsv", name = "entity_name")

# 2. Inspect to see current state and issues
inspect(entity)

# 3. Common fixes based on inspection output:
# - For multivalued columns (comma-separated values):
entity <- entity %>% set_variables_multivalued('column_name' = ',')

# - For parent-child relationships:
entity <- entity %>% set_parents(
  names = c("parent_entity"),
  id_columns = c("parent_id_column")
)

# - For incorrect ID column detection:
entity <- entity %>% redetect_columns_as_variables(c('wrongly_detected_id'))
entity <- entity %>% redetect_column_as_id('should_be_id_column')

# - For metadata sync issues:
entity <- entity %>% sync_variable_metadata()

# 4. Validate entity
is_valid <- validate(entity)

# 5. Create study from multiple entities
study <- study_from_entities(list(entity1, entity2), name = "study_name")

# 6. Validate the complete study
validate(study)

# 7. Export to STF format
export_to_stf(study, "output_directory")
```

### Key Data Structures

**Entity Class:**
- `@data`: tibble with actual data
- `@variables`: tibble with extensive metadata including data_type, data_shape, display_name, etc.
- `@name`, `@description`, `@display_name`: entity metadata
- `@children`: list of child entities for hierarchical relationships

**Variable Metadata Fields (key ones):**
- `data_type`: factor("id", "string", "number", "date", "longitude", "integer", "category")
- `data_shape`: factor("continuous", "categorical", "ordinal", "binary")  
- `provider_label`: original column name from file
- `is_multi_valued`: boolean for delimited columns
- `entity_name`, `entity_level`: for hierarchical relationships

### Text Output Patterns
From examining inspect() and validate() methods, the system outputs structured text like:

```
Entity-level metadata
Row counts
ID columns  
Summary of important metadata for all variables and categories
Variable annotation summary
```

Validation outputs success/failure with specific error messages and fix suggestions.

## WebSocket Real-time Architecture

### User Flow After "Auto-Wrangle" Button

1. **Job Creation**: `POST /api/jobs/create` returns `jobId` immediately
2. **WebSocket Connection**: Frontend connects to `ws://localhost:4000/jobs/{jobId}`  
3. **Real-time Updates**: Orchestrator broadcasts step progress via WebSocket

### WebSocket Message Types
```typescript
export type JobUpdate = 
  | { type: 'step_started'; step: WranglerStep }
  | { type: 'step_completed'; step: WranglerStep; result: StepResult; progress: Progress }
  | { type: 'entity_completed'; entityName: string; validated: boolean }
  | { type: 'phase_changed'; phase: WorkflowPhase; message: string }
  | { type: 'job_completed'; script: string; success: boolean }
  | { type: 'job_failed'; error: string; partialScript?: string };
```

### Frontend Integration
```typescript
// useJobProgress hook connects WebSocket and manages state
const { status, steps, currentStep, phase } = useJobProgress(jobId);

// Components update in real-time based on WebSocket messages
<WorkflowProgress phase={phase} currentStep={currentStep} />
<StepDisplay steps={steps} />
<CodeViewer code={currentStep?.code} />
<WranglerOutput output={currentStep?.result?.output} />
```

### Orchestrator Broadcasting
```typescript
// Workflow service broadcasts at key moments:
- Before step execution: 'step_started'  
- After step execution: 'step_completed' with R output
- Entity validation: 'entity_completed' 
- Phase transitions: 'phase_changed'
- Job completion: 'job_completed' with full script
```

**Benefits over polling:**
- Instant feedback (no 2-second delay)
- Efficient (only sends when state changes)
- Rich message types for different UI states
- Multiple browser tabs can connect to same job

## Key Implementation Details

### Step Definition

```typescript
interface WranglerStep {
  id: string;
  phase: 'understanding' | 'entity_wrangling' | 'study_creation';
  entityName?: string;  // Which entity this step is working on
  purpose: string;
  code: string;
  attempt: number;
  isFinalAttempt?: boolean;  // Mark the step that achieved validation
}

interface StepResult {
  stepId: string;
  success: boolean;
  output: string;  // Raw text from R console
  error?: string;
  duration: number;
}

interface EntityMapping {
  name: string;                    // Entity name (e.g., "household")
  filename: string;                // Source file (e.g., "households.tsv")
  likely_id_columns: string[];     // Detected ID columns
  parent?: string;                 // Parent entity name (null for root)
  parent_id_column?: string;       // Column that links to parent
}

interface WorkflowState {
  jobId: string;
  phase: 'understanding' | 'entity_wrangling' | 'study_creation';
  currentEntity?: string;
  completedEntities: string[];
  steps: Array<{ step: WranglerStep; result: StepResult }>;
  entityValidationStatus: Record<string, boolean>;
  studyValidated: boolean;
  entityMapping?: EntityMapping[];      // Plan from Phase 1
  processingOrder?: string[];           // Entity processing order
}
```

### Workflow Service

The main orchestration logic:

```typescript
class WorkflowService {
  constructor(
    private claude: ClaudeService,
    private rserve: RserveService
  ) {}
  
  async executeJob(job: WranglerJob): Promise<WranglerResult> {
    const state: WorkflowState = {
      jobId: job.id,
      phase: 'understanding',
      completedEntities: [],
      steps: [],
      entityValidationStatus: {},
      studyValidated: false
    };
    
    try {
      // Phase 1: Data Understanding
      await this.executeDataUnderstanding(job, state);
      
      // Phase 2: Entity-by-Entity Wrangling
      const entities = await this.identifyEntities(state);
      for (const entity of entities) {
        await this.wrangleEntity(entity, job, state);
      }
      
      // Phase 3: Study Creation
      await this.createAndValidateStudy(job, state);
      
      return {
        success: true,
        script: this.assembleFullScript(state.steps),
        steps: state.steps
      };
      
    } catch (error) {
      return {
        success: false,
        error: error.message,
        steps: state.steps
      };
    }
  }
  
  private async executeDataUnderstanding(job: WranglerJob, state: WorkflowState) {
    // For each uploaded file, create entity and capture inspect() output
    const fileAnalyses = [];
    
    for (const file of job.files) {
      // Hardcode the simple analysis step - no need for Claude to generate this
      const analysisStep: WranglerStep = {
        id: uuidv4(),
        phase: 'understanding',
        purpose: `Analyze file ${file.name}`,
        code: `entity_from_file("${file.name}") %>% inspect()`,
        attempt: 1
      };
      
      const result = await this.executeStep(analysisStep, state);
      
      fileAnalyses.push({
        filename: file.name,
        inspectOutput: result.output
      });
    }
    
    // Send all inspect outputs + instructions to Claude for entity planning
    const entityPlan = await this.claude.planEntityMappings(
      fileAnalyses,
      job.instructions
    );
    
    state.entityMapping = entityPlan.entities;
    state.processingOrder = entityPlan.processing_order;
  }
  
  private async wrangleEntity(
    entityName: string, 
    job: WranglerJob, 
    state: WorkflowState
  ) {
    state.phase = 'entity_wrangling';
    state.currentEntity = entityName;
    
    let validated = false;
    let attempts = 0;
    const maxAttempts = 5;
    
    while (!validated && attempts < maxAttempts) {
      attempts++;
      
      // Generate wrangling step
      const step = await this.claude.generateEntityWranglingStep(
        entityName,
        this.getEntityContext(state, entityName),
        job.instructions,
        state.entityMapping,
        attempts
      );
      
      // Execute step
      const result = await this.executeStep(step, state);
      
      // Check if entity validation succeeded
      if (result.success && result.output.includes('Entity is valid')) {
        validated = true;
        step.isFinalAttempt = true;
      }
      
      // Store step in audit trail after marking final attempt
      state.steps.push({ step, result });
      
      state.entityValidationStatus[entityName] = validated;
    }
    
    if (validated) {
      state.completedEntities.push(entityName);
    } else {
      throw new Error(`Failed to validate entity ${entityName} after ${attempts} attempts`);
    }
  }
  
  private async executeStep(step: WranglerStep, state: WorkflowState): Promise<StepResult> {
    const startTime = Date.now();
    
    try {
      // Execute R code and capture all output
      const output = await this.rserve.executeAndCapture(state.jobId, step.code);
      
      const result: StepResult = {
        stepId: step.id,
        success: !output.error,
        output: output.stdout,
        error: output.error,
        duration: Date.now() - startTime
      };
      
      // Send real-time update (steps stored by caller)
      await this.sendProgressUpdate(state);
      
      return result;
      
    } catch (error) {
      const result: StepResult = {
        stepId: step.id,
        success: false,
        output: '',
        error: error.message,
        duration: Date.now() - startTime
      };
      
      return result;
    }
  }
  
  private getEntityContext(state: WorkflowState, entityName: string): string {
    // Build context from previous steps for this entity
    const entitySteps = state.steps.filter(
      s => s.step.entityName === entityName
    );
    
    if (entitySteps.length === 0) {
      return "No previous attempts for this entity.";
    }
    
    const lastStep = entitySteps[entitySteps.length - 1];
    return `
Previous attempt output:
${lastStep.result.output}

${lastStep.result.error ? `Error: ${lastStep.result.error}` : ''}
    `;
  }

  private assembleFullScript(steps: Array<{ step: WranglerStep; result: StepResult }>): string {
    // Only include final successful attempts for each entity, plus all non-entity steps
    const finalSteps = steps.filter(s => 
      s.step.isFinalAttempt || s.step.phase !== 'entity_wrangling'
    );
    
    return finalSteps
      .map(s => `# ${s.step.purpose}\n${s.step.code}`)
      .join('\n\n');
  }
}
```

### Claude Service for Stepwise Generation

```typescript
class ClaudeService {
  async planEntityMappings(
    fileAnalyses: Array<{filename: string, inspectOutput: string}>,
    instructions: string
  ): Promise<{entities: EntityMapping[], processing_order: string[]}> {
    const prompt = `
Based on these file analyses, create a plan for entity creation and relationships.

User instructions: ${instructions}

File analyses:
${fileAnalyses.map(f => `
=== ${f.filename} ===
${f.inspectOutput}
`).join('\n')}

Create a JSON response with:
1. "entities": Array of entities to create with names, file mappings, and relationships
2. "processing_order": Array of entity names in dependency order (parents before children)

Consider:
- Appropriate entity names based on data content
- Parent-child relationships based on ID columns
- Processing order to handle dependencies

Return valid JSON only.
`;

    const response = await this.client.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 2000,
      messages: [{ role: 'user', content: prompt }]
    });
    
    return JSON.parse(this.extractJSONResponse(response.content[0].text));
  }

  async generateEntityWranglingStep(
    entityName: string,
    previousOutput: string,
    userInstructions: string,
    entityMapping: EntityMapping[],
    attempt: number
  ): Promise<WranglerStep> {
    const currentEntity = entityMapping.find(e => e.name === entityName);
    const prompt = `
You are wrangling the "${entityName}" entity using study.wrangler functions.

ORIGINAL USER INSTRUCTIONS: 
${userInstructions}

ENTITY PLAN (from Phase 1):
${JSON.stringify(entityMapping, null, 2)}

CURRENT ENTITY DETAILS:
${JSON.stringify(currentEntity, null, 2)}

PREVIOUS R OUTPUT (attempt ${attempt}):
${previousOutput}

Generate R code to:
${attempt === 1 ? `
1. Load entity: entity_from_file("${currentEntity?.filename}", name="${entityName}")
2. Address issues shown in inspect() output above
3. Apply entity relationships: ${currentEntity?.parent ? 
   `set_parents(names=c("${currentEntity.parent}"), id_columns=c("${currentEntity.parent_id_column}"))` : 
   'none needed (root entity)'}
4. Run validate(entity) to check success
` : `
Fix the validation issues from the previous attempt.
Focus on the specific errors/warnings in the output above.
Follow any fix suggestions provided by the wrangler output.
After fixes, run validate(entity) again.
`}

Core functions available:
- entity_from_file(), inspect(), validate()
- set_parents(), set_variables_multivalued(), redetect_columns_as_variables()
- sync_variable_metadata()

Follow specific guidance from wrangler's inspect()/validate() output messages.
`;

    const response = await this.client.messages.create({
      model: 'claude-3-5-sonnet-20241022', 
      max_tokens: 1500,
      messages: [{ role: 'user', content: prompt }]
    });
    
    return {
      id: uuidv4(),
      phase: 'entity_wrangling',
      entityName,
      purpose: `Wrangle ${entityName} entity (attempt ${attempt})`,
      code: this.extractRCode(response.content[0].text),
      attempt
    };
  }
}
```

### Rserve Service for Text Capture

```typescript
class RserveService {
  async executeAndCapture(jobId: string, code: string): Promise<{
    stdout: string;
    error?: string;
  }> {
    try {
      // Wrap code to capture all output
      const wrappedCode = `
# Capture all output
.captureOutput <- textConnection("outputText", "w", local = TRUE)
sink(.captureOutput)
sink(.captureOutput, type = "message")

tryCatch({
  ${code}
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\\n")
}, finally = {
  sink()
  sink(type = "message")
  close(.captureOutput)
})

# Return captured output
paste(outputText, collapse = "\\n")
`;

      const result = await this.client.eval(wrappedCode);
      
      // Parse for errors
      const output = result.toString();
      const hasError = output.includes("ERROR:");
      
      return {
        stdout: output,
        error: hasError ? output.split("ERROR:")[1].trim() : undefined
      };
      
    } catch (error) {
      return {
        stdout: '',
        error: error.message
      };
    }
  }
}
```

## Development Phases

### Phase 1: Core Infrastructure (Week 1)
- Set up yarn workspace + nx monorepo with Next.js and orchestrator
- Implement basic file upload and job creation
- Set up Docker Compose environment with Rserve + Redis
- Create WebSocket server for real-time progress updates

### Phase 2: Stepwise Workflow (Week 2)
- Implement workflow service with three phases
- Create Claude prompts for each phase
- Build text parser for wrangler outputs
- Test with simple single-entity datasets

### Phase 3: Multi-Entity Support (Week 3)
- Enhance entity relationship detection
- Implement cross-entity validation handling
- Test with complex multi-entity datasets
- Refine Claude prompts based on results

### Phase 4: UI Polish (Week 4)
- Create intuitive progress visualization
- Add step-by-step code display
- Show parsed wrangler output in structured format
- Add ability to download final script

## Key Design Decisions

### Why Stepwise Execution?
- Matches natural wrangler workflow
- Provides immediate feedback for corrections
- Reduces wasted computation
- Makes debugging easier
- Better user experience with visible progress

### Why Parse Plain Text?
- Works with existing wrangler package
- No need to modify R code
- Claude is excellent at understanding text output
- Maintains flexibility for future changes

### Why Separate Orchestrator Service?
- Keeps Claude API key secure
- Manages complex workflow state
- Enables future Java migration
- Better separation of concerns

## File Upload Restrictions (Phase 1)

**Supported file types:**
- `.tsv`, `.txt` (tab-delimited)
- `.csv` (comma-delimited)
- Auto-detection via `entity_from_file()` function

**File size limits:**
- Maximum 10MB per file
- Maximum 50MB total upload
- Maximum 10 files per job

**Unsupported (future versions):**
- Excel files (`.xlsx`, `.xls`)
- JSON, XML formats
- Compressed archives
- Binary formats

**Processing approach:**
- Each file → `entity_from_file(filename)` → `inspect(entity)`
- Full inspect() output sent to Claude (high token usage but comprehensive analysis)
- Claude creates entity mapping plan from inspect() text outputs

## Context Passing for Claude API Calls

Since each Claude API call is stateless, every request must include full context:

### Entity Wrangling Step Context
```typescript
async generateEntityWranglingStep(
  entityName: string,
  previousOutput: string,
  userInstructions: string,
  entityMapping: EntityMapping[],
  attempt: number
): Promise<WranglerStep>
```

**Context included in every prompt:**
- **User instructions**: Original upload description/requirements
- **Entity mapping**: Complete plan from Phase 1 (entity relationships, ID columns)
- **Previous R output**: Raw text from last inspect()/validate() call
- **Current entity context**: Which entity, what attempt, expected relationships
- **Basic API functions**: Core wrangler functions (rely on wrangler's built-in guidance for specifics)

### System Prompt Template
```typescript
const systemPrompt = `
You generate study.wrangler R code. Available core functions:
- entity_from_file(), inspect(), validate()
- set_parents(), set_variables_multivalued(), redetect_columns_as_variables()
- sync_variable_metadata(), study_from_entities(), export_to_stf()

Follow the specific fix suggestions in wrangler's inspect()/validate() output.
`;
```

## Security & Prompt Injection Protection

### Input Sanitization
```typescript
function sanitizeUserInstructions(instructions: string): string {
  return instructions
    .replace(/[a-zA-Z_][a-zA-Z0-9_]*\s*\(/g, '') // Remove function calls
    .replace(/[<>{}]/g, '')                       // Remove dangerous characters  
    .replace(/system\s*\(/gi, '')                // Remove system calls
    .slice(0, 2000);                             // Limit length
}
```

### Generated Code Validation
```typescript
function validateGeneratedCode(code: string): boolean {
  const allowedFunctions = [
    // study.wrangler functions
    'entity_from_file', 'inspect', 'validate', 'set_parents',
    'set_variables_multivalued', 'set_variables_univalued', 
    'redetect_columns_as_variables', 'redetect_column_as_id',
    'sync_variable_metadata', 'study_from_entities', 'export_to_stf',
    // R basics
    'library', 'print', 'cat', 'head', 'str', 'summary', 'names'
  ];
  
  const functionCalls = code.match(/[a-zA-Z_][a-zA-Z0-9_]*(?=\s*\()/g) || [];
  const hasSystemCalls = /system\s*\(|eval\s*\(|source\s*\(/i.test(code);
  
  return !hasSystemCalls && 
         functionCalls.every(fn => allowedFunctions.includes(fn));
}
```

### R Environment Sandboxing
- **Containerized execution**: Docker container with no network access
- **File system limits**: Read-only except for temp upload directory  
- **Resource limits**: 2GB RAM, 30-second timeout per step
- **Function blacklist**: No system(), eval(), source() calls allowed
- **Session isolation**: Each job gets fresh R session

### Error Handling
- All generated R code validated before execution
- Malicious code detection triggers job failure with sanitized error message
- Failed validation attempts logged for monitoring

## Performance & Infrastructure

- R sessions isolated per job
- Temporary files cleaned up after processing
- Rate limiting on API endpoints  
- Maximum file size limits enforced
- Timeout protection for long-running operations
- File type validation on upload

## Testing Considerations

• **Mock Claude API calls** - Create deterministic test fixtures for Claude responses (entity mappings, R code generation) to avoid API costs and flaky tests during development

• **R code execution sandbox** - Test generated R code against known datasets with expected outcomes; validate that malicious code detection works properly  

• **End-to-end workflow tests** - Full pipeline tests from file upload → entity analysis → wrangling → study creation → STF export using sample TSV/CSV files

• **WebSocket integration tests** - Verify real-time progress updates are sent correctly at each phase; test multiple client connections and job state synchronization

• **Error handling and retry logic** - Test failure scenarios: malformed files, validation failures, Claude API timeouts, R execution errors; ensure proper error messages and graceful degradation

**Key testing infrastructure:**
- `tests/fixtures/` directory with sample datasets (both valid and problematic)
- Lightweight R test environment that can run actual study.wrangler functions
- Comprehensive mocking for Claude API interactions
- Test containers for isolated R execution environment

## Future Enhancements

1. **Learning from Success**: Store successful wrangling patterns
2. **Parallel Entity Processing**: Process independent entities simultaneously  
3. **Advanced File Handling**: Support for 1-to-many file-to-entity mappings
4. **Custom Wrangler Functions**: Extend API based on common needs
5. **Export Options**: Multiple output formats beyond STF

