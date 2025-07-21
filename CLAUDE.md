# Agentic Data Wrangler Architecture - Stepwise Approach

## Overview

An AI-powered tool that uses Claude to incrementally build R scripts using the `study.wrangler` package. The agent works step-by-step through the wrangler workflow, parsing text feedback to make informed decisions about the next action.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Next.js App (Port 3000)                      │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Frontend (React Components)                             │   │
│  │  - File Upload UI                                        │   │
│  │  - Step Progress Tracker                                │   │
│  │  - Current State Display                               │   │
│  │  - R Code Viewer                                       │   │
│  │  - Wrangler Output Console                             │   │
│  └─────────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  API Routes (/app/api/*)                                │   │
│  │  - /api/jobs/create                                     │   │
│  │  - /api/jobs/[id]/status                               │   │
│  │  - /api/jobs/[id]/steps                                │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│           Step Orchestrator Service (Port 4000)                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Core Components:                                        │   │
│  │  - Workflow Engine (manages step progression)           │   │
│  │  - Claude Service (generates next steps)                │   │
│  │  - Rserve Client (executes R code)                     │   │
│  │  - Text Parser (extracts info from wrangler output)    │   │
│  │  - WebSocket Server (real-time updates)                │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
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
Claude analyzes file structure and relationships
    ↓
Generate initial exploration code
    ↓
Execute and capture output
    ↓
Claude interprets results and plans entity mapping
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

## Project Structure

```
agentic-data-wrangler/
├── apps/
│   ├── web/                      # Next.js application
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
│   └── orchestrator/             # Node.js orchestrator service
│       ├── src/
│       │   ├── server.ts
│       │   ├── services/
│       │   │   ├── claude.service.ts
│       │   │   ├── rserve.service.ts
│       │   │   ├── workflow.service.ts
│       │   │   └── text-parser.service.ts
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
├── packages/
│   └── shared/                   # Shared types & utilities
│       ├── src/
│       │   ├── types.ts
│       │   └── text-patterns.ts
│       └── package.json
│
├── docker/
│   ├── r-wrangler/
│   │   └── Dockerfile           # Your existing Dockerfile
│   └── docker-compose.yml
│
├── package.json                  # Root package.json
├── pnpm-workspace.yaml          # PNPM workspace config
└── turbo.json                   # Turborepo config
```

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
}

interface StepResult {
  stepId: string;
  success: boolean;
  output: string;  // Raw text from R console
  error?: string;
  duration: number;
}

interface WorkflowState {
  jobId: string;
  phase: 'understanding' | 'entity_wrangling' | 'study_creation';
  currentEntity?: string;
  completedEntities: string[];
  steps: Array<{ step: WranglerStep; result: StepResult }>;
  entityValidationStatus: Record<string, boolean>;
  studyValidated: boolean;
}
```

### Text Parser Service

Since wrangler outputs plain text, we need to parse it intelligently:

```typescript
class TextParserService {
  // Parse entity inspection output
  parseEntityInspection(output: string): EntityInspectionResult {
    // Extract key information using regex patterns
    const rowCount = this.extractPattern(output, /Rows:\s*(\d+)/);
    const columns = this.extractList(output, /Columns:\s*(.+)/);
    const idColumns = this.extractList(output, /ID columns:\s*(.+)/);
    const issues = this.extractSection(output, /Issues:/, /^$/m);
    
    return { rowCount, columns, idColumns, issues };
  }
  
  // Parse validation output
  parseValidationResult(output: string): ValidationResult {
    const isValid = output.includes('Validation passed') || 
                   output.includes('Entity is valid');
    const errors = this.extractSection(output, /Errors:/, /^$/m);
    const warnings = this.extractSection(output, /Warnings:/, /^$/m);
    
    return { isValid, errors, warnings };
  }
  
  // Extract patterns from text
  private extractPattern(text: string, pattern: RegExp): string | null {
    const match = text.match(pattern);
    return match ? match[1] : null;
  }
  
  private extractList(text: string, pattern: RegExp): string[] {
    const match = text.match(pattern);
    if (!match) return [];
    return match[1].split(',').map(s => s.trim());
  }
  
  private extractSection(text: string, startPattern: RegExp, endPattern: RegExp): string[] {
    // Extract lines between patterns
    const lines = text.split('\n');
    const startIdx = lines.findIndex(l => startPattern.test(l));
    if (startIdx === -1) return [];
    
    const endIdx = lines.findIndex((l, i) => i > startIdx && endPattern.test(l));
    const sectionLines = lines.slice(startIdx + 1, endIdx === -1 ? undefined : endIdx);
    
    return sectionLines
      .map(l => l.trim())
      .filter(l => l.length > 0 && !l.startsWith('-'));
  }
}
```

### Workflow Service

The main orchestration logic:

```typescript
class WorkflowService {
  constructor(
    private claude: ClaudeService,
    private rserve: RserveService,
    private parser: TextParserService
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
    // Get initial exploration code from Claude
    const step = await this.claude.generateDataUnderstandingStep(
      job.files,
      job.instructions
    );
    
    // Execute and capture output
    const result = await this.executeStep(step, state);
    
    // Let Claude interpret the results and plan entity mapping
    const interpretation = await this.claude.interpretDataStructure(
      job.files,
      job.instructions,
      result.output
    );
    
    state.entityMapping = interpretation.entityMapping;
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
        job.files.find(f => f.name === state.entityMapping[entityName]),
        this.getEntityContext(state, entityName),
        attempts
      );
      
      // Execute step
      const result = await this.executeStep(step, state);
      
      // Check if entity validates
      if (result.output.includes('inspect(')) {
        const inspection = this.parser.parseEntityInspection(result.output);
        // Store for context
        state.lastInspection[entityName] = inspection;
      }
      
      if (result.output.includes('validate')) {
        const validation = this.parser.parseValidationResult(result.output);
        validated = validation.isValid;
        state.entityValidationStatus[entityName] = validated;
      }
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
      
      // Store step and result
      state.steps.push({ step, result });
      
      // Send real-time update
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
      
      state.steps.push({ step, result });
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
${state.lastInspection[entityName] ? 
  `Last inspection showed: ${JSON.stringify(state.lastInspection[entityName])}` : ''}
    `;
  }
}
```

### Claude Service for Stepwise Generation

```typescript
class ClaudeService {
  async generateDataUnderstandingStep(
    files: UploadedFile[],
    instructions: string
  ): Promise<WranglerStep> {
    const prompt = `
You are analyzing data files to understand their structure and relationships.
Generate R code to explore the uploaded data files.

Files uploaded:
${files.map(f => `- ${f.name} (${f.size} bytes)`).join('\n')}

User instructions: ${instructions}

Generate R code that:
1. Shows the first few rows of each file
2. Shows column names and types
3. Identifies potential ID columns
4. Shows row counts
5. Looks for obvious relationships between files

Use str(), head(), names(), and summary() to explore the data.
`;

    const response = await this.client.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 2000,
      messages: [{ role: 'user', content: prompt }]
    });
    
    return {
      id: uuidv4(),
      phase: 'understanding',
      purpose: 'Initial data exploration',
      code: this.extractRCode(response.content[0].text),
      attempt: 1
    };
  }
  
  async generateEntityWranglingStep(
    entityName: string,
    file: UploadedFile,
    context: string,
    attempt: number
  ): Promise<WranglerStep> {
    const prompt = `
You are wrangling the "${entityName}" entity from file "${file.name}".
${attempt > 1 ? `This is attempt ${attempt}. Previous attempts and their outputs:` : ''}
${context}

Generate R code using study.wrangler functions to:
${attempt === 1 ? `
1. Load entity from file using entity_from_file(file_path, name = "entity_name")
2. Run inspect(entity) to see current state and issues  
3. Address any issues found (multivalued columns, missing ID columns, etc.)
4. Run validate(entity) to check if it passes validation
5. If validation fails, address the specific errors and try again
` : `
Fix the validation issues from the previous attempt.
Focus on the specific errors/warnings shown in the output.
Common fixes:
- Use set_parents() for missing parent relationships  
- Use set_variables_multivalued() for delimited columns
- Use redetect_columns_as_variables() to change ID detection
- Use sync_variable_metadata() to fix metadata alignment
After fixes, run inspect() and validate() again.
`}

Available functions (UPDATED with actual API):
- entity_from_file(file_path, preprocess_fn = NULL, name = "entity_name", ...)
- entity_from_csv(file_path, preprocess_fn = NULL, ...)  
- entity_from_tsv(file_path, preprocess_fn = NULL, ...)
- entity_from_tibble(data, preprocess_fn = NULL, skip_type_convert = FALSE, ...)
- inspect(entity, variable_name = NULL)
- inspect_variable(entity, variable_name)
- validate(entity) # returns boolean TRUE/FALSE
- study_from_entities(entities, name = "study_name", ...)
- set_parents(entity, names = c("parent_name"), id_columns = c("parent.id"))
- set_variables_multivalued(entity, 'variable.name' = 'delimiter')
- set_variables_univalued(entity, 'variable.name')
- redetect_columns_as_variables(entity, c('col_name.1', 'col_name.2'))
- redetect_column_as_id(entity, column_name)
- sync_variable_metadata(entity)
- set_entity_metadata(entity, ...)
- set_variable_metadata(entity, variable_name, ...)
- get_entity_name(entity)
- get_data(entity)
- export_to_stf(study, output_dir)
`;

    const response = await this.client.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 2000,
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
- Set up monorepo with Next.js and orchestrator
- Implement basic file upload and job creation
- Set up Docker environment with Rserve
- Create WebSocket connection for progress updates

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

## Security & Performance

- R sessions isolated per job
- Temporary files cleaned up after processing
- Rate limiting on API endpoints
- Maximum file size limits enforced
- Timeout protection for long-running operations

## Future Enhancements

1. **Learning from Success**: Store successful wrangling patterns
2. **Parallel Entity Processing**: Process independent entities simultaneously  
3. **Advanced File Handling**: Support for 1-to-many file-to-entity mappings
4. **Custom Wrangler Functions**: Extend API based on common needs
5. **Export Options**: Multiple output formats beyond STF

