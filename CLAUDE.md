# Study Wrangler AI Ecosystem - Overall Vision

## Vision Statement

Transform biomedical data wrangling from a manual, error-prone process into an AI-assisted workflow that guides researchers through converting messy tabular data files into clean, validated, database-ready formats. The ecosystem progresses from local proof-of-concept to integrated web service for the VEuPathDB research community.

## The Data Wrangling Challenge

Biomedical researchers frequently work with complex relational datasets spread across multiple CSV/TSV files with issues like:
- Inconsistent column naming and data types
- Missing or incorrect entity relationships 
- Multivalued columns (comma-separated values)
- Poor metadata annotation
- Complex parent-child hierarchies between entities

The `study.wrangler` R package solves these technical challenges, but requires deep R expertise. Our AI ecosystem makes this powerful tooling accessible through conversational interfaces.

## Core AI Approach: Stepwise Wrangling

Both implementations share the same fundamental approach:

### Phase 1: Data Discovery & Planning
- AI analyzes uploaded files using `entity_from_file() %>% inspect()`
- Parses structured text output to understand data structure
- Creates entity mapping plan with relationships and processing order
- Engages user in clarifying questions about domain-specific context

### Phase 2: Iterative Entity Wrangling  
- For each entity: load → inspect → fix issues → validate → repeat until clean
- AI generates targeted R code using study.wrangler functions:
  - `set_variables_multivalued()` for delimited columns
  - `set_parents()` for hierarchical relationships
  - `redetect_columns_as_variables()` for ID detection fixes
  - `sync_variable_metadata()` for metadata alignment
- Parse validation output to guide next steps

### Phase 3: Study Assembly & Export
- Combine validated entities into study object
- Cross-entity validation and relationship verification
- Export to Study Transfer Format (STF) for database ingestion
- Generate complete, reproducible R scripts

## Implementation Approach

### Phase 1: Local Single-User Validation (MCP Architecture)
**Target**: Proof of concept and AI prompt refinement
**Deployment**: `npm install -g bio-wrangler-mcp` + Docker
**Interface**: Claude Code chat with MCP tools
**Benefits**: 
- Zero configuration, works with existing file structures
- Rapid iteration on AI prompts and conversation flow
- Safe testing environment for complex datasets
- No scalability concerns during development

### Phase 2: ClinEpiDB/VEuPathDB Integration 
**Target**: Production web service for research community
**Deployment**: Cloud-native chat widget integrated into existing portals
**Interface**: AI chat assistant embedded in data upload workflows
**Benefits**: 
- No installation required for researchers
- Integrated with existing authentication and file management
- Scalable to handle variable research community load
- Direct path to database ingestion via existing workflows

## Shared Technical Foundation

### R Package Preservation
Both implementations build on the unmodified `study.wrangler` R package:
- Existing GitHub installation continues working: `remotes::install_github('VEuPathDB/study-wrangler')`
- AI layers parse existing text output from `inspect()` and `validate()` methods
- No modifications to core R functionality required

### Security & Validation
- Generated R code limited to study.wrangler function whitelist
- Input sanitization prevents prompt injection
- Containerized R execution with resource limits
- Session isolation between users/jobs

### AI Context Management
- Stateless Claude API calls with comprehensive context passing
- Entity mapping plans preserved across wrangling steps
- Previous validation outputs inform next iteration attempts
- User instructions maintained throughout workflow

## Development Roadmap

### Phase 1: MCP Proof of Concept (6 weeks)
**Goal**: Validate AI approach and refine conversation patterns
- Docker-based R environment with study.wrangler
- MCP tools for file inspection and entity wrangling  
- Claude Code integration for conversational interface
- Comprehensive testing with diverse biomedical datasets
- AI prompt optimization based on real-world usage patterns

### Phase 2: Web Integration Planning (2 weeks)
**Goal**: Architecture design for ClinEpiDB/VEuPathDB integration
- Scalability analysis for concurrent users and large files
- Infrastructure requirements and cost estimation  
- Integration points with existing authentication/file systems
- User experience design for chat-assisted data upload

### Phase 3: Web Service Implementation (8 weeks)
**Goal**: Production-ready integration with research portals
- Chat widget embedded in existing upload workflows
- Auto-scaling R backend with queue management
- Integration with VEuPathDB authentication and file handling
- Advanced error handling and user guidance
- Performance monitoring and optimization

### Phase 4: Community Deployment (4 weeks)
**Goal**: Launch and community adoption
- Beta testing with select research groups
- Documentation and user onboarding materials  
- Monitoring and feedback collection systems
- Iterative improvements based on real usage

## Success Metrics

### Phase 1 Validation
- Successfully wrangle 20+ diverse biomedical datasets  
- AI conversation quality rated >4/5 by test researchers
- <10% cases requiring manual R intervention
- Conversation patterns documented for web implementation

### Phase 2 Production Service
- Handle 50+ concurrent users during peak hours
- <2 minute average wait time for R backend availability
- 95%+ successful completion rate for upload workflows
- Integration seamlessly with existing ClinEpiDB/VEuPathDB UX

### Research Impact
- 200+ datasets successfully wrangled within first year
- 80% reduction in data preparation time for participating researchers  
- Improved metadata quality measurable in resulting datasets
- Positive researcher satisfaction surveys (>4.5/5 rating)

## Key Technical Decisions

### Why Start with MCP Architecture?
- **Rapid AI iteration**: Perfect conversation flows without web complexity
- **Real dataset testing**: Work with actual researcher files during development
- **Lower stakes validation**: Prove concept before committing to web infrastructure
- **Parallel development**: Web architecture planning can happen alongside MCP refinement

### Why Skip Multi-User Standalone Platform?
- **Researcher workflow integration**: Users prefer embedded tools over separate platforms
- **Reduced maintenance burden**: One production system instead of two
- **Faster time-to-impact**: Direct path to serving the research community
- **Resource efficiency**: Focus development effort on final target architecture

## Long-term Vision

Transform biomedical data wrangling from an artisanal craft into a standardized, AI-assisted process integrated directly into research workflows. Enable researchers to seamlessly move from raw data files to database-ready formats within their existing tools, dramatically reducing time-to-publication for data-intensive studies while ensuring high-quality, well-annotated datasets that maximize research reproducibility.