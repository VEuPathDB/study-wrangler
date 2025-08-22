# Biomedical Data Wrangler - MCP Architecture Plan

## Overview

An automated biomedical data processing system that uses Claude MCP (Model Context Protocol) to provide a conversational interface for wrangling messy biomedical data files into clean, validated formats. The system leverages the existing R package `study.wrangler` (VEuPathDB/study-wrangler) through iterative script building.

The `study.wrangler` package provides tools for converting tabular data files (TSV/CSV) into validated Entity objects, establishing parent-child relationships between entities, and exporting to Study Transfer Format (STF) for biomedical research databases.

## Single-User Local Architecture

All-local implementation using npm-installable packages with Docker containerization for R/RServe dependencies. Eliminates multi-user complexity while providing a clean, reproducible environment.

### User Installation & Setup

```bash
# Install the toolchain
npm install -g claude-code
npm install -g bio-wrangler-mcp

# Navigate to study directory and start
cd ~/my-biomedical-study
bio-wrangler-mcp start     # Mounts current directory into container
claude-code                # Connects to local MCP server
```

### Architecture Components

- **Docker Container**: R + study-wrangler library + RServe service (localhost:6311)
- **MCP Server (TypeScript)**: Container lifecycle management + RServe client + MCP tool exposure
- **Claude Code**: MCP client with conversational interface and file system integration

## User Workflow

1. **Data Preparation**: User starts with directory containing tab-delimited files (one per entity/table)

2. **Interactive Discovery**: Through Claude Code chat interface:
   - User describes data and entity relationships in natural language
   - AI inspects files using `file_path %>% entity_from_file(name = "entity_name") %>% inspect()`
   - Back-and-forth clarification questions based on inspection output

3. **Entity Wrangling**: Iterative process to generate R scripts:
   - Load entities with `file_path %>% entity_from_file(name = "entity_name")`
   - Fix issues identified by `inspect()` using functions like:
     - `entity %>% set_variables_multivalued('column_name' = 'delimiter')` for comma-separated columns
     - `entity %>% set_parents(names = c("parent_entity"), id_columns = c("parent_id_col"))` for hierarchical relationships  
     - `entity %>% redetect_columns_as_variables(c('wrongly_detected_id'))` for ID detection fixes
     - `entity %>% sync_variable_metadata()` for metadata synchronization
   - Validate with `entity %>% validate()` until successful
   - Build up working R script incrementally

4. **Study Assembly**: 
   - Call `study_from_entities(list(entity1, entity2, ...), name = "study_name")` to combine entities
   - Validate with `study %>% validate()` to check all entity relationships

5. **Export**: Generate final Study Transfer Format (STF) files using `study %>% export_to_stf("output_directory")` and save R scripts

## Repository Organization

The study-wrangler repository maintains its original R package structure while adding MCP components:

```
study-wrangler/                   # Root maintains R package structure
├── R/                           # Core R package source code
│   ├── entity_from_file.R       # Creates Entity objects from TSV/CSV files
│   ├── Entity-inspect.R         # Inspection methods showing data structure & issues
│   ├── Entity-validate.R        # Validation with detailed error messages & fix suggestions
│   ├── Entity-methods.R         # Core entity manipulation methods
│   ├── study_from_entities.R    # Combines entities into Study objects
│   ├── Study-validate.R         # Study-level validation
│   └── Study-export-STF.R       # STF format export
├── man/                         # R documentation
├── tests/                       # R package tests
├── inst/extdata/toy_example/    # Sample datasets for testing
│   ├── households.tsv
│   ├── participants.tsv
│   └── participant_observations.tsv
├── DESCRIPTION                  # R package metadata
├── NAMESPACE                    # R package exports
│
├── bio-wrangler-mcp/            # NEW: NPM package for MCP server
│   ├── package.json            # TypeScript MCP server configuration
│   ├── src/                    # TypeScript source code
│   │   ├── mcp-server.ts       # Main MCP server implementation
│   │   ├── container-manager.ts # Docker lifecycle management  
│   │   └── rserve-client.ts    # RServe communication layer
│   └── dist/                   # Compiled JavaScript output
│
├── docker/                     # NEW: Layered Docker images
│   ├── Dockerfile.base         # Base image with study.wrangler + dependencies
│   ├── Dockerfile.rstudio      # RStudio development environment
│   ├── Dockerfile.rserve       # RServe service for MCP server
│   ├── start-rserve.R          # Smart R startup script (dev/prod modes)
│   └── build.sh               # Build script for all images
│
└── CLAUDE-MCP.md               # This architecture document
```

**Key R Package Components:**
- **Entity Class**: S4 object with `@data` (tibble), `@variables` (metadata), `@name`, `@children`
- **Variable Metadata**: Extensive metadata including `data_type`, `data_shape`, `provider_label`, `is_multi_valued`
- **Validation System**: Rich text output with specific error messages and fix suggestions
- **Hierarchical Relationships**: Parent-child entity relationships with ID column mappings

## Technical Architecture

### MCP Tools

- `start_wrangling_session()` - Initialize R session in container
- `inspect_file(filepath, entity_name)` - Run `filepath %>% entity_from_file(name = entity_name) %>% inspect()`
- `validate_entity(entity_var_name)` - Execute `entity_var_name %>% validate()` and return results
- `reload_study_wrangler()` - Reload R package with `devtools::load_all()` (dev mode)
- `execute_r_code(code)` - Execute arbitrary R code and capture output
- `create_study(entity_names, study_name)` - Run `study_from_entities(list(...), name = study_name)`
- `export_to_stf(study_var_name, output_dir)` - Execute STF export

### Container Management

- **Start time mounting**: Directory mounted when `bio-wrangler-mcp start` is called
- **Development mode**: Auto-detects and mounts R source for `devtools::load_all()`
- **Session isolation**: One container per study directory session  
- **Automatic Docker handling**: MCP server manages container lifecycle
- **Local port binding**: RServe accessible only to local MCP server

## Key Benefits

### For Users
- **Single install**: Just npm + Docker, no R configuration needed
- **Directory-native**: Work directly with local file structures
- **One study per session**: Clean separation, mount current directory only
- **Reproducible**: Identical R environment regardless of host system

### For Implementation  
- **No multi-user complexity**: Single local session, no authentication/isolation needed
- **Platform agnostic**: Docker handles R installation differences
- **Version controlled**: Container tags for study-wrangler library updates
- **Development friendly**: Fast R package reloading without container rebuilds

## Key Design Decisions

### Constraints Applied
- **No arbitrary R code generation** - AI limited to study-wrangler functions only
- **Single-user focus** - No collaboration features needed
- **Memory-based processing** - No intermediate file I/O during wrangling
- **Iterative script building** - Build up working R scripts step by step

### Efficiency Optimizations
- **Persistent R sessions** - Maintain object state between operations
- **Incremental execution** - Avoid full script re-runs when possible
- **Local file system integration** - Direct access to user's data directories

### User Experience Benefits
- **Conversational interface** - Natural language descriptions instead of web forms
- **File-system native** - Work directly with local directories
- **Iterative refinement** - Natural back-and-forth during validation
- **Simplified deployment** - Single npm package installation

## Dependencies

- **Docker**: Container runtime for R environment
- **Node.js**: MCP server runtime
- **Claude Code**: User interface (MCP client)
- **R + study.wrangler**: Containerized, no local R installation needed