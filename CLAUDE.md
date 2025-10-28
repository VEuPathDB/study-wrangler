# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Common Development Commands

### Testing
- `devtools::test()` - Run all package tests (takes ~2 minutes)
- `testthat::test_check("study.wrangler")` - Alternative test runner
- Single test file: `devtools::load_all(); library(testthat); test_file('tests/testthat/test-FILENAME.R')`
- `devtools::check()` - Full R CMD check (includes tests, documentation, and package structure validation)

#### Snapshot Testing
Some tests use `testthat`'s snapshot testing to capture and validate exact output (e.g., VDI export format).

**Snapshot files location:** `tests/testthat/_snaps/`

**When to regenerate snapshots:**
- After intentional changes to export formats
- When adding new snapshot tests
- If snapshots become corrupted

**How to regenerate snapshots:**
```r
# From RStudio or R console
unlink("tests/testthat/_snaps/Test-Name.md")  # Delete specific snapshot
devtools::test_file("tests/testthat/test-File.R")  # Run test to regenerate
# Review the new snapshot carefully before committing!
```

**File permission issues (Docker):**
When running tests via `docker exec` as Claude Code does, snapshot files may be created with root ownership, causing permission errors in RStudio. Fix with:
```bash
docker exec study-wrangler-dev chown -R rstudio:rstudio /study.wrangler/tests/testthat/_snaps
```

To avoid this, Claude Code should run tests with the rstudio user:
```bash
docker exec --user rstudio study-wrangler-dev R -e "..."
```

**Committing snapshots:** Always commit the `_snaps/` directory contents to git. These serve as the baseline for future test runs.

### Package Development
- `devtools::load_all()` - Load all package functions during development (required after code changes)
- `devtools::document()` - Generate documentation and update NAMESPACE file
- `devtools::build(path='dist')` - Build the package into a tarball
- `devtools::install()` - Install the package locally

### Docker Development Environment
```bash
# Build container
docker build -t veupathdb/study-wrangler .

# Run named development environment for shared access
docker run --rm -ti --name study-wrangler-dev -v $PWD:/study.wrangler -e PASSWORD=password -p 8888:8787 veupathdb/study-wrangler
# Navigate to localhost:8888, login with "rstudio"/"password"
```

### Claude Code Integration (Shared Container)
When using the named container `study-wrangler-dev`, Claude Code can run commands in the same environment:
```bash
# Run all tests via Claude Code
docker exec study-wrangler-dev R -e "setwd('/study.wrangler'); library(devtools); test()"

# Run single test file via Claude Code  
docker exec study-wrangler-dev R -e "setwd('/study.wrangler'); library(devtools); load_all(); library(testthat); test_file('tests/testthat/test-Entity-categories.R')"

# Check package compilation
docker exec study-wrangler-dev R -e "setwd('/study.wrangler'); library(devtools); load_all()"

# Build documentation
docker exec study-wrangler-dev R -e "setwd('/study.wrangler'); library(devtools); document()"
```

## Architecture Overview

### Core Components

**Entity System**: The package centers around `Entity` and `Study` S4 classes for representing hierarchical study data:
- `Entity` class (`R/Entity-class.R`) - Represents data entities with metadata, validation, and transformation capabilities
- `Study` class (`R/Study-class.R`) - Container for collections of related entities
- Entities can have parent-child relationships forming hierarchical data structures

**Key Data Formats**:
- **STF (Study Transfer Format)**: YAML + TSV format for importing/exporting study data
- **VDI (VEuPathDB Data Import)**: Target format for VEuPathDB database loading

### Main Workflows

**Data Import Pipeline**:
1. `entity_from_file()` - Create entities from raw data files
2. `entity_from_stf()` - Import from STF format
3. `study_from_entities()` - Assemble entities into study objects

**Data Export Pipeline**:
1. `export_to_stf()` - Export studies to STF format
2. `export_to_vdi()` - Export studies to VDI format for database loading

**Data Validation & QC**:
- `validate()` methods for entities and studies with plugin-based validation profiles
- `inspect()` methods for data quality assessment
- Type inference and conversion utilities
- Validation profiles: "baseline" (default), "eda" (extended validation for EDA platform)

### File Organization

- **R/**: Core package source code
  - `00_*` files: Package setup (generics, imports)
  - `Entity-*` files: Entity class methods and functionality
  - `Study-*` files: Study class methods and functionality
  - `entity_from_*` and `study_from_*`: Constructor functions
  - `entity-validators-*` files: Validation functions organized by category
  - `validators.R`: Plugin-based validation registry system
- **tests/testthat/**: Test suite using testthat framework
- **inst/extdata/toy_example/**: Example datasets for testing
- **vignettes/**: Package tutorials and documentation
- **STF_documentation/**: Documentation for Study Transfer Format

### Dependencies

**Core Dependencies** (DESCRIPTION):
- `tidyverse` - Data manipulation and visualization
- `skimr` - Data summarization
- `glue` - String interpolation
- `knitr` - Dynamic document generation
- `digest` - Hashing utilities
- `yaml` - YAML parsing for STF format
- Custom: `plot.data` (VEuPathDB package for data visualization)

**Development Dependencies**:
- `testthat` - Testing framework
- `roxygen2` - Documentation generation
- `devtools` - Development tools

### Development Notes

- Uses S4 object system for Entity and Study classes
- Documentation generated with roxygen2 (use `@` tags in source comments)
- NAMESPACE file is auto-generated - do not edit manually
- Package follows tidyverse coding conventions
- Uses `load_all()` during development to reload changes
- Tests validate both individual functions and complete workflows (STF import/export, VDI export)