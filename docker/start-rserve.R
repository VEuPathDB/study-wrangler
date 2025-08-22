#!/usr/bin/env Rscript

# Start RServe with appropriate study.wrangler loading mode

# Check if we're in development mode (R source mounted)
dev_mode <- file.exists("/study.wrangler.dev/DESCRIPTION")
dev_env <- Sys.getenv("DEV_MODE", "") == "true"

if (dev_mode || dev_env) {
  cat("Starting RServe in DEVELOPMENT mode\n")
  cat("Using devtools::load_all() for study.wrangler\n")
  
  # Load development version from mounted source
  if (dev_mode) {
    devtools::load_all("/study.wrangler.dev")
  } else {
    # Fallback to installed version if source not mounted
    library(study.wrangler)
  }
  
} else {
  cat("Starting RServe in PRODUCTION mode\n") 
  cat("Using installed study.wrangler package\n")
  
  # Load installed version
  library(study.wrangler)
}

# Verify library loaded
cat("study.wrangler version:", packageVersion("study.wrangler"), "\n")
cat("Starting RServe on port 6311...\n")

# Start RServe
Rserve::Rserve(debug=FALSE, args='--no-save')