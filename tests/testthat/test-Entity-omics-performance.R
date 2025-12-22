#' Performance tests for wide omics entities
#'
#' These tests benchmark hydration performance on synthetic wide datasets
#' to validate optimizations for entities with many similar variables.
#'
#' These benchmarks are opt-in and only run when RUN_BENCHMARKS=true.
#'
#' How to run:
#'
#' From R console/RStudio:
#'   Sys.setenv(RUN_BENCHMARKS = "true")
#'   devtools::test_file("tests/testthat/test-Entity-omics-performance.R")
#'
#' From terminal:
#'   RUN_BENCHMARKS=true R -e "devtools::test_file('tests/testthat/test-Entity-omics-performance.R')"
#'
#' From Docker (as used by Claude Code):
#'   docker exec --user rstudio study-wrangler-dev R -e "Sys.setenv(RUN_BENCHMARKS='true'); library(devtools); test_file('tests/testthat/test-Entity-omics-performance.R')"

test_that("Benchmark: stable_id optimization for wide omics entities", {
  skip_if_not(Sys.getenv("RUN_BENCHMARKS") == "true",
              "Performance benchmarks only run with RUN_BENCHMARKS=true")

  # Create synthetic wide omics entity
  n_samples <- 200
  n_genes <- 25000  # Upper bound for most omics experiments

  # Generate sample IDs
  sample_ids <- paste0("SAMPLE_", sprintf("%04d", 1:n_samples))

  # Generate gene expression data (all continuous, similar distributions)
  set.seed(123)
  gene_data <- matrix(
    rnorm(n_samples * n_genes, mean = 5, sd = 2),
    nrow = n_samples,
    ncol = n_genes
  )
  colnames(gene_data) <- paste0("GENE_", sprintf("%05d", 1:n_genes), "_expression")

  # Combine into tibble
  data <- bind_cols(
    tibble(Sample.Id = sample_ids),
    as_tibble(gene_data)
  )

  # Create entity WITHOUT stable_ids (baseline - slow path)
  message("\n=== Creating baseline entity (no stable_ids) ===")
  entity_no_stable_ids <- entity_from_tibble(
    data = data,
    name = "baseline_omics",
    skip_type_convert = TRUE
  ) %>%
    quiet %>%
    redetect_column_as_id("Sample.Id")

  # Create entity WITH stable_ids (optimized path)
  message("\n=== Creating optimized entity (with stable_ids) ===")
  entity_with_stable_ids <- entity_from_tibble(
    data = data,
    name = "optimized_omics",
    skip_type_convert = TRUE
  ) %>%
    quiet() %>%
    redetect_column_as_id("Sample.Id")

  # Set stable_ids for all gene variables (use gene name as stable_id)
  gene_vars <- colnames(gene_data)
  entity_with_stable_ids <- entity_with_stable_ids %>%
    set_variables_stable_ids(gene_vars)

  # Validate that stable_ids were set correctly
  expect_true(entity_with_stable_ids %>% validate())

  # Benchmark hydration WITHOUT stable_ids
  message("\n=== Benchmarking WITHOUT stable_ids (baseline) ===")
  time_baseline <- system.time({
    metadata_baseline <- entity_no_stable_ids %>%
      get_hydrated_variable_and_category_metadata()
  })

  # Clear cache to ensure fair comparison
  rm(list = ls(envir = .hvacm_cache), envir = .hvacm_cache)

  # Benchmark hydration WITH stable_ids
  message("\n=== Benchmarking WITH stable_ids (optimized) ===")
  time_optimized <- system.time({
    metadata_optimized <- entity_with_stable_ids %>%
      get_hydrated_variable_and_category_metadata()
  })

  # Report results
  message("\n=== BENCHMARK RESULTS ===")
  message(sprintf("Baseline (no stable_ids):    %.2f seconds", time_baseline["elapsed"]))
  message(sprintf("Optimized (with stable_ids): %.2f seconds", time_optimized["elapsed"]))
  message(sprintf("Speedup: %.1fx", time_baseline["elapsed"] / time_optimized["elapsed"]))

  # Verify results are equivalent (all stable_ids should be set)
  expect_true(all(!is.na(metadata_baseline$stable_id)))
  expect_true(all(!is.na(metadata_optimized$stable_id)))

  # Check that optimized version used provided stable_ids
  gene_metadata_optimized <- metadata_optimized %>%
    filter(variable %in% gene_vars)
  expect_true(all(gene_metadata_optimized$stable_id == gene_metadata_optimized$variable))
})
