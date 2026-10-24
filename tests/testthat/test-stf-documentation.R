# Drift tests: STF_documentation/metadata-fields.md must keep up with the
# metadata fields the STF loaders accept (variable_metadata_defaults and
# collection_metadata_defaults) and with their allowed values.
#
# STF_documentation/ lives at the repository root, not in inst/, so it is not
# part of an installed package. These tests run under devtools::test() and
# skip under R CMD check.

metadata_fields_doc_path <- function() {
  test_path("..", "..", "STF_documentation", "metadata-fields.md")
}

read_metadata_fields_doc <- function() {
  path <- metadata_fields_doc_path()
  skip_if_not(file.exists(path), "STF_documentation/ is not available (installed package)")
  readLines(path, warn = FALSE)
}

# TRUE when `term` appears in backticks somewhere in the doc
documented_in_backticks <- function(doc, term) {
  any(grepl(paste0("`", term, "`"), doc, fixed = TRUE))
}

test_that("metadata-fields.md documents every variable metadata field", {
  doc <- read_metadata_fields_doc()
  internal_only <- c(
    # entity_from_stf() forces has_values = FALSE for every entry in
    # `categories` and the default TRUE applies to every column-backed
    # variable, so a YAML author never sets it
    "has_values"
  )
  fields <- setdiff(names(variable_metadata_defaults), internal_only)
  undocumented <- fields[!map_lgl(fields, ~ documented_in_backticks(doc, .x))]
  expect_equal(undocumented, character(0))
})

test_that("metadata-fields.md documents every collection metadata field", {
  doc <- read_metadata_fields_doc()
  fields <- c("category", names(collection_metadata_defaults))
  undocumented <- fields[!map_lgl(fields, ~ documented_in_backticks(doc, .x))]
  expect_equal(undocumented, character(0))
})

test_that("metadata-fields.md lists every allowed value of the factor fields", {
  doc <- read_metadata_fields_doc()
  factor_levels <- list(
    data_type = levels(variable_metadata_defaults$data_type),
    data_shape = levels(variable_metadata_defaults$data_shape),
    display_type = levels(variable_metadata_defaults$display_type),
    scale = levels(variable_metadata_defaults$scale),
    hidden = levels(variable_metadata_defaults$hidden[[1]])
  )
  undocumented <- factor_levels %>%
    imap(~ paste0(.y, ": ", .x[!map_lgl(.x, function(level) documented_in_backticks(doc, level))])) %>%
    unlist() %>%
    keep(~ !grepl(": $", .x)) %>%
    unname()
  expect_equal(undocumented, character(0))
})

test_that("metadata-fields.md documents every entity-level YAML key", {
  doc <- read_metadata_fields_doc()
  # entity_from_stf() passes every scalar top-level YAML key to entity(), so
  # the accepted keys are the constructor's scalar arguments. `quiet` is an
  # R session setting, not study metadata.
  scalar_args <- setdiff(names(formals(entity)), c("data", "variables", "children", "collections", "quiet"))
  sections <- c("id_columns", "variables", "categories", "collections")
  keys <- c(scalar_args, sections)
  undocumented <- keys[!map_lgl(keys, ~ documented_in_backticks(doc, .x))]
  expect_equal(undocumented, character(0))
})
