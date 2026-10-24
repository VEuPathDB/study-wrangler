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

# Scalar top-level keys of an entity YAML file: entity_from_stf() passes every
# scalar top-level key to entity(), so they are the constructor's scalar
# arguments. `quiet` is an R session setting, not study metadata.
entity_scalar_yaml_keys <- function() {
  setdiff(names(formals(entity)), c("data", "variables", "children", "collections", "quiet"))
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
  scalar_args <- entity_scalar_yaml_keys()
  sections <- c("id_columns", "variables", "categories", "collections")
  keys <- c(scalar_args, sections)
  undocumented <- keys[!map_lgl(keys, ~ documented_in_backticks(doc, .x))]
  expect_equal(undocumented, character(0))
})

test_that("the documented section names are exactly the ones entity_from_stf() reads", {
  doc <- read_metadata_fields_doc()
  # entity_from_stf() silently drops unknown list-valued top-level keys, so a
  # misspelt section is ignored without error. The doc spells the sections out;
  # keep that list in step with the `metadata$<section>` reads in the loader.
  documented_sections <- c("id_columns", "variables", "categories", "collections")
  loader_source <- paste(deparse(body(entity_from_stf)), collapse = "\n")
  loader_keys <- unique(str_match_all(loader_source, "metadata\\$([A-Za-z_]+)")[[1]][, 2])
  loader_sections <- setdiff(loader_keys, entity_scalar_yaml_keys())
  expect_setequal(loader_sections, documented_sections)
  expect_true(any(grepl("silently", doc, fixed = TRUE) & grepl("misspelt", doc, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Study-Transfer-Format-STF.md: the worked examples must load and validate,
# and the section anchors that metadata-fields.md links to must exist.

stf_format_doc_path <- function() {
  test_path("..", "..", "STF_documentation", "Study-Transfer-Format-STF.md")
}

read_stf_format_doc <- function() {
  path <- stf_format_doc_path()
  skip_if_not(file.exists(path), "STF_documentation/ is not available (installed package)")
  readLines(path, warn = FALSE)
}

# The contents of the fenced code block that follows the line
# `<!-- example:<name> -->` in `doc`
doc_example_block <- function(doc, name) {
  marker <- which(trimws(doc) == paste0("<!-- example:", name, " -->"))
  expect_length(marker, 1)
  fences <- which(grepl("^```", doc))
  fences <- fences[fences > marker]
  expect_gte(length(fences), 2)
  if (length(marker) != 1 || length(fences) < 2) return(character(0))
  doc[seq(fences[1] + 1, fences[2] - 1)]
}

# Write an STF directory for a one-entity study from a doc example
write_doc_example_stf <- function(doc, example, entity_name) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(doc_example_block(doc, paste0(example, ":tsv")),
             file.path(dir, paste0("entity-", entity_name, ".tsv")))
  writeLines(doc_example_block(doc, paste0(example, ":yaml")),
             file.path(dir, paste0("entity-", entity_name, ".yaml")))
  writeLines(c(paste0("name: Documentation example (", example, ")"),
               "entities:", paste0("  - ", entity_name)),
             file.path(dir, "study.yaml"))
  dir
}

# GitHub's heading anchor: lower case, punctuation other than `-` and `_`
# removed, spaces turned into hyphens
github_slug <- function(heading) {
  heading %>% tolower() %>% gsub("[^a-z0-9 _-]", "", .) %>% gsub(" ", "-", .)
}

test_that("the format doc has the sections that metadata-fields.md links to", {
  doc <- read_stf_format_doc()
  headings <- doc[grepl("^#+ ", doc)] %>% sub("^#+ +", "", .) %>% trimws()
  expected <- c(
    "Variable categories (the variable tree)",
    "Collections",
    "Geocoordinates for EDA",
    "Curation notes (curation-notes.md)"
  )
  expect_true(all(expected %in% headings), info = paste(setdiff(expected, headings), collapse = ", "))

  fields_doc <- read_metadata_fields_doc()
  linked <- str_match_all(paste(fields_doc, collapse = "\n"),
                          "Study-Transfer-Format-STF\\.md#([a-z0-9_-]+)")[[1]][, 2] %>% unique()
  expect_setequal(setdiff(linked, map_chr(headings, github_slug)), character(0))
})

test_that("the variable categories example loads and validates", {
  doc <- read_stf_format_doc()
  dir <- write_doc_example_stf(doc, "categories", "participant")
  study <- study_from_stf(dir) %>% quiet()
  expect_true(validate(study))
  expect_true(validate(study, profiles = c("baseline", "eda")))

  participant <- study %>% get_entity("participant")
  categories <- participant %>% get_category_metadata()
  expect_gte(nrow(categories), 2)
  # two levels: at least one category sits inside another
  expect_true(any(categories$parent_variable %in% categories$variable))
})

test_that("the geocoordinates example loads, and passes EDA validation after inference", {
  doc <- read_stf_format_doc()
  dir <- write_doc_example_stf(doc, "geo", "site")
  study <- study_from_stf(dir) %>% quiet()
  expect_true(validate(study))

  site <- study %>% get_entity("site")
  expect_false(any(grepl("^geohash_", names(site@data))))
  # without the geohash columns EDA validation fails ...
  expect_false(suppressWarnings(suppressMessages(validate(study, profiles = c("baseline", "eda")))))

  # ... and the documented step derives them
  study <- suppressMessages(study %>% map_entities(infer_geo_variables_for_eda)) %>% quiet()
  expect_equal(sum(grepl("^geohash_", names(get_entity(study, "site")@data))), 6)
  expect_true(validate(study, profiles = c("baseline", "eda")))
})
