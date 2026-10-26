# most STF testing is from a Study object but here we test the standalone entity file handling

expect_roundtrip <- function(entity, output_dir) {
  expect_silent(
    entity %>% export_entity_to_stf(output_dir)
  )
  tsv_path <- glue("{output_dir}/entity-{get_entity_name(entity)}.tsv")
  expect_silent(
    entity2 <- entity_from_stf(tsv_path)
  )
  expect_equal(
    entity2,
    entity
  )
}


test_that("A regular entity roundtrips via STF directory", {
  study_name <- 'my STF study'
  output_dir <- "./tmp/stf-entity"

  expect_no_error(
    study <- make_study(name = study_name)
  )
  observations <- study %>% get_entity('observation') %>% verbose()
  expect_true(observations %>% quiet() %>% validate())

  expect_roundtrip(observations, output_dir)
})

test_that("An entity with collections roundtrips via STF directory", {
  
  study_name <- 'my STF collections study'
  output_dir <- "./tmp/stf-collections-entity"
  
  expect_no_error(
    study <- make_study(name = study_name)
  )
  observations <- study %>% get_entity('observation') %>% verbose()
  
  category_spec <- list(
    category_name = "integer.measures",
    children = c("Height..cm.", "Weight..kg."),
    display_name = "integer-based anatomical measures",
    definition = "integer-based anatomical measures"
  )
  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(!!!category_spec),
      "Successfully created category 'integer.measures'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'integer.measures'"
  )
  
  expect_true(observations %>% quiet() %>% validate())

  collection_spec = list(
    member = "gene",
    member_plural = "genes",
    display_name = "raw read count",
    is_proportion = FALSE,
    is_compositional = FALSE,
    normalization_method = "none"
  )
  expect_silent(
    observations <- observations %>% create_variable_collection('integer.measures', !!!collection_spec)
  )
  
  expect_roundtrip(observations, output_dir)  
})

test_that("An all-numeric column declared as string loads as un-mutated character from STF", {
  dir <- withr::local_tempdir()
  tsv_path <- file.path(dir, "entity-team.tsv")
  writeLines(c("team \\\\ Descriptors\troom_number", "T1\t2.10", "T2\t3.50"), tsv_path)
  writeLines(c(
    "name: team",
    "id_columns:",
    "  - id_column: team",
    "variables:",
    "  - variable: room_number",
    "    data_type: string",
    "    data_shape: categorical"
  ), file.path(dir, "entity-team.yaml"))

  entity <- entity_from_stf(tsv_path)
  expect_type(get_data(entity)$room_number, "character")
  expect_equal(get_data(entity)$room_number, c("2.10", "3.50"))
})

test_that("A wide root entity (few rows, many columns) roundtrips via transposed STF", {
  output_dir <- "./tmp/stf-wide-root-entity"
  n_vars <- STF_TRANSPOSE_MIN_COLUMNS + 10
  data <- tibble(Household.Id = c("H001", "H002", "H003"))
  for (i in seq_len(n_vars)) {
    data[[sprintf("Var%03d", i)]] <- c(i, i + 1, i + 2)
  }

  entity <- entity_from_tibble(data, name = "household")
  expect_equal(dim(get_data(entity)), c(3, n_vars + 1))

  expect_silent(entity %>% export_entity_to_stf(output_dir))
  tsv_path <- file.path(output_dir, "entity-household.tsv")
  first_cell <- strsplit(readLines(tsv_path, n = 1), "\t")[[1]][1]
  expect_match(first_cell, "^Descriptors ")

  entity2 <- entity_from_stf(tsv_path)
  expect_equal(dim(get_data(entity2)), c(3, n_vars + 1))
  expect_equal(get_data(entity2)$Household.Id, c("H001", "H002", "H003"))
})

test_that("A TSV without the Descriptors marker is an error when YAML is present", {
  output_dir <- "./tmp/stf-no-marker"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tsv_path <- file.path(output_dir, "entity-person.tsv")
  writeLines(c("P.Id\tAge", "P1\t10", "P2\t20"), tsv_path)
  writeLines("name: person", file.path(output_dir, "entity-person.yaml"))
  expect_error(entity_from_stf(tsv_path), "Descriptors")
})

test_that("Fallback entity file match uses the file name, not the directory path", {
  output_dir <- "./tmp/entity-dir-fallback"
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)
  writeLines("notes", file.path(output_dir, "notes.tsv"))
  # notes.tsv must not be loaded as an entity just because the directory
  # name contains "entity-"; with no real entity files we get this error
  # rather than a "Descriptors marker" error from reading notes.tsv
  expect_error(study_from_stf(output_dir), "At least one entity must be provided")
})
