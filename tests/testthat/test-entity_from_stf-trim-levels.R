# read_tsv() trims data cells, so padded vocabulary_order / ordinal_levels
# entries in the YAML must be trimmed on read to match them.

make_padded_stf <- function(output_dir, vocabulary_order, ordinal_levels) {
  sites <- entity_from_tibble(
    tibble(
      site = c("S1", "S2", "S3"),
      habitat = c("forest edge", "river", "forest edge"),
      stage = c("early", "late", "early")
    ),
    name = "site",
    quiet = TRUE
  ) %>%
    set_variable_metadata("habitat", display_name = "habitat") %>%
    set_variable_metadata("stage", display_name = "stage") %>%
    set_variable_ordinal_levels("stage", levels = c("early", "late"))

  study_from_entities(list(sites), name = "padded levels") %>%
    quiet() %>%
    export_to_stf(output_dir)

  tsv_path <- file.path(output_dir, "entity-site.tsv")
  tsv <- readLines(tsv_path)
  writeLines(
    gsub("forest edge", "forest edge   ", gsub("\tearly", "\t  early ", tsv, fixed = TRUE), fixed = TRUE),
    tsv_path
  )

  yaml_path <- file.path(output_dir, "entity-site.yaml")
  metadata <- yaml::read_yaml(yaml_path)
  metadata$variables <- map(metadata$variables, function(v) {
    if (v$variable == "habitat") v$vocabulary_order <- vocabulary_order
    if (v$variable == "stage") v$ordinal_levels <- ordinal_levels
    v
  })
  yaml::write_yaml(metadata, yaml_path)
}

exported_vocabulary <- function(output_dir, study, entity, variable_name) {
  stable_id <- entity %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == variable_name) %>%
    pull(stable_id)
  column_names <- attributegraph_table_fields %>%
    map(~ list(name = .x$name, cacheFileIndex = .x$cacheFileIndex)) %>%
    bind_rows() %>%
    arrange(cacheFileIndex) %>%
    pull(name)
  cache <- list.files(output_dir, pattern = "^attributegraph_.*\\.cache$", full.names = TRUE)
  readr::read_tsv(cache, col_names = column_names, col_types = readr::cols(.default = "c"),
                  quote = "", trim_ws = FALSE) %>%
    filter(stable_id == !!stable_id) %>%
    pull(vocabulary)
}

test_that("padded vocabulary_order and ordinal_levels in STF YAML are trimmed to match the data", {
  stf_dir <- withr::local_tempdir()
  make_padded_stf(
    stf_dir,
    vocabulary_order = list("  forest edge   ", "river"),
    ordinal_levels = list("  early ", "late")
  )

  expect_no_warning(study <- study_from_stf(stf_dir) %>% quiet())
  sites <- study %>% get_entity("site")
  metadata <- sites %>% get_variable_metadata()
  expect_equal(metadata %>% filter(variable == "habitat") %>% pull(vocabulary_order) %>% unlist(),
               c("forest edge", "river"))
  expect_equal(metadata %>% filter(variable == "stage") %>% pull(ordinal_levels) %>% unlist(),
               c("early", "late"))

  expect_true(study %>% validate(profiles = c("baseline", "eda")))

  vdi_dir <- withr::local_tempdir()
  expect_no_error(study %>% export_to_vdi(vdi_dir))
  expect_equal(exported_vocabulary(vdi_dir, study, sites, "habitat"), '["forest edge","river"]')
  expect_equal(exported_vocabulary(vdi_dir, study, sites, "stage"), '["early","late"]')
})

test_that("entries duplicated or emptied by the trim are dropped with a warning naming the variable", {
  stf_dir <- withr::local_tempdir()
  make_padded_stf(
    stf_dir,
    vocabulary_order = list("forest edge", " forest edge ", "  ", "river"),
    ordinal_levels = list("early", "late", "late ")
  )

  warnings <- character()
  study <- withCallingHandlers(
    study_from_stf(stf_dir) %>% quiet(),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 2)
  expect_match(warnings, "'habitat': vocabulary_order", fixed = TRUE, all = FALSE)
  expect_match(warnings, "'stage': ordinal_levels", fixed = TRUE, all = FALSE)

  metadata <- study %>% get_entity("site") %>% get_variable_metadata()
  expect_equal(metadata %>% filter(variable == "habitat") %>% pull(vocabulary_order) %>% unlist(),
               c("forest edge", "river"))
  expect_equal(metadata %>% filter(variable == "stage") %>% pull(ordinal_levels) %>% unlist(),
               c("early", "late"))
  expect_true(study %>% validate(profiles = c("baseline", "eda")))
})

test_that("vocabulary_order set through the R API is not trimmed", {
  sites <- entity_from_tibble(
    tibble(site = c("S1", "S2", "S3"), habitat = c(" river ", "forest edge", " river ")),
    name = "site",
    quiet = TRUE
  ) %>%
    set_variable_metadata("habitat", vocabulary_order = list(" river "))

  expect_equal(
    sites %>% get_variable_metadata() %>% filter(variable == "habitat") %>% pull(vocabulary_order) %>% unlist(),
    " river "
  )
})
