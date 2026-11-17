make_study_with_id_only_child <- function(with_category) {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(households_path, name = "household", quiet = TRUE)

  household_ids <- households %>% get_data() %>% pull(Household.Id)
  visits <- entity_from_tibble(
    tibble(
      Visit.Id = paste0("V", seq_along(household_ids)),
      Household.Id = household_ids
    ),
    name = "visit",
    quiet = TRUE
  ) %>%
    set_parents(names = "household", id_columns = "Household.Id")

  if (with_category) {
    visits <- visits %>% create_variable_category("visit_details", character(0)) %>% quiet()
  }

  study_from_entities(list(households, visits), name = "id-only child entity")
}

read_attributegraph_cache <- function(output_dir, study, entity_name) {
  path <- file.path(
    output_dir,
    glue("attributegraph_{get_study_abbreviation(study)}_{get_entity_abbreviation(study, entity_name)}.cache")
  )
  expect_true(file.exists(path))
  readLines(path)
}

for (with_category in c(FALSE, TRUE)) {
  test_that(glue("VDI export of a child entity with only ID columns (category: {with_category})"), {
    study <- make_study_with_id_only_child(with_category)
    visits <- study %>% get_entity("visit")
    expect_equal(visits %>% get_variable_metadata() %>% nrow(), 0)
    expect_equal(visits %>% get_category_metadata() %>% nrow(), as.integer(with_category))

    expect_true(study %>% quiet() %>% validate())

    hydrated <- visits %>% get_hydrated_variable_and_category_metadata()
    expect_true(all(c("vocabulary", "precision", "distinct_values_count",
                      "stable_id", "parent_stable_id") %in% names(hydrated)))
    expect_equal(nrow(hydrated), as.integer(with_category))

    output_dir <- withr::local_tempdir()
    expect_no_warning(expect_no_error(
      study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
    ))
    expect_true(file.exists(file.path(output_dir, "install.json")))

    rows <- read_attributegraph_cache(output_dir, study, "visit")
    if (with_category) {
      expect_length(rows, 1)
      expect_match(rows, hydrated$stable_id, fixed = TRUE)
    } else {
      expect_length(rows, 0)
    }

    install <- jsonlite::read_json(file.path(output_dir, "install.json"))
    table_names <- map_chr(install, ~ .x$name %||% NA_character_)
    visit_abbrev <- get_entity_abbreviation(study, "visit")
    expect_true(any(grepl(glue("^attributegraph_.*_{visit_abbrev}$"), table_names)))

    attributevalue_table <- install[[which(grepl(glue("^attributevalue_.*_{visit_abbrev}$"), table_names))]]
    expect_equal(attributevalue_table$fields[[3]]$maxLength, 1)
  })
}
