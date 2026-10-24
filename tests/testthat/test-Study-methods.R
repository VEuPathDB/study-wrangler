test_that("get_study_id() works", {
  expect_no_error(
    study <- make_study()
  )
  
  expect_equal(
    study %>% get_study_name(),
    NA_character_
  )
  
  expect_error(
    study_id <- study %>% get_study_id()
  )
  
  expect_no_error(
    study <- study %>% quiet() %>% set_study_name('amazing study')
  )

  # Simple test (less future-proof):
  # This approach works, but it directly depends on RNG behavior.
  # If RNG changes in future R versions, the test may fail unexpectedly.
  #
  # expect_equal(
  #   study %>% get_study_id(),
  #   'GCfFdZ1SzOf'
  # )
  
  # Mocked approach (more robust):
  # By mocking `generate_alphanumeric_id()`, we ensure the test remains stable
  # even if RNG behavior or defaults change in future R versions.
  with_mocked_bindings(
    generate_alphanumeric_id = function(...) "GCfFdZ1SzOf",
    {
      expect_equal(
        study %>% get_study_id(),
        "STUDY_GCfFdZ1SzOf"
      )
    }
  )

})

test_that("get_entity_abbreviation() is stable across invocations", {
  expect_no_error(
    study <- make_study(name = 'my cool study')
  )
  
  # Verify the entity names
  expect_equal(
    study %>% get_entity_names(),
    c("household", "participant", "observation")
  )
  
  # Get the abbreviations for the first invocation
  first_abbreviations <- sapply(
    study %>% get_entity_names(),
    function(name) get_entity_abbreviation(study, name)
  )
  
  # Ensure abbreviations are unique
  expect_equal(
    length(unique(first_abbreviations)),
    length(first_abbreviations)
  )
  
  # Get the abbreviations for the second invocation
  second_abbreviations <- sapply(
    study %>% get_entity_names(),
    function(name) get_entity_abbreviation(study, name)
  )
  
  # Ensure that the abbreviations are consistent across invocations
  expect_equal(
    first_abbreviations,
    second_abbreviations
  )
})


test_that("get_study_abbreviation() is stable across invocations", {
  expect_no_error(
    study <- make_study(name = 'my cool study')
  )
  
  first_abbreviation <- study %>% get_study_abbreviation()
  second_abbreviation <- study %>% get_study_abbreviation()
  
  expect_equal(
    first_abbreviation,
    second_abbreviation
  )
})


test_that("get_study_abbreviation() throws an error when there's no study_id (due to no name at the moment)", {
  expect_no_error(
    study <- make_study()
  )

  expect_error(  
    abbreviation <- study %>% get_study_abbreviation(),
    "not allowed to call get_study_abbreviation.+on a study with no study name"
  )
})

test_that("map_entities(infer_geo_variables_for_eda) is safe on a mixed study", {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')

  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }
  households <- entity_from_file(households_path, name = "household",
                                 preprocess_fn = add_geo_coords, quiet = TRUE) %>%
    verbose()
  participants <- entity_from_file(participants_path, name = "participant", quiet = TRUE) %>%
    redetect_columns_as_variables('Name') %>%
    set_parents(names = c("household"), id_columns = c("Household.Id")) %>%
    verbose()

  study <- study_from_entities(list(households, participants), name = 'mixed geo study')

  collect_messages <- function(expr) {
    messages <- character(0)
    value <- withCallingHandlers(
      expr,
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    list(value = value, messages = messages)
  }

  # The geo entity is not quiet, so it reports what it did, as it would on its own.
  # The non-geo entity must add nothing: no message and no warning.
  geo_alone <- collect_messages(households %>% infer_geo_variables_for_eda())
  expect_no_warning(
    mapped <- collect_messages(study %>% map_entities(infer_geo_variables_for_eda))
  )
  result <- mapped$value
  expect_identical(mapped$messages, geo_alone$messages)
  expect_false(any(grepl("No geographic coordinate", mapped$messages)))

  geo <- result %>% get_entity('household')
  expect_equal(sum(grepl("^geohash_", names(geo@data))), 6)

  non_geo_before <- study %>% get_entity('participant')
  non_geo_after  <- result %>% get_entity('participant')
  expect_identical(non_geo_after@data, non_geo_before@data)
  expect_identical(non_geo_after@variables, non_geo_before@variables)
})

test_that("map_entities(infer_geo_variables_for_eda) emits nothing on a non-geo study", {
  study <- make_study(quiet_entities = FALSE, name = 'no geo here')
  expect_silent(result <- study %>% map_entities(infer_geo_variables_for_eda))
  expect_identical(get_entities(result), get_entities(study))
})
