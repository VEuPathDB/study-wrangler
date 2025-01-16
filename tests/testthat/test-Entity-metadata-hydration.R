test_that("get_hydrated_variable_metadata(entity) works", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  households <- study %>% get_root_entity()
  participants <- study %>% get_entity('participant')
  observations <- study %>% get_entity('observation')
  
  # Test that hydrated variable metadata runs without errors or messages
  expect_no_error(
    expect_no_message(
      {
        households_hvmd <- households %>% get_hydrated_variable_metadata()
        participants_hvmd <- participants %>% get_hydrated_variable_metadata()
        observations_hvmd <- observations %>% get_hydrated_variable_metadata()
      }
    )
  )
  
  # List of expected hydrated columns
  hydrated_columns <- c(
    "vocabulary", "precision", "distinct_values_count", "range_min", 
    "range_max", "bin_width_computed", "lower_quartile", "median", 
    "upper_quartile", "mean"
  )
  
  # Test that hydrated columns are present in all hydrated tibbles
  for (hvmd in list(households_hvmd, participants_hvmd, observations_hvmd)) {
    expect_true(all(hydrated_columns %in% names(hvmd)))
  }
  
  # Test that stable_id and parent_stable_id are NA in regular metadata
  for (entity in list(households, participants, observations)) {
    regular_metadata <- entity %>% get_variable_metadata()
    expect_true(all(is.na(regular_metadata$stable_id)))
    expect_true(all(is.na(regular_metadata$parent_stable_id)))
  }
  
  # Test that stable_id and parent_stable_id are truthy in hydrated metadata
  for (hvmd in list(households_hvmd, participants_hvmd, observations_hvmd)) {
    expect_true(all(!is.na(hvmd$stable_id)))
    expect_true(all(!is.na(hvmd$parent_stable_id)))
    expect_true(is.character(hvmd$stable_id))
    expect_true(is.character(hvmd$parent_stable_id))
    expect_true(is.list(hvmd$vocabulary))
    expect_true(is.integer(hvmd$precision))
  }
})
