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
        "GCfFdZ1SzOf"
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
