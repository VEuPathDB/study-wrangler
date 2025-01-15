test_that("set_entity_metadata() works", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
    
  households <- entity_from_file(file_path)

  expect_equal(households@name, NA_character_)

  expect_message(
    expect_message(
      households <- households %>%
        set_entity_metadata(name = 'household'),
      "Note: added default display_name,"
    ),
    "Note: added default display_name_plural,"
  )
  
  expect_equal(households@name, 'household')
  expect_equal(households@display_name, 'household')
  expect_equal(households@display_name_plural, 'households')
})

test_that("set_variable_display_names_from_provider_labels() works", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  households <- entity_from_file(file_path, name='household')
  
  expect_message(
    households <- households %>% set_variable_display_names_from_provider_labels(),
    "Copied provider_label over to display_name for 4 variables"
  )
  # should have no effect second time
  expect_message(
    households <- households %>% set_variable_display_names_from_provider_labels(),
    "Copied provider_label over to display_name for 0 variables"
  )
})

test_that("get_stable_id() and set_stable_id() work", {
  # Load the example entity
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = 'household')
  
  # Check if a stable_id is generated when none is set
  message_without_dupes$reset()
  expect_message(
    generated_id <- households %>% get_stable_id(),
    "Generating temporary stable_id for entity 'household'"
  )
  
  # Check that the generated ID starts with the correct prefix and matches the expected pattern
  expect_true(
    grepl("^ENT_", generated_id),
    info = "Generated ID should start with the prefix 'ENT_'"
  )
  expect_true(
    grepl("^ENT_[a-zA-Z0-9]{8}$", generated_id),
    info = "Generated ID should follow the pattern 'ENT_' followed by 8 alphanumeric characters"
  )
  
  # Set a custom stable_id and check if it's returned correctly
  a_stable_id <- 'ENTITY_01234'
  households <- households %>% quiet() %>% set_stable_id(a_stable_id)
  
  expect_equal(
    households %>% get_stable_id(),
    a_stable_id,
    info = "The set stable_id should be returned unchanged"
  )
  
  # Ensure no message is shown when a stable_id is already set
  expect_silent(
    households %>% get_stable_id()
  )
})
