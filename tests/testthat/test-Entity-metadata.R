test_that("set_entity_metadata() works", {
  file_path <- testthat::test_path("fixtures/households.tsv")
    
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
  file_path <- testthat::test_path("fixtures/households.tsv")
  
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
