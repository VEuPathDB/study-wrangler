test_that("set_entity_metadata() works", {
  file_path <- testthat::test_path("fixtures/households.tsv")
    
  households <- entity_from_file(file_path)

  expect_equal(households@name, NA_character_)
  
  households <- households %>%
    set_entity_metadata(name = 'household')
  
  expect_equal(households@name, 'household')
  # also expect the default setting of `display_name`
  # and `display_name_plural` as in the `entity()` constructor
  expect_equal(households@display_name, 'household')
  expect_equal(households@display_name_plural, 'households')
})
