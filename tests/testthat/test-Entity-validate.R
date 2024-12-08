test_that("validate(households) returns TRUE for original households fixture data", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path)
  # validate it
  is_valid <- validate(households, quiet=TRUE)
  expect_true(is_valid)
})

test_that("validate() fails and warns about missing metadata", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path)
  # add a new data column
  households@data <-
    households@data %>%
      mutate(newColumn = 42)
    
  # validate
  expect_message(is_valid <- validate(households), "Variable metadata is missing for these data columns: newColumn")
  expect_false(is_valid)
  
  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced variables metadata by adding defaults for: newColumn"
  )
  
  # now it should be valid
  expect_true(validate(households, quiet=TRUE))
})

test_that("validate() fails and warns about extra metadata", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path)
  # remove a new data column
  households@data <-
    households@data %>%
    select(-c('Owns.property'))
  
  # validate
  expect_message(is_valid <- validate(households), "These variables have metadata but no data columns: Owns.property")
  expect_false(is_valid)

  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced metadata by removing these variables with no data: Owns.property"
  )
  
  # now it should be valid
  expect_true(validate(households, quiet=TRUE))
})
