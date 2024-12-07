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
  expect_message(is_valid <- validate(households), "Data columns missing in variables' metadata: newColumn")
  expect_false(is_valid)
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
  expect_message(is_valid <- validate(households), "Variables' metadata rows missing in data columns: Owns.property")
  expect_false(is_valid)
})
