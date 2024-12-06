# tests/testthat/test-entity_from_file.R
test_that("entity_from_file works as expected", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  result <- entity_from_file(file_path)
  
  # Check the class
  expect_s4_class(result, "Entity")
  
  # Check that data is a tibble
  expect_s3_class(result@data, "tbl_df")
  
  # Check that the metadata is a tibble
  expect_s3_class(result@metadata, "tbl_df")

  # Check that metadata variable names match the data tibble column names
  expect_equal(colnames(result@data), result@metadata$variable)
})

test_that("entity_from_file warns about duplicate column names in input file", {
  # File with duplicate "Animal" column
  file_path <- testthat::test_path("fixtures/duplicateColumns.tsv")
  
  expect_warning(
    result <- entity_from_file(file_path),
    "Duplicate column names detected in input file."
  )
  
  expect_true(any(duplicated(result@metadata$provider_label)))

  expect_true(n_distinct(result@metadata$variable) == nrow(result@metadata))
})

test_that("entity_from_file detects column types correctly", {
  file_path <- testthat::test_path("fixtures/households.tsv")
  result <- entity_from_file(file_path)
  
  # Check metadata data_type
  expected_types <- c("id", "integer", "string", "date", "string")
  expect_equal(result@metadata$data_type, expected_types)
  
  # Check the data_shape has been inferred correctly
  expected_shapes <- c(NA, "continuous", "categorical", "continuous", "categorical")
  expect_equal(result@metadata$data_shape, expected_shapes)
})

test_that("entity_from_file detects invalid dates", {
  file_path <- testthat::test_path("fixtures/households.tsv")
  
  # Modify the data to introduce an invalid date
  modify_fn <- function(data) {
    data$`Enrollment date`[2] <- "2021-02-29" # Invalid date
    return(data)
  }
  
  expect_warning(
    result <- entity_from_file(file_path, preprocess_fn = modify_fn),
    "expected valid date, but got" # Expected error message from readr
  )
})




