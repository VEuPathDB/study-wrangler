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
  
  # Check that metadata matches column names
  expect_equal(colnames(result@data), result@metadata$variable)
})
