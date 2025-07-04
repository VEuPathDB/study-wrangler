test_that("entity_from_file works as expected", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  expect_silent(
    result <- entity_from_file(file_path)
  )
  
  # Check the class
  expect_s4_class(result, "Entity")
  
  # Check that data is a tibble
  expect_s3_class(result@data, "tbl_df")
  
  # Check that the metadata is a tibble
  expect_s3_class(result@variables, "tbl_df")

  # Check that metadata variable names match the data tibble column names
  expect_equal(colnames(result@data), result@variables$variable)
})

test_that("entity_from_file warns about duplicate column names in input file", {
  # File with duplicate "Animal" column
  file_path <- system.file("extdata", "toy_example/duplicateColumns.tsv", package = 'study.wrangler')

  expect_warning(
    result <- entity_from_file(file_path),
    "Duplicate column names detected in input."
  )
  
  expect_true(any(duplicated(result@variables$provider_label)))

  expect_true(n_distinct(result@variables$variable) == nrow(result@variables))
  
  message_without_dupes$reset()
  expect_no_error(
    expect_message(
      expect_output(
        inspect(result),
        "Entity-level metadata"
      ),
      "Warning: because this entity has no `name` .required., a placeholder entity ID has been generated."
    )
  )
})

test_that("entity_from_file detects column types correctly", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  expect_silent(
    result <- entity_from_file(file_path)
  )
  
  # Check metadata data_type
  expected_types <- c("id", "integer", "string", "date", "string")
  expect_equal(as.vector(result@variables$data_type), expected_types)
  
  # Check the data_shape has been inferred correctly
  expected_shapes <- c(NA, "continuous", "categorical", "continuous", "categorical")
  expect_equal(as.vector(result@variables$data_shape), expected_shapes)
})

test_that("entity_from_file detects invalid dates", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Modify the data to introduce an invalid date
  spoof_bad_date <- function(data) {
    data$`Enrollment date`[2] <- "2021-02-29" # Invalid date
    return(data)
  }
  
  expect_warning(
    result <- entity_from_file(file_path, name = 'household', preprocess_fn = spoof_bad_date),
    "expected valid date, but got" # Expected error message from readr
  )
  # the Enrollment.date column should still be character type
  expect_true(
    result %>% get_data() %>% pull(Enrollment.date) %>% is.character()
  )
  # and should contain no NAs
  expect_false(
    result %>% get_data() %>% pull(Enrollment.date) %>% is.na() %>% any()
  )
  
  expect_true(
    result %>% quiet() %>% redetect_columns_as_variables('Enrollment.date') %>% validate()
  )
  
})

test_that("entity_from_file rejects unknown ... metadata arguments", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  expect_error(
    result <- entity_from_file(file_path, does_not_exist = "at all"),
    "not valid Entity metadata names"
  )  
})

test_that("entity_from_file sets metadata from ... args", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  expect_silent(
    result <- entity_from_file(file_path, name = "household")
  )
  
  expect_equal(result@name, 'household')
})

test_that("entity_from_file reports problems from read_tsv()", {
  
  file_path <- system.file("extdata", "toy_example/brokenHouseholds.tsv", package = 'study.wrangler')
  
  expect_error(
    result <- entity_from_file(file_path, name = "household"),
    "Issues were encountered while parsing the file.+5 columns.+6 columns"
  )
})

