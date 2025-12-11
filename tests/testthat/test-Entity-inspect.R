# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect(entity) outputs categorical values in full", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path)
  # inspect it and grab the output
  message_without_dupes$reset()
  
  # the new hydrated metadata cache voids this:
  # expect_message(
  output <- capture.output(inspect(households))
  #   "Warning: because this entity has no `name` .required., a placeholder entity ID has been generated."
  # )

  # Make sure it contains a factor value longer than three characters
  expect_true(any(grepl("Concrete", output)))
  # TO DO nearer release time... (when things have settled)
  # Expect the output to match the stored snapshot
  #expect_snapshot_output(output) 
})

test_that("inspect(entity) counts annotations properly", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')
  
  expect_message(
    output <- capture.output(inspect(households)),
    "Generating temporary stable_id for entity"
  )
  
  # expect 4 variables with no annotations
  expect_true(any(grepl('Total number of variables\\s*\\b4\\b', output, perl=TRUE)))
  expect_true(any(grepl('display_name provided\\*\\s*\\b0\\b', output, perl=TRUE)))
  expect_true(any(grepl('definition provided\\s*\\b0\\b', output, perl=TRUE)))

  expect_message(
    expect_message(
      households <- households %>%
        set_variable_metadata('Owns.property',
                              display_name="Owns property",
                              definition="The occupants own the property outright or with a mortgage") %>%
        set_variable_metadata('Enrollment.date',
                              definition="The date on which the household was enrolled in the study"),
      "Made metadata update"
    ),
    "Made metadata update"
  )
  
  # let's see if these show up
  output <- capture.output(inspect(households))
  expect_true(any(grepl('display_name provided\\*\\s*\\b1\\b', output, perl=TRUE)))
  expect_true(any(grepl('definition provided\\s*\\b2\\b', output, perl=TRUE)))

})

test_that("Collections are shown in inspect() properly", {
  study <- make_study_with_collections(name = "collections study")

  observations <- study %>% get_entity('observation')

  message_without_dupes$reset()

  expect_message(
    output <- capture.output(inspect(observations)),
    "Generating temporary stable_id for entity"
  )

  expect_true(any(grepl('stable_id\\s*COL_', output, perl=TRUE)))
  expect_true(any(grepl('category\\s*integer.measures', output, perl=TRUE)))

})

test_that("inspect() handles entities with problematic continuous variables for bin width computation", {
  # Create an entity with continuous variables that might cause findBinWidth() to fail
  # Test cases: all identical values, very small range, etc.
  test_data <- tibble(
    id = 1:10,
    constant_value = rep(5.0, 10),  # All identical - may cause bin width issues
    near_constant = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.00001),  # Nearly identical
    normal_var = seq(1, 10, length.out = 10),  # Normal variable for comparison
    all_na = rep(NA_real_, 10)  # All missing values
  )

  # Create a temporary file with this data
  temp_file <- tempfile(fileext = ".tsv")
  write.table(test_data, temp_file, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create entity from file
  entity <- entity_from_file(temp_file, name = "test")

  # Clean up temp file
  unlink(temp_file)

  # The inspect function should not throw an error even with problematic variables
  # This is the key test - previously this would fail with:
  # "Error in if (x == 0) ...: missing value where TRUE/FALSE needed"
  expect_no_error({
    output <- capture.output(inspect(entity))
  })

  # Verify the output contains expected sections
  expect_true(any(grepl("Summary of variable values", output)))
  expect_true(length(output) > 0)
})

test_that("inspect() handles entities with very large numeric values", {
  # Test with very large numeric values (hundreds of billions)
  # These values caused: "Error in if (dx == 0) ...: missing value where TRUE/FALSE needed"
  file_path <- system.file("extdata", "test_large_values.tsv", package = 'study.wrangler')

  # Create entity from file
  entity <- entity_from_file(file_path, name = "test_large_values")

  # This should not throw an error
  expect_no_error({
    output <- capture.output(inspect(entity))
  })

  # Verify the output contains expected sections
  expect_true(any(grepl("Summary of variable values", output)))
  expect_true(length(output) > 0)
})
