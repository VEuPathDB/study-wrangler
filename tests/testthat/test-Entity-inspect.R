# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect(entity) outputs categorical values in full", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name = 'household')
  # inspect it and grab the output
  message_without_dupes$reset()
  
  expect_message(
    output <- capture.output(inspect(households)),
    "Generating temporary stable_id for entity"
  )

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
  
  output <- capture.output(inspect(households))

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

test_that("inspect() respects max_variables parameter", {
  # Create entity with 150 variables
  data <- tibble(id = 1:10)
  for (i in 1:150) {
    data[[paste0("var_", sprintf("%03d", i))]] <- rnorm(10)
  }

  entity <- entity_from_tibble(data, name = "test_wide", skip_type_convert = TRUE) %>%
    quiet() %>%
    redetect_column_as_id("id")

  # Test with default limit (100)
  output_default <- capture.output(inspect(entity))
  expect_true(any(grepl("WARNING.*100 of 150", output_default)))

  # Test with Inf (no limit)
  output_unlimited <- capture.output(inspect(entity, max_variables = Inf))
  expect_false(any(grepl("WARNING", output_unlimited)))
})
