# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect(entity) outputs categorical values in full", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path)
  # inspect it and grab the output
  expect_message(
    output <- capture.output(inspect(households)),
    "Warning: because this entity has no `name` .required., a placeholder entity ID has been generated."
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
