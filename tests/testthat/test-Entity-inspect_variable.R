# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect_variable(entity, variable_name) outputs categorical values in full", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path)
  # inspect it and grab the output
  output <- capture.output(inspect_variable(households, 'Construction.material'))
  # Make sure it contains a factor value longer than three characters
  expect_true(any(grepl("Concrete", output)))
  # TO DO nearer release time... (when things have settled)
  # Expect the output to match the stored snapshot
  #expect_snapshot_output(output) 
})
