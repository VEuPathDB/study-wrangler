# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect(entity) outputs categorical values in full", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path)
  # inspect it and grab the output
  output <- capture.output(inspect(households))
  # Make sure it contains a factor value longer than three characters
  expect_true(any(grepl("Concrete", output)))
  # TO DO nearer release time... (when things have settled)
  # Expect the output to match the stored snapshot
  #expect_snapshot_output(output) 
})
