# By default, skimr abbreviates all factor values to three characters.
# We have overridden that behaviour.
test_that("inspect_variable(entity, variable_name) outputs categorical values in full", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name = 'household')

  # inspect it and grab the output
  output <- capture.output(households %>% quiet() %>% inspect_variable('Construction.material'))

  # Make sure it contains a factor value longer than three characters
  expect_true(any(grepl("Concrete", output)))

  # Does it contain hydrated-only variables?
  hydrated_only_vars = c("precision", "vocabulary")
  expect_true(all(hydrated_only_vars %>% map_lgl(~ any(str_detect(output, .x)))))
  
})
