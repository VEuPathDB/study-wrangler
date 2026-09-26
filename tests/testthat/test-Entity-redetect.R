test_that("redetect_columns_as_variables() resets data_shape when a continuous column becomes a string", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = 'household', quiet = TRUE)

  before <- households %>% get_variable_metadata() %>%
    filter(variable == "Number.of.animals") %>% select(data_type, data_shape)
  expect_equal(as.character(before$data_type), "integer")
  expect_equal(as.character(before$data_shape), "continuous")

  households <- households %>%
    modify_data(mutate(Number.of.animals = as.character(Number.of.animals))) %>%
    redetect_columns_as_variables("Number.of.animals")

  after <- households %>% get_variable_metadata() %>%
    filter(variable == "Number.of.animals") %>% select(data_type, data_shape)
  expect_equal(as.character(after$data_type), "string")
  # data_shape must be re-inferred, not left stuck on the old "continuous"
  expect_equal(as.character(after$data_shape), "categorical")

  # a stale data_type "string" / data_shape "continuous" combination crashes
  # hydration (findBinWidth() has no method for character columns); this must
  # not error now that data_shape is re-inferred alongside data_type
  expect_no_error(households %>% get_hydrated_variable_and_category_metadata())
  expect_no_error(inspect_variable(households, "Number.of.animals"))
})

test_that("redetect_columns_as_variables() resets data_shape when a categorical column becomes numeric", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = 'household', quiet = TRUE) %>%
    modify_data(mutate(Number.of.animals = as.character(Number.of.animals))) %>%
    redetect_columns_as_variables("Number.of.animals")

  before <- households %>% get_variable_metadata() %>%
    filter(variable == "Number.of.animals") %>% select(data_type, data_shape)
  expect_equal(as.character(before$data_type), "string")
  expect_equal(as.character(before$data_shape), "categorical")

  households <- households %>%
    modify_data(mutate(Number.of.animals = as.numeric(Number.of.animals))) %>%
    redetect_columns_as_variables("Number.of.animals")

  after <- households %>% get_variable_metadata() %>%
    filter(variable == "Number.of.animals") %>% select(data_type, data_shape)
  expect_equal(as.character(after$data_type), "number")
  # data_shape must be re-inferred back to "continuous", not left on "categorical"
  expect_equal(as.character(after$data_shape), "continuous")

  expect_no_error(households %>% get_hydrated_variable_and_category_metadata())
  expect_no_error(inspect_variable(households, "Number.of.animals"))
})
