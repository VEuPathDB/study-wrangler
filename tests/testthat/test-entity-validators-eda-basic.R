# Tests for eda_display_name_not_null validator
test_that("eda_display_name_not_null validator passes when all variables have display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('Number.of.animals', display_name = 'Number of animals') %>%
    set_variable_metadata('Owns.property', display_name = 'Owns property') %>%
    set_variable_metadata('Enrollment.date', display_name = 'Enrollment date') %>%
    set_variable_metadata('Construction.material', display_name = 'Construction material') %>%
    verbose()

  # Should pass EDA validation
  expect_true(households %>% quiet() %>% validate(profiles = "eda"))
})

test_that("eda_display_name_not_null validator fails when some variables missing display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('Number.of.animals', display_name = 'Number of animals') %>%
    set_variable_metadata('Owns.property', display_name = 'Owns property') %>%
    # Leave Enrollment.date and Construction.material without display_name
    verbose()

  # Should fail validation with specific error message
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Fatal issue encountered.*EDA validation requires display_name for all variable columns. Missing display_name for: Enrollment.date, Construction.material.*To set display_name for these variables, use:.*set_variable_metadata"
  )
  expect_false(is_valid)
})

test_that("eda_display_name_not_null validator fails when all variables missing display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household')

  # Should fail validation listing all variables
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Fatal issue encountered.*EDA validation requires display_name for all variable columns. Missing display_name for: Number.of.animals, Owns.property, Enrollment.date, Construction.material.*To set display_name for these variables, use:.*set_variable_metadata"
  )
  expect_false(is_valid)
})

test_that("eda_display_name_not_null validator passes when display_name set via set_variable_display_names_from_provider_labels", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('Number.of.animals', provider_label = list(c("Number of animals"))) %>%
    set_variable_metadata('Owns.property', provider_label = list(c("Owns property"))) %>%
    set_variable_metadata('Enrollment.date', provider_label = list(c("Enrollment date"))) %>%
    set_variable_metadata('Construction.material', provider_label = list(c("Construction material"))) %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should pass EDA validation after copying provider_label to display_name
  expect_true(households %>% quiet() %>% validate(profiles = "eda"))
})
