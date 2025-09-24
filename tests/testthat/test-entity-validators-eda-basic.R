# Tests for eda_display_name_not_null validator
test_that("eda_display_name_not_null validator passes when all variables have display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('HHID', display_name = 'Household ID') %>%
    set_variable_metadata('VILLAGE_NAME', display_name = 'Village Name') %>%
    set_variable_metadata('OWNERSHIP', display_name = 'Ownership') %>%
    set_variable_metadata('ROOMS', display_name = 'Number of Rooms') %>%
    verbose()

  # Should pass EDA validation
  expect_true(households %>% quiet() %>% validate(profiles = "eda"))
})

test_that("eda_display_name_not_null validator fails when some variables missing display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('HHID', display_name = 'Household ID') %>%
    set_variable_metadata('VILLAGE_NAME', display_name = 'Village Name') %>%
    # Leave OWNERSHIP and ROOMS without display_name
    verbose()

  # Should fail validation with specific error message
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Validation issues found.*EDA validation requires display_name for all variables. Missing display_name for: OWNERSHIP, ROOMS"
  )
  expect_false(is_valid)
})

test_that("eda_display_name_not_null validator fails when all variables missing display_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household')

  # Should fail validation listing all variables
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Validation issues found.*EDA validation requires display_name for all variables. Missing display_name for: HHID, VILLAGE_NAME, OWNERSHIP, ROOMS"
  )
  expect_false(is_valid)
})

test_that("eda_display_name_not_null validator passes when display_name set via set_variable_display_names_from_provider_labels", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('HHID', provider_label = list(c("Household ID"))) %>%
    set_variable_metadata('VILLAGE_NAME', provider_label = list(c("Village Name"))) %>%
    set_variable_metadata('OWNERSHIP', provider_label = list(c("Ownership Status"))) %>%
    set_variable_metadata('ROOMS', provider_label = list(c("Number of Rooms"))) %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should pass EDA validation after copying provider_label to display_name
  expect_true(households %>% quiet() %>% validate(profiles = "eda"))
})
