# Tests for unique stable_ids validator
test_that("unique stable_ids validator passes when all stable_ids are unique", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet()

  # Should pass validation - auto-generated stable_ids should be unique
  expect_true(households %>% validate(profiles = "baseline"))
})

test_that("unique stable_ids validator fails when duplicate stable_ids exist", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    # Manually set the same stable_id for two different variables
    set_variable_metadata('Number.of.animals', stable_id = 'VAR_duplicate') %>%
    set_variable_metadata('Owns.property', stable_id = 'VAR_duplicate') %>%
    verbose()

  # Should fail validation with specific error message
  expect_warning(
    is_valid <- validate(households, profiles = "baseline"),
    "Duplicate stable_ids detected.*VAR_duplicate.*Number.of.animals.*Owns.property"
  )
  expect_false(is_valid)
})

test_that("unique stable_ids validator catches duplicates in categories", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    # Create two categories with the same stable_id
    create_variable_category("category1", c("Number.of.animals"), stable_id = 'VAR_catdup') %>%
    create_variable_category("category2", c("Owns.property"), stable_id = 'VAR_catdup') %>%
    verbose()

  # Should fail validation with error about duplicate category stable_ids
  expect_warning(
    is_valid <- validate(households, profiles = "baseline"),
    "Duplicate stable_ids detected.*VAR_catdup.*category1.*category2"
  )
  expect_false(is_valid)
})

test_that("unique stable_ids validator catches duplicates between variables and categories", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    # Set variable stable_id
    set_variable_metadata('Number.of.animals', stable_id = 'VAR_shared') %>%
    # Create category with same stable_id
    create_variable_category("category1", c("Owns.property"), stable_id = 'VAR_shared') %>%
    verbose()

  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "baseline"),
    "Duplicate stable_ids detected.*VAR_shared"
  )
  expect_false(is_valid)
})

test_that("unique stable_ids validator passes when duplicates are fixed", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Test with duplicates - should fail
  households_with_dupes <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('Number.of.animals', stable_id = 'VAR_duplicate') %>%
    set_variable_metadata('Owns.property', stable_id = 'VAR_duplicate') %>%
    verbose()

  expect_warning(
    is_valid_dupes <- validate(households_with_dupes, profiles = "baseline"),
    "Duplicate stable_ids detected"
  )
  expect_false(is_valid_dupes)

  # Test with unique stable_ids - should pass
  households_fixed <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>%
    set_variable_metadata('Number.of.animals', stable_id = 'VAR_unique1') %>%
    set_variable_metadata('Owns.property', stable_id = 'VAR_unique2')

  expect_true(validate(households_fixed, profiles = "baseline"))
})
