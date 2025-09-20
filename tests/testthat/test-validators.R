test_that("validator system works correctly", {
  # Test list_validators function
  validators <- list_validators()
  expect_s3_class(validators, "tbl_df")  # Should be a tibble
  expect_true(all(c("name", "object_type", "profiles", "description", "stop_on_error", "registration_order") %in% names(validators)))
  
  # Should have some baseline validators
  expect_gt(nrow(validators), 0)
  expect_true(any(grepl("baseline", validators$profiles)))
  
  # Should have both entity and study validators
  expect_true(any(grepl("entity", validators$object_type)))
  expect_true(any(grepl("study", validators$object_type)))
  
  # Should have some stop_on_error validators
  expect_true(any(validators$stop_on_error))
  
  # Registration order should be numeric and unique
  expect_true(is.numeric(validators$registration_order))
  expect_equal(length(unique(validators$registration_order)), nrow(validators))
})

test_that("get_validators_for_profiles works correctly", {
  # Get entity validators for baseline profile
  entity_validators <- get_validators_for_profiles("baseline", "entity")
  expect_true(length(entity_validators) > 0)
  expect_true(all(sapply(entity_validators, function(v) is.function(v$func))))
  expect_true(all(sapply(entity_validators, function(v) "stop_on_error" %in% names(v))))
  
  # Get study validators for baseline profile
  study_validators <- get_validators_for_profiles("baseline", "study")
  expect_true(length(study_validators) > 0)
  expect_true(all(sapply(study_validators, function(v) is.function(v$func))))
  expect_true(all(sapply(study_validators, function(v) "stop_on_error" %in% names(v))))
  
  # Get validators for non-existent profile
  empty_validators <- get_validators_for_profiles("nonexistent", "entity")
  expect_equal(length(empty_validators), 0)
})

test_that("validator functions return proper structure", {
  # Test that we can access a validator function
  entity_validators <- get_validators_for_profiles("baseline", "entity")
  
  # Should have at least the metadata validator
  expect_true(length(entity_validators) > 0)
  
  # Each validator should be a metadata object with a function
  for (validator_meta in entity_validators) {
    expect_true(is.function(validator_meta$func))
    expect_true("stop_on_error" %in% names(validator_meta))
    expect_true("registration_order" %in% names(validator_meta))
  }
})