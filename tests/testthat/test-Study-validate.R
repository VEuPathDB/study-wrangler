test_that("validate(study) does the right thing", {
  
  expect_silent(study <- make_study(name='my study'))
  
  expect_message(
    expect_true(
      validate(study)
    ),
    "Study and its 3 entities are valid"
  )
  
})

test_that("validate(study) fails with an invalid entity", {
  
  expect_silent(study <- make_study(name='my study'))
  
  study@root_entity <- study@root_entity %>% redetect_columns_as_variables('Household.Id')
  
  expect_warning(
    expect_warning(
      expect_false(
        validate(study)
      ),
      "The entity named 'household' is not valid.+Please run.+validate"
    ),
    "Error: one or more entities is invalid"
  )
  
})

test_that("validate(study) fails for things", {
  
  expect_silent(study <- make_study())
  
  expect_warning(
    expect_false(
      validate(study)
    ),
    "Study name is missing"
  )
  
  # oh noes, let's add a name
  expect_message(
    study <- study %>% set_study_name('Brian'),
    "Adding study name.+Brian"
  )

  expect_true(
    study %>% quiet() %>% validate()
  )
  
})

# test the parent IDs are correct, row-wise
test_that("check_parent_ids() works", {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')

  households <- entity_from_file(households_path, name="household")
  
  participants <- entity_from_file(participants_path, name="participant", quiet=TRUE) %>%
    redetect_columns_as_variables('Name') %>%
    set_parents(names=c("household"), id_columns=c("Household.Id")) %>% verbose()
  
  # this should be silent
  expect_silent(
    check_result <- check_parent_child_join(households, participants)
  )
  # and it should be valid
  expect_true(check_result$is_valid)
  
  # now mess up some of the parent IDs in participants
  bad_participants <- participants %>% modify_data(
    mutate(Household.Id = if_else(row_number() %% 3 == 0, 'H007', Household.Id))
  )

  # this should be silent
  expect_silent(
    check_result <- check_parent_child_join(households, bad_participants)
  )
  # and it should be invalid
  expect_false(check_result$is_valid)
  expect_equal(
    check_result$missing_mappings %>% nrow(),
    2
  )
    
})

# now test the whole study version (in validate())
test_that("validate(study) checks parent ID relationships row-wise", {
  
  expect_no_error(study <- make_study(name = 'cool study'))
  
  expect_true(
    study %>% quiet() %>% validate()
  )
  entities <- get_entities(study)
  # library(zeallot) opportunity:
  households <- entities[[1]]
  participants <- entities[[2]]
  observations <- entities[[3]]

  expect_no_error(
    study <- study_from_entities(entities=list(households, participants, observations), name = 'should be the same')
  )
  expect_true(
    study %>% quiet() %>% validate()
  )
  
  bad_participants <- participants %>%
    modify_data(
      mutate(
        Household.Id = if_else(row_number() %% 3 == 0, 'H007', Household.Id)
      )
    )
  
  expect_silent(
    bad_study <- study_from_entities(entities=list(households, bad_participants, observations), name = 'a bad one')
  )
  expect_warning(
    validate(bad_study),
    "relationships are problematic in the following pairs.+household.+participant"
  )
})


# Tests for study-level entity stable_id uniqueness validator (EDA profile)
test_that("study entity stable_ids validator only runs in eda profile", {
  study <- make_study(name = 'test study')
  entities <- get_entities(study)

  # Create duplicates
  entities[[1]] <- entities[[1]] %>% quiet() %>% set_stable_id('ENT_dup')
  entities[[2]] <- entities[[2]] %>% quiet() %>% set_stable_id('ENT_dup')
  study <- study_from_entities(entities = entities, name = 'test', quiet = TRUE)

  # Should pass baseline (validator not registered there)
  expect_true(study %>% quiet() %>% validate(profiles = "baseline"))

  # Note: Can't test eda profile here because the eda_variable_display_name_not_null
  # validator is fatal and runs before the entity stable_id validator
})

test_that("study entity stable_ids validator with duplicates shows in list_validators", {
  # Check that the validator is registered to eda profile
  validators <- list_validators()

  eda_study_validators <- validators %>%
    filter(grepl("eda", profiles), grepl("study", object_type))

  expect_true("study_unique_entity_stable_ids" %in% eda_study_validators$name)
})

test_that("study entity stable_ids validator logic works correctly", {
  # Test the validator function directly to avoid EDA display_name issues
  study <- make_study(name = 'test study')
  entities <- get_entities(study)

  # Set two entities to have the same stable_id
  entities[[1]] <- entities[[1]] %>% quiet() %>% set_stable_id('ENT_duplicate')
  entities[[2]] <- entities[[2]] %>% quiet() %>% set_stable_id('ENT_duplicate')

  # Recreate study with modified entities
  study <- study_from_entities(entities = entities, name = 'test study', quiet = TRUE)

  # Call the validator function directly
  result <- validate_study_unique_entity_stable_ids(study)

  expect_false(result$valid)
  expect_false(result$fatal %||% FALSE)
  expect_match(result$message, "Duplicate stable_ids detected")
  expect_match(result$message, "ENT_duplicate")
})

test_that("study entity stable_ids validator passes with unique IDs", {
  study <- make_study(name = 'test study')

  # Call the validator function directly
  result <- validate_study_unique_entity_stable_ids(study)

  expect_true(result$valid)
})

test_that("study entity stable_ids validator remediation works with helper", {
  study <- make_study(name = 'test study')
  entities <- get_entities(study)

  # Create duplicates
  entities[[1]] <- entities[[1]] %>% quiet() %>% set_stable_id('ENT_dup')
  entities[[2]] <- entities[[2]] %>% quiet() %>% set_stable_id('ENT_dup')
  study <- study_from_entities(entities = entities, name = 'test', quiet = TRUE)

  # Should fail
  result <- validate_study_unique_entity_stable_ids(study)
  expect_false(result$valid)

  # Fix using helper
  entity_names <- get_entity_names(study)[1:2]
  study <- study %>%
    quiet() %>%
    set_entity_stable_ids(entity_names, c('ENT_unique1', 'ENT_unique2'))

  # Should now pass
  result <- validate_study_unique_entity_stable_ids(study)
  expect_true(result$valid)
})


