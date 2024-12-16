test_that("validate(study) does the right thing", {
  
  expect_silent(study <- make_study(name='my study'))
  
  expect_message(
    expect_true(
      validate(study)
    ),
    "Entity is valid"
  )
  
})

test_that("validate(study) fails for things", {
  
  expect_silent(study <- make_study())
  
  expect_message(
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
    study %>% set_quiet() %>% validate()
  )
  
})

# test the parent IDs are correct, row-wise
test_that("check_parent_ids() works", {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')

  households <- entity_from_file(households_path, name="household")
  
  participants <- entity_from_file(participants_path, name="participant", quiet=TRUE) %>%
    redo_type_detection_as_variables_only('Name') %>%
    set_parents(names=c("household"), columns=c("Household.Id")) %>% set_quiet(FALSE)
  
  # this should be silent
  expect_silent(
    check_result <- check_parent_ids(households, participants)
  )
  # and it should be valid
  expect_true(check_result$is_valid)
  
  # now mess up some of the parent IDs in participants
  bad_participants <- participants %>% set_data(
    mutate(Household.Id = if_else(row_number() %% 3 == 0, 'H007', Household.Id))
  )

  # this should be silent
  expect_silent(
    check_result <- check_parent_ids(households, bad_participants)
  )
  # and it should be invalid
  expect_false(check_result$is_valid)
  expect_equal(
    check_result$missing_mappings %>% nrow(),
    2
  )
    
})