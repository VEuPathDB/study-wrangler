test_that("participants fixture loads and fails validation due to wrongly infered ID column", {
  file_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  
    expect_no_error(
    participants <- entity_from_file(file_path, name="participant")
  )
  expect_message(
    validate(participants),
    "There are multiple ID columns per entity level.+Participant.Id, Name"
  )
  
  # now we fix it
  expect_message(
    participants <- participants %>%
      redo_type_detection_as_variables_only('Name'),
    "Redoing type detection"
  )

  # and now it should validate
  expect_true(validate(participants, quiet=TRUE))
})

test_that("set_parents() works and the result validates", {
  file_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  participants <- entity_from_file(file_path, name="participant")

  expect_message(
    expect_false(validate(participants)),
    "There are multiple ID columns per entity level.+Name"
  )
  
  expect_message(
    participants <- participants %>% redo_type_detection_as_variables_only(columns = c('Name')),
    "Redoing type detection"
  )

  # it should now validate  
  expect_true(validate(participants, quiet=TRUE))
  
  # but Household.Id is still a regular variable
  output <- capture.output(inspect(participants))
  expect_true(any(grepl("Total number of variables\\s+5\\b", output)))
  expect_true(any(grepl("Household.Id\\s+string\\s+categorical\\b", output)))
  
  expect_message(
    participants <- participants %>%
      set_parents(names=c("household"), columns=c("Household.Id")),
    "Parent entity relationships and columns have been set"
  )

  # Household.Id is no longer a regular variable
  output <- capture.output(inspect(participants))
  expect_true(any(grepl("Total number of variables\\s+4\\b", output)))
  expect_false(any(grepl("Household.Id\\s+string\\s+categorical\\b", output)))
  
  expect_true(validate(participants, quiet=TRUE))
})


test_that("participant_observations fixture loads and validates", {

  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name="observation")
  
  expect_output(
    inspect(observations),
    "Total number of variables\\s+7\\b"
  )

  expect_message(
    observations <- observations %>%
      set_parents(names=c("participant", "household"), columns=c("Participant.Id", "Household.Id")),
    "Parent entity relationships and columns have been set"
  )
  
  expect_output(
    inspect(observations),
    "Total number of variables\\s+5\\b"
  )
  
  expect_true(validate(observations, quiet=TRUE))
  
})

