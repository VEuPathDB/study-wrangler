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
      redetect_columns_as_variables('Name'),
    "Redoing type detection"
  )

  # and now it should validate
  expect_true(participants %>% quiet() %>% validate())
})

test_that("set_parents() works and the result validates", {
  file_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  participants <- entity_from_file(file_path, name="participant")

  expect_message(
    expect_false(validate(participants)),
    "There are multiple ID columns per entity level.+Name"
  )
  
  expect_message(
    participants <- participants %>% redetect_columns_as_variables(columns = c('Name')),
    "Redoing type detection"
  )

  # it should now validate  
  expect_true(participants %>% quiet() %>% validate())

  # but Household.Id is still a regular variable
  output <- capture.output(participants %>% quiet() %>% inspect())
  expect_true(any(grepl("Total number of variables\\s+5\\b", output)))
  expect_true(any(grepl("Household.Id\\s+string\\s+categorical\\b", output)))
  
  expect_message(
    participants <- participants %>%
      set_parents(names=c("household"), columns=c("Household.Id")),
    "Parent entity relationships and columns have been set"
  )

  # Household.Id is no longer a regular variable
  output <- capture.output(participants %>% quiet() %>% inspect())
  expect_true(any(grepl("Total number of variables\\s+4\\b", output)))
  expect_false(any(grepl("Household.Id\\s+string\\s+categorical\\b", output)))
  
  expect_true(participants %>% quiet() %>% validate())
})


test_that("participant_observations fixture loads and validates", {

  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name="observation")
  
  expect_output(
    observations %>% quiet() %>% inspect(),
    "Total number of variables\\s+7\\b"
  )

  # get_parent_name() should return NULL silently
  expect_silent(
    expect_null(
      observations %>% get_parent_name()
    )
  )
  
  parent_names = c("participant", "household")
  parent_columns = c("Participant.Id", "Household.Id")
  
  expect_message(
    observations <- observations %>%
      set_parents(names = parent_names, columns = parent_columns),
    "Parent entity relationships and columns have been set"
  )
  
  expect_output(
    observations %>% quiet() %>% inspect(),
    "Total number of variables\\s+5\\b"
  )
  
  expect_true(observations %>% quiet() %>% validate())

  # check that `get_parents()` returns the same.
  expect_silent(
    parents <- observations %>% get_parents()
  )
  expect_equal(parents, list(names = parent_names, columns = parent_columns))
  
  # test bad args to set_parents()
  parent_names_empty <- character(0)
  parent_columns_empty <- character(0)
  expect_message(
    observations <- observations %>%
      set_parents(names = parent_names_empty, columns = parent_columns_empty),
    "No parent entity relationships provided. No changes made."
  )
  
  expect_error(
    observations <- observations %>%
      set_parents(names = c("participant", "household"), columns = c("Participant.Id")),
    "Error: 'names' and 'columns' must have the same length."
  )
  
  expect_error(
    observations <- observations %>%
      set_parents(names = c("participant", "household"), columns = c("NonexistentColumn", "AnotherFakeColumn")),
    "Error: the following data columns do not exist in this entity: NonexistentColumn, AnotherFakeColumn"
  )
  
  # get_parent_name()
  expect_equal(
    observations %>% get_parent_name(),
    'participant'
  )
})

