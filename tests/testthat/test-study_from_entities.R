test_that("study_from_entities() works", {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  observations_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  
  households <- entity_from_file(households_path, name="household")
  participants <- entity_from_file(participants_path, name="participant")
  observations <- entity_from_file(observations_path, name="observation")
  
  expect_true(households %>% quiet() %>% validate())
  
  expect_message(
    participants <- participants %>%
      redetect_columns_as_variables('Name'),
    "Redoing type detection"
  )
  expect_message(
    participants <- participants %>%
      set_parents(names=c("household"), columns=c("Household.Id")),
    "Parent entity relationships and columns have been set"
  )
  expect_true(participants %>% quiet() %>% validate())
  
  
  expect_message(
    observations <- observations %>%
      set_parents(names = c("participant", "household"), columns = c("Participant.Id", "Household.Id")),
    "Parent entity relationships and columns have been set"
  )
  expect_true(observations %>% quiet() %>% validate())
  
  # now build a study
  
  expect_silent(
    study <- study_from_entities(entities=c(households, participants, observations), name="my study")
  )
  
  expected_entities <- list(households, participants, observations)
  
  expect_equal(
    lapply(get_entities(study), get_entity_name),
    lapply(expected_entities, get_entity_name)
  )

  # test get_entity()
  expect_equal(
    study %>% get_entity('household') %>% remove_children(),
    households
  )
  expect_equal(
    study %>% get_entity('participant') %>% remove_children(),
    participants
  )
  expect_equal(
    study %>% get_entity('observation') %>% remove_children(),
    observations
  )
    
  # won't build study due to missing linking entity
  expect_error(
    study <- study_from_entities(entities=c(households, observations), name="my bad study"),
    "Tree does not include all provided entities. Check for disconnected or orphaned nodes."
  )

  oopsie <- observations %>% quiet() %>% set_entity_name('household')
  expect_error(
    study <- study_from_entities(entities=c(households, participants, oopsie), name="my study"),
    "Entities must all have unique.+entity_name.+there are duplicates"
  )
  
})

