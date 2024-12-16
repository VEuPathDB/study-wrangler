make_study <- function() {
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  observations_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  
  # quiet entities suppress output
  households <- entity_from_file(households_path, name="household", quiet=TRUE)

  participants <- entity_from_file(participants_path, name="participant", quiet=TRUE) %>%
    redo_type_detection_as_variables_only('Name') %>%
    set_parents(names=c("household"), columns=c("Household.Id"))

  observations <- entity_from_file(observations_path, name="observation", quiet=TRUE) %>%
    set_parents(
      names=c("participant","household"), 
      columns=c("Participant.Id","Household.Id")
    )

  study <- study_from_entities(entities = c(households, participants, observations), name = "my study")
  return(study)
}
