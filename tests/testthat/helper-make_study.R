make_study <- function(...) {
  metadata <- list(...)
  validate_object_metadata_names('Study', metadata)
  
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  observations_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  
  # quiet entities suppress output
  households <- entity_from_file(households_path, name="household", quiet=TRUE)

  participants <- entity_from_file(participants_path, name="participant", quiet=TRUE) %>%
    redetect_columns_as_variables('Name') %>%
    set_parents(names=c("household"), columns=c("Household.Id"))

  observations <- entity_from_file(observations_path, name="observation", quiet=TRUE) %>%
    set_parents(
      names=c("participant","household"), 
      columns=c("Participant.Id","Household.Id")
    )

  # call study_from_entities with 'spread' metadata 
  args = c(list(entities = c(households, participants, observations)), metadata)
  study <- do.call(study_from_entities, args)
  return(study)
}
