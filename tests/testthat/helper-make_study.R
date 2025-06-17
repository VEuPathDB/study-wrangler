make_study <- function(quiet_entities = TRUE, ...) {
  metadata <- list(...)
  validate_object_metadata_names('Study', metadata)
  
  households_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  participants_path <- system.file("extdata", "toy_example/participants.tsv", package = 'study.wrangler')
  observations_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  
  # quiet entities suppress output
  households <- entity_from_file(households_path, name="household", quiet=quiet_entities)

  participants <- entity_from_file(participants_path, name="participant", quiet=TRUE) %>%
    redetect_columns_as_variables('Name') %>%
    set_parents(names=c("household"), id_columns=c("Household.Id")) %>%
    set_quiet(quiet_entities)

  observations <- entity_from_file(observations_path, name="observation", quiet=TRUE) %>%
    set_parents(
      names=c("participant","household"), 
      id_columns=c("Participant.Id","Household.Id")
    ) %>%
    set_quiet(quiet_entities)

  # call study_from_entities with 'spread' metadata 
  args = c(list(entities = c(households, participants, observations)), metadata)
  study <- do.call(study_from_entities, args)
  return(study)
}

#' makes a study and munges the ID column names to be the same as the entity
#' names and then exports to STF and deletes the YAML files
#'
#' these very minimal files should load and be able to form a study
#' (via `study_from_stf()`) although variables will lack all specialised
#' annnotations.
#' 
make_minimal_stf <- function(output_directory) {
  study <- make_study(name = 'minimal stf demo')
  
  households <- study %>% get_entity('household') %>%
    modify_data(rename(household = Household.Id)) %>% sync_variable_metadata()
  
  participants <- study %>% get_entity('participant') %>%
    modify_data(rename(household = Household.Id, participant = Participant.Id)) %>%
    sync_variable_metadata() %>%
    redetect_column_as_id('participant') %>%
    set_parents('household', 'household')
  
  observations <- study %>% get_entity('observation') %>%
    modify_data(rename(household = Household.Id, participant = Participant.Id, observation = Part..Obs..Id)) %>%
    sync_variable_metadata() %>%
    redetect_column_as_id('participant') %>%
    redetect_column_as_id('household') %>%
    set_parents(c('participant', 'household'), c('participant', 'household'))
  
  study_from_entities(list(households, participants, observations)) %>%
    export_to_stf(output_directory)
  
  # remove all the yaml files
  list.files(output_directory, pattern = "\\.yaml$", full.names = TRUE) %>% unlink()
  
}

make_study_with_collections <- function(quiet_entities = TRUE, ...) {
  study <- make_study(quiet_entities, ...)

  households <- study %>% get_root_entity() %>% verbose()
  participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()
  
  suppressMessages(
    observations <- observations %>%
      create_variable_category(
        category_name = "integer.measures",
        children = c("Height..cm.", "Weight..kg."),
        display_name = "integer-based anatomical measures",
        definition = "integer-based anatomical measures"
      )
  )

  observations <- observations %>%
    create_variable_collection('integer.measures', member = 'measurement', member_plural = 'measurements')
  
  study <- study_from_entities(list(households, participants, observations), name = study %>% get_study_name())

  return(study)   
}