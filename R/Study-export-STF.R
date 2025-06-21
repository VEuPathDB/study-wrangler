#'
#' for Study and EntityPath objects
#'
setGeneric("export_to_stf", function(object, output_directory) standardGeneric("export_to_stf"))


#' export_to_stf
#'
#' Export a `Study` as STF files to a specified output directory, creating the directory if it doesn't exist.
#'
#' @param object A `Study` object.
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (which will be created if it doesn't exist).
#' @return The `Study` object (invisibly).
#' @export
setMethod("export_to_stf", "Study", function(object, output_directory) {
  study <- object
  if (missing(output_directory)) {
    stop("Required argument output_directory not provided.")
  }
  # reset the warning message deduplicator
  message_without_dupes$reset()
  
  # Check if the directory exists, and create it if necessary
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)  # Use recursive = TRUE for nested directories
  }
  
  # first handle the study metadata
  # there isn't very much of it at the moment
  study_metadata.path <- file.path(output_directory, 'study.yaml')
  entities <- study %>% get_entities()

  study_metadata <- study %>%
    slotNames() %>%
    keep(~ is.character(slot(study, .x))) %>%
    set_names() %>% # make a named character vector, c(name = 'name'), etc
    map(~ slot(study, .x)) %>% # map it to a list with the actual slot value as values
    list_assign( # add the entities to the list
      entities = entities %>% map(
        function(entity) entity %>% get_entity_name()
      )
    )
  write_pretty_yaml(study_metadata, study_metadata.path)

  # Call the new entity export function for each entity
  for (entity in entities) {
    export_entity_to_stf(entity, output_directory)
  }
  
  # Return the study object invisibly
  return(invisible(study))
})
