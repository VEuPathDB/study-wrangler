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
  yaml::write_yaml(study_metadata, study_metadata.path)
  
  export_entity_to_stf_recursively(
    EntityPath(list(get_root_entity(study))),
    output_directory
  )
  
  # Return the study object invisibly
  return(invisible(study))
})


# not sure recursion is necessary here, but we used it for VDI export
# so it may come in handy at some point?
export_entity_to_stf_recursively <- function(object, output_directory) {
  entities <- object
  entity <- entities[[length(entities)]]
  entity_name <- entity %>% get_entity_name()
  
  # write the metadata to YAML
  entity_metadata.path <- file.path(output_directory, glue("entity-{entity_name}.yaml"))
  
  ids_metadata <- entity %>% get_id_column_metadata() %>% tibble_to_sparse_object()
  variables_metadata <- entity %>% get_variable_metadata() %>% tibble_to_sparse_object()
  categories_metadata <- entity %>% get_category_metadata() %>% tibble_to_sparse_object()
  
  entity_metadata <- entity %>%
    slotNames() %>%
    keep(~ is.character(slot(entity, .x))) %>%
    set_names() %>% # make a named character vector, c(name = 'name'), etc
    map(~ slot(entity, .x)) %>% # map it to a list with the actual slot value as values
    discard(is.na) %>% # to remove any YAML entries with no value provided
    list_assign( # add the tabular metadata
      ids = ids_metadata,
      variables = variables_metadata,
      categories = categories_metadata
    )
  
  yaml::write_yaml(entity_metadata, entity_metadata.path)
  
  # write the data to STF-style TSV
  # (auto tall or wide format)
  
  
  # Recurse into child entities
  child_entities <- entity %>% get_children()
  for (child in child_entities) {
    export_entity_to_stf_recursively(
      EntityPath(c(entities, list(child))),
      output_directory
    )
  }
}

