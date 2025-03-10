#'
#' study_from_stf
#' 
#' @description
#' Creates a study object from STF files in the given directory
#'
#' @param directory containing STF files for one study
#' @return A Study object
#'
#' @export
study_from_stf <- function(directory) {
  
  filepaths <- list.files(directory, full.names = TRUE)
  if (length(filepaths) == 0) stop(glue("Can't find directory '{directory}' or it is empty"))
  study_metadata_path <- file.path(directory, 'study.yaml')
  
  study <- NULL
  
  # get the study metadata if it exists
  if (study_metadata_path %in% filepaths) {
    # we'll look for the entity files listed in study.yaml rather than try to load
    # everything (see `else`)
    study_metadata <- yaml::read_yaml(study_metadata_path)
    if (!is_empty(study_metadata$entities)) {
      
      entities <- study_metadata$entities %>%
        map(
          function(entity_name) {
            tsv_path <- file.path(directory, glue("entity-{entity_name}.tsv"))
            yaml_path <- file.path(directory, glue("entity-{entity_name}.yaml"))
            entity <- entity_from_stf(tsv_path, yaml_path)
            if (!validate(quiet(entity)))
              stop(glue("Can't create valid entity from STF file '{tsv_path}' with metadata '{yaml_path}'"))
            return(entity)
          }
        )
      # strip down the metadata so we can hopefully pass it directly to
      # the study constructor
      study_metadata <- study_metadata %>% discard(names(study_metadata) == "entities")
      study <- do.call(study_from_entities, c(list(entities), study_metadata))      
    } else {
      stop(glue("Malformed study metadata in '{study_metadata_path}'. No entities listed."))
    }
  } else {
    # we can still make a study but it won't have a name
    # or any other metadata
    entity_tsv_files <- filepaths %>% keep(~ .x %>% str_detect('entity-.+\\.tsv$'))

    entities <- entity_tsv_files %>% map(
      function(file_path) {
        entity <- entity_from_stf(file_path)
        if (!validate(quiet(entity)))
          stop(glue("Can't create valid entity from STF file '{file_path}'"))
        return(entity)
      }
    )
    
    study <- study_from_entities(entities)
  }
  return(study)
}

