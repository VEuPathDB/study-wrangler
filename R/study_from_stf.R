study_from_stf <- function(directory) {
  
  filenames <- list.files(directory, full.names = FALSE)
  filepaths <- list.files(directory, full.names = TRUE)
  
  if (length(filenames) == 0) stop(glue("Can't find directory '{directory}' or it is empty"))

  study <- NULL
  
  # get the study metadata if it exists
  if ('study.yaml' %in% filenames) {
    
    
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

