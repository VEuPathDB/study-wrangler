#' @export
setGeneric("get_root_entity", function(study) standardGeneric("get_root_entity"))
#' @export
setGeneric("get_entities", function(study) standardGeneric("get_entities"))
#' @export
setGeneric("get_study_name", function(study) standardGeneric("get_study_name"))
#' @export
setGeneric("set_study_metadata", function(study, ...) standardGeneric("set_study_metadata"))
#' @export
setGeneric("set_study_name", function(study, name) standardGeneric("set_study_name"))


#' get_root_entity
#'
#' Gets the root entity of the study.
#'
#' @param study A `Study` object.
#' @return The root `Entity` object of the study.
#' @export
setMethod("get_root_entity", "Study", function(study) {
  return(study@root_entity)
})

#' get_entities
#'
#' Gets a depth-first flattened list of entities, root to leaf order.
#'
#' @param study A `Study` object.
#' @return list of entities.
#' @export
setMethod("get_entities", "Study", function(study) {
  return(flatten_entities(study@root_entity))
})


#' get_study_name
#'
#' Gets the name of the study.
#'
#' @param study A `Study` object.
#' @return A `character` string representing the name of the study.
#' @export
setMethod("get_study_name", "Study", function(study) {
  return(study@name)
})

#' set_quiet
#' 
#' Sets an internal flag so that subsequent operations do not emit informational
#' and help messages
#' 
#' 
#' Example usage:
#' ```R
#' study <- study %>% set_quiet() %>% validate()
#' ```
#' 
#' @param entity a Study object
#' @param value = TRUE (default is TRUE; pass FALSE to make chatty again)
#' @returns a new Study object with the modified quiet slot
#' @export
setMethod("set_quiet", "Study", function(object, quiet = TRUE) {
  initialize(object, quiet = quiet)
})


#' set_study_metadata
#' 
#' Sets metadata such as `name` etc (see "Study-class.R")
#' 
#' @param study a Study object
#' @param ... key = value pairs for setting metadata
#' @returns modified Study
#' @export
setMethod("set_study_metadata", "Study", function(study, ...) {
  metadata <- list(...)
  
  # Validate metadata keys
  validate_object_metadata_names('Study', metadata)
  
  if (length(metadata) == 0)
    return(study)
  
  # Merge new metadata with the existing study slots
  current_metadata <- as_list(study)
  updated_metadata <- modifyList(current_metadata, metadata)
  

  # Clone and modify the study
  return(do.call(initialize, c(study, updated_metadata)))
})

#' set_study_name
#' 
#' Sets the study name
#' 
#' @param study a Study object
#' @param name a character string
#' @returns modified Study
#' @export
setMethod("set_study_name", "Study", function(study, name) {
  # potentially do validation on name
  if (!study@quiet) message(glue("Adding study name '{name}'..."))
  return(study %>% set_study_metadata(name = name))
})
