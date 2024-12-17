#' @export
setGeneric("get_root_entity", function(study) standardGeneric("get_root_entity"))
#' @export
setGeneric("get_entities", function(study) standardGeneric("get_entities"))
#' @export
setGeneric("get_entity", function(study, ...) standardGeneric("get_entity"))
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


#' get_entity
#'
#' Gets an entity by name
#'
#' @param study A `Study` object.
#' @param name 
#' @return the matching entity or NULL
#' @export
setMethod("get_entity", "Study", function(study, name) {
  entities <- flatten_entities(study@root_entity)
  
  # TO DO
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

#' set_quiet for Study
#' 
#' Sets an internal flag to enable or disable quiet mode.
#' 
#' @param object a Study object
#' @param quiet logical; TRUE to suppress messages, FALSE to enable them
#' @returns a new Study object with the modified quiet slot
#' @export
setMethod("set_quiet", "Study", function(object, quiet) {
  initialize(object, quiet = quiet)
})

#' quiet for Study
#' 
#' Enables quiet mode on a Study object.
#' 
#' @param object a Study object
#' @returns a new Study object with quiet mode enabled
#' @export
setMethod("quiet", "Study", function(object) {
  set_quiet(object, quiet = TRUE)
})

#' verbose for Study
#' 
#' Enables verbose mode on a Study object.
#' 
#' @param object a Study object
#' @returns a new Study object with quiet mode disabled
#' @export
setMethod("verbose", "Study", function(object) {
  set_quiet(object, quiet = FALSE)
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
