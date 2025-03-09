library(glue)

#' @export
setGeneric("get_root_entity", function(study) standardGeneric("get_root_entity"))
#' @export
setGeneric("get_entities", function(study) standardGeneric("get_entities"))
#' @export
setGeneric("get_entity_names", function(study) standardGeneric("get_entity_names"))
#' @export
setGeneric("get_entity", function(study, ...) standardGeneric("get_entity"))
#' @export
setGeneric("get_study_name", function(study) standardGeneric("get_study_name"))
#' @export
setGeneric("set_study_metadata", function(study, ...) standardGeneric("set_study_metadata"))
#' @export
setGeneric("set_study_name", function(study, name) standardGeneric("set_study_name"))
#' @export
setGeneric("get_study_id", function(study) standardGeneric("get_study_id"))
#' @export
setGeneric("get_study_abbreviation", function(study) standardGeneric("get_study_abbreviation"))
#' @export
setGeneric("get_entity_abbreviation", function(study, entity_name) standardGeneric("get_entity_abbreviation"))



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

#' get_entity_names
#'
#' Gets a list of the names of the entities returned by get_entities()
#'
#' @param study A `Study` object.
#' @return list of entity names (`character`).
#' @export
setMethod("get_entity_names", "Study", function(study) {
  return(sapply(get_entities(study), get_entity_name))
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
  return(entities %>% detect(~ get_entity_name(.x) == name))
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


#' get_study_id
#'
#' Gets the study's uniquely generated ID.
#' 
#' Fatal error if the study_name is not set
#'
#' @param study A `Study` object.
#' @return A `character` string representing a generated unique ID for the study.
#' @export
setMethod("get_study_id", "Study", function(study) {
  name <- study %>% get_study_name()
  if (is_truthy(name)) {
    return(prefixed_alphanumeric_id(seed_string=name, prefix="STUDY_", length=10))
  } else {
    stop("Error: not allowed to call get_study_id() on a study with no name.")
  }
})

#' get_study_abbreviation
#'
#' Gets an SQL-friendly string to use in table names.
#' Depends on get_study_id()
#' Output is only 8 characters long but should still be unique.
#' 
#' Fatal error if the study_id is not available
#'
#' @param study A `Study` object.
#' @return A `character` string representing a short generated unique ID for the study for use in table names.
#' @export
setMethod("get_study_abbreviation", "Study", function(study) {
  name <- study %>% get_study_name()
  if (is_truthy(name)) {
    return(prefixed_alphanumeric_id(seed_string=name, prefix="s", length=10))
  } else {
    stop("Error: not allowed to call get_study_abbreviation() on a study with no study name.")
  }
})


# This implementation generates abbreviations on the fly.
# Consider adding caching for performance optimization if needed.
setMethod("get_entity_abbreviation", "Study", function(study, entity_name) {
  entity_names <- get_entity_names(study)
  abbreviations <- abbreviate(entity_names, minlength = 4)
  return(abbreviations[entity_name])
})

