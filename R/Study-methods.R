#' @export
setGeneric("get_root_entity", function(study) standardGeneric("get_root_entity"))
#' @export
setGeneric("get_entities", function(study) standardGeneric("get_entities"))
#' @export
setGeneric("get_study_name", function(study) standardGeneric("get_study_name"))



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
setMethod("set_quiet", "Study", function(object, ...) {
  quiet <- if (missing(...)) { TRUE } else { ... }
  initialize(object, quiet = quiet)
})
