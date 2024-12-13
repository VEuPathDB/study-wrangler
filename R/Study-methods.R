#' @export
setGeneric("get_root_entity", function(study) standardGeneric("get_root_entity"))
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
