#' A generic for inspecting objects
#'
#' @param object The object to inspect
#' @param ... Additional arguments
#' @export
setGeneric("inspect", function(object, ...) standardGeneric("inspect"))

#' A generic for setting a quiet option on an object
#' 
#' @export
setGeneric("set_quiet", function(object, quiet, ...) standardGeneric("set_quiet"))

#' A generic for enabling quiet mode on an object
#' 
#' @export
setGeneric("quiet", function(object, ...) standardGeneric("quiet"))

#' A generic for enabling verbose mode on an object
#' 
#' @export
setGeneric("verbose", function(object, ...) standardGeneric("verbose"))


#' Validate Generic
#'
#' Defines the S4 generic for the validate function.
#' 
#' @param entity The object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setGeneric("validate", function(object) standardGeneric("validate"))


#'
#' for Study and EntityPath objects
#'
setGeneric("export_to_vdi", function(object, output_directory, install_json = NULL, entitytypegraph_cache = NULL) {
  standardGeneric("export_to_vdi")
})
