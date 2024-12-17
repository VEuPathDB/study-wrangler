#' A generic for inspecting objects
#'
#' @param object The object to inspect
#' @param ... Additional arguments
#' @export
setGeneric("inspect", function(object, ...) standardGeneric("inspect"))

#' A generic for setting a quiet option on an object
#' 
#' @param value = TRUE
#' @export
setGeneric("set_quiet", function(object, ...) standardGeneric("set_quiet"))


#' Validate Generic
#'
#' Defines the S4 generic for the validate function.
#' 
#' @param entity The object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setGeneric("validate", function(object) standardGeneric("validate"))
