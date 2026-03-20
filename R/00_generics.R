#' A generic for inspecting objects
#'
#' @param object The object to inspect
#' @param ... Additional arguments. For Entity: variable_name, max_variables (default 100)
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
#' @param object The object to validate.
#' @param profile Character vector of validation profiles to use. If NULL, uses global config.
#' @param format Character. "full" (default) shows R-oriented messages with fix-it commands.
#'   "upload" shows plain-language messages suitable for web upload users.
#' @returns a Boolean indicating success or failure
#' @export
setGeneric("validate", function(object, profiles = NULL, format = "full") standardGeneric("validate"))
