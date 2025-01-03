#' not exported
setGeneric("export_to_vdi", function(entities, output_directory, install_json) standardGeneric("export_to_vdi"))


#' export_to_vdi
#'
#' Export an `Entity` object as part of the `Study` VDI export process
#'
#' @param study A `Study` object.
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (which will be created if it doesn't exist).
#' @param install_json A JSON-like list object that contains table schema and index definitions
#' @return appended `install_json` JSON-like object
#' @export
setMethod("export_to_vdi", "EntityPath", function(entities, output_directory, install_json) {
  if (missing(output_directory)) {
    stop("Required argument output_directory not provided.")
  }
  if (missing(install_json)) {
    stop("Required argument install_json not provided.")
  }
  # Validate that all elements in the list are of class 'Entity'
  if (!all(sapply(entities, function(x) is(x, "Entity")))) {
    stop("All elements of 'entities' must be of class 'Entity'.")
  }
  
  return(install_json)
})
