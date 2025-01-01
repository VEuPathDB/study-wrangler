#' @export
setGeneric("export_to_vdi", function(study, output_directory) standardGeneric("export_to_vdi"))


#' export_to_vdi
#'
#' Export a `Study` object to a specified output directory, creating the directory if it doesn't exist.
#'
#' @param study A `Study` object.
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (which will be created if it doesn't exist).
#' @return The `Study` object (invisibly).
#' @export
setMethod("export_to_vdi", "Study", function(study, output_directory) {
  if (missing(output_directory)) {
    stop("Required argument output_directory not provided.")
  }
  
  # Check if the directory exists, and create it if necessary
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)  # Use recursive = TRUE for nested directories
  }
  
  # Return the study object invisibly
  return(invisible(study))
})
