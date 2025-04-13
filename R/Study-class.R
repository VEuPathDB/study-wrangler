#' S4 Class for Study
#'
#' The `Study` class represents a scientific study or dataset that includes a name and a root entity.
#'
#' @slot name A `character` string representing the name of the study.
#' @slot root_entity An `Entity` object representing the root entity of the study.
#' @slot quiet A `logical` to stop any info/message output for validation and manipulation functions (default = FALSE)
#' 
#'
#' @keywords classes
#' @exportClass Study
#'
#' @examples
#' # Create a Study object
#' study <- study(name = "Example Study", root_entity = root_entity)
#' print(study)
setClass(
  "Study",
  slots = list(
    name = "character",
    root_entity = "Entity",
    collections = "tbl_df",
    quiet = "logical"
  )
)

#'
#' Study constructor with sensible defaults
#'
#' @export
study <- function(name = NA_character_, root_entity, collections = empty_collections, quiet = FALSE) {
  new("Study", name = name, root_entity = root_entity, collections = collections, quiet = quiet)
}
