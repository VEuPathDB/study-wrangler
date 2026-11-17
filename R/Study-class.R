#' S4 Class for Study
#'
#' The `Study` class represents a scientific study or dataset that includes a name and a root entity.
#'
#' @slot name A `character` string representing the name of the study.
#' @slot stable_id A `character` string, the study's stable_id from its source system.
#'   When unset, `get_study_id()` derives one from the name.
#' @slot root_entity An `Entity` object representing the root entity of the study.
#' @slot quiet A `logical` to stop any info/message output for validation and manipulation functions (default = FALSE)
#' 
#'
#' @keywords classes
#' @exportClass Study
#'
#' @examples
#' # Create a Study object
#' root_entity <- entity(data = tibble::tibble(id = 1:3), name = "example_entity")
#' study <- study(name = "Example Study", root_entity = root_entity)
#' print(study)
setClass(
  "Study",
  slots = list(
    name = "character",
    stable_id = "character",
    root_entity = "Entity",
    quiet = "logical"
  )
)

#'
#' Study constructor with sensible defaults
#'
#' @export
study <- function(name = NA_character_, root_entity, quiet = FALSE, stable_id = NA_character_) {
  new("Study", name = name, stable_id = stable_id, root_entity = root_entity, quiet = quiet)
}
