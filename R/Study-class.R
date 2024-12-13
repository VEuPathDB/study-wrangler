#' S4 Class for Study
#'
#' The `Study` class represents a scientific study or dataset that includes a name and a root entity.
#'
#' @slot name A `character` string representing the name of the study.
#' @slot root_entity An `Entity` object representing the root entity of the study.
#'
#' @keywords classes
#' @exportClass Study
#'
#' @examples
#' # Create a Study object
#' root_entity <- new("Entity", name = "root", data = tibble(), variables = tibble())
#' study <- new("Study", name = "Example Study", root_entity = root_entity)
#' print(study)
setClass(
  "Study",
  slots = list(
    name = "character",
    root_entity = "Entity"
  )
)
