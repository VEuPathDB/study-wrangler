library(tibble)

#' S4 Class for Entity
#'
#' The `Entity` class represents a data entity, typically used in hierarchical data structures. 
#' It includes the data itself, metadata about variables, and descriptive information about the entity.
#'
#' @slot data A `tbl_df` (tibble) containing the entity's data. This is the main dataset for the entity.
#' @slot variables A `tbl_df` (tibble) containing metadata about the variables in the `data` slot, including ID columns. See 'Entity-metadata-defaults.R' for more details.
#' @slot name A `character` string representing the internal name of the entity (e.g., `"household"`).
#' @slot description A `character` string providing a description of the entity.
#' @slot display_name A `character` string representing the external display name of the entity 
#'   (e.g., `"Household"`). Defaults to the value of `name` if not explicitly provided.
#' @slot display_name_plural A `character` string representing the pluralized external display name 
#'   of the entity (e.g., `"Households"`). Defaults to `display_name` with `"s"` appended if not explicitly provided.
#' @param children a list of child entities (if the entity has been assembled into a tree). Empty for leaf nodes and unassembled entities.
#'
#' @keywords classes
#' @exportClass Entity
#'
#' @examples
#' # Define an Entity object
#' library(tibble)
#' data <- tibble(id = 1:3, value = c("A", "B", "C"))
#' variables <- tibble(variable = c("id", "value"), data_type = c("id", "string"), ...)
#' entity <- new("Entity",
#'               data = data,
#'               variables = variables,
#'               name = "example_entity",
#'               description = "An example entity",
#'               display_name = "Example Entity",
#'               display_name_plural = "Example Entities")
#' print(entity)
setClass(
  "Entity",
  slots = list(
    data = "tbl_df",
    variables = "tbl_df",
    
    # entity metadata
    name = "character",
    description = "character",
    display_name = "character",
    display_name_plural = "character",
    children = "list"
  )
)

#' Constructor Function for Entity Class
#'
#' A helper function to create `Entity` objects. Provides defaults for `display_name` and 
#' `display_name_plural` based on the `name` parameter.
#'
#' @param data A `tbl_df` (tibble) containing the entity's data.
#' @param variables A `tbl_df` (tibble) containing metadata for the variables in the `data`. Defaults to an empty tibble.
#' @param name A `character` string representing the internal name of the entity. Defaults to `NA_character_`.
#' @param description A `character` string providing a description of the entity. Defaults to `NA_character_`.
#' @param display_name A `character` string for the external display name of the entity. Defaults to the value of `name`.
#' @param display_name_plural A `character` string for the pluralized external display name. Defaults to `display_name` with `"s"` appended.
#' @param children a list of child entities (if the entity has been assembled into a tree). Empty for leaf nodes and unassembled entities.
#'
#' @return An object of class `Entity`.
#' @export
#'
#' @examples
#' # Create an Entity object using the constructor
#' library(tibble)
#' data <- tibble(id = 1:3, value = c("A", "B", "C"))
#' variables <- tibble(variable = c("id", "value"), data_type = c("id", "string", ...))
#' entity <- entity(
#'   data = data,
#'   variables = variables,
#'   name = "example_entity",
#'   description = "An example entity"
#' )
#' print(entity)
entity <- function(data,
                   variables = tibble(),
                   name = NA_character_,
                   description = NA_character_,
                   display_name = NA_character_,
                   display_name_plural = NA_character_,
                   children = list()
) {
  
  metadata <- list(
    data = data,
    variables = variables,
    name = name,
    description = description,
    display_name = display_name,
    display_name_plural = display_name_plural,
    children = children
  )
  
  metadata <- apply_entity_metadata_defaults(metadata)
  
  do.call(new, c("Entity", metadata))
}
