empty_collections = tibble(
  category = character(),
  member = character(),
  memberPlural = character(),
  label = character(),
  isProportion = logical(),
  isCompositional = logical(),
  normalizationMethod = character()
)


#' @export
setGeneric("create_variable_collection", function(entity, ...) standardGeneric("create_variable_collection"))
#' @export
setGeneric("delete_variable_collection", function(entity, category) standardGeneric("delete_variable_collection"))

#' create_variable_collection
#'
#' create a variable collection
#' 
#' @param entity an Entity object
#' @param ... key-value pairs defining collection metadata, see example below, all must be provided
#'        However, the `label` field can be omitted if the `category` exists and has a non-null `display_name`.
#'        In that case the variable category's `display_name` will be used as the collection label.
#' @returns a new Entity object with the variable collection added to its `collections` tibble
#' 
#' @examples
#' entity <- entity %>% create_variable_collection(
#'   category = "integer.measures",
#'   member = "measurement",
#'   memberPlural = "measurements",
#'   label = "integer-based anatomical measurements",
#'   isProportion = FALSE,
#'   isCompositional = FALSE,
#'   normalizationMethod = "none"
#' )
#' 
#' @export
setMethod("create_variable_collection", "Entity", function(entity, ...) {
  collections <- entity@collections
  updates <- list(...)
  
  # silently attempt to get the category's display_name from variable metadata
  category_display_name <- entity %>%
    get_category_metadata() %>%
    filter(variable %in% updates$category) %>%
    pull(display_name) %>%
    pluck(1, .default = NULL)
  # and, if successful, patch that in to `updates` as `label` if that hasn't been provided
  if (!is.null(category_display_name) && is.null(updates$label)) {
    updates$label <- category_display_name
  }
  
  updates_fields <- names(updates)
  collections_fields <- names(collections)
  if (!identical(collections_fields, names(empty_collections))) {
    stop("Internal error: entity@collections is corrupted")
  }
  
  # Ensure all fields are valid column names
  invalid_fields <- setdiff(updates_fields, collections_fields)
  if (length(invalid_fields) > 0) {
    stop(glue("Error: invalid field(s): {toString(invalid_fields)}"))
  }

  # Ensure all collection metadata fields are provided
  missing_fields <- setdiff(collections_fields, updates_fields)
  if (length(missing_fields) > 0) {
    stop(glue("Error: missing field(s): {toString(missing_fields)}"))
  }
  
  # Check we don't already have it
  if (
    collections %>%
    filter(category == updates$category) %>% nrow()
  ) {
    stop(glue("Error: variable collection '{updates$category}' already exists in entity"))
  }

  # Check the category exists in that entity
  if (
    entity %>%
    get_category_metadata() %>%
    filter(variable == updates$category) %>%
    nrow() != 1
  ) {
    stop(glue("variable collection cannot be added because category '{updates$category}' does not exist in entity"))
  }
    
  # Finally, add the row!  
  collections <- collections %>%
    bind_rows(
      tibble(!!!updates)
    ) 
  initialize(entity, collections = collections)
})


#' delete_variable_collection
#'
#' delete a variable collection
#' 
#' @param entity an Entity object
#' @param category a character string containing the category name that the collection corresponds to
#' @returns a new Entity object with the variable collection removed
#' 
#' @export
setMethod("delete_variable_collection", "Entity", function(entity, category) {
  collections <- entity@collections
  new_collections <- collections %>%
    filter(category != !!category)
  if (nrow(new_collections) == nrow(collections)) {
    stop(glue("Error: variable collection '{category}' not found and therefore not deleted"))
  }
  initialize(entity, collections = new_collections)
})
