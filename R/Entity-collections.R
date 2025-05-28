# main key column is called 'category' (character)
# but that's not included here (because it can't have a default)
# (this is analogous to `variable_metadata_defaults`)
collection_metadata_defaults = tibble(
  member = NA_character_,
  memberPlural = NA_character_,
  label = NA_character_,
  isProportion = FALSE,
  isCompositional = FALSE,
  normalizationMethod = NA_character_
)

empty_collections <- collection_metadata_defaults %>%
  bind_rows(tibble(category = NA_character_)) %>%
  relocate(category) %>%
  slice(0)

#' @export
setGeneric("create_variable_collection", function(entity, category, ...) standardGeneric("create_variable_collection"))
#' @export
setGeneric("delete_variable_collection", function(entity, category) standardGeneric("delete_variable_collection"))

#' create_variable_collection
#'
#' create a variable collection
#' 
#' @param entity an Entity object
#' @param category character value that is the same as the "variable category" that should already exist
#' @param ... key-value pairs defining collection metadata, defaults or NAs will be used if not provided.
#'        The `label` field can be omitted if the `category` exists and has a non-null `display_name`.
#'        In that case the variable category's `display_name` will be used as the collection label.
#' @returns a new Entity object with the variable collection added to its `collections` tibble
#' 
#' @examples
#' 
#' # TO DO: update with full collection_metadata_defaults
#' 
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
setMethod("create_variable_collection", "Entity", function(entity, category, ...) {
  collections <- entity@collections
  updates <- list(...)
  
  if (missing(category)) {
    stop("Error: must provide `category` argument to `create_variable_collection(entity, category, ...)`")
  }
  
  # silently attempt to get the category's display_name from variable metadata
  category_display_name <- entity %>%
    get_category_metadata() %>%
    filter(variable %in% !!category) %>%
    pull(display_name) %>%
    pluck(1, .default = NULL)
  # and, if successful, patch that in to `updates` as `label` if that hasn't been provided
  if (!is.null(category_display_name) && is.null(updates$label)) {
    updates$label <- category_display_name
  }
  
  updates_fields <- names(updates)
  collections_fields <- names(collection_metadata_defaults)

  # Ensure all fields are valid column names
  invalid_fields <- setdiff(updates_fields, collections_fields)
  if (length(invalid_fields) > 0) {
    stop(glue("Error: invalid field(s): {toString(invalid_fields)}"))
  }

  # Check we don't already have it
  if (
    collections %>%
    filter(category == !!category) %>% nrow()
  ) {
    stop(glue("Error: variable collection '{category}' already exists in entity"))
  }

  # Check the category exists in that entity
  if (
    entity %>%
    get_category_metadata() %>%
    filter(variable == category) %>%
    nrow() != 1
  ) {
    stop(glue("variable collection cannot be added because category '{category}' does not exist in entity"))
  }
    
  # Finally, add the row!  
  collections <- collections %>%
    bind_rows(
      # blend the provided values in `updates` into the default values (overriding them)
      # and add the category column
      list_modify(collection_metadata_defaults, category = category, !!!updates)
    ) %>% relocate(category) # it's nice to have the category as the first column

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
