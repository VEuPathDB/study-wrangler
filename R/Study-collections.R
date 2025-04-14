empty_collections = tibble(
  entity = character(),
  category = character(),
  member = character(),
  memberPlural = character(),
  label = character(),
  isProportion = logical(),
  isCompositional = logical(),
  normalizationMethod = character()
)


#' @export
setGeneric("create_variable_collection", function(study, ...) standardGeneric("create_variable_collection"))
#' @export
setGeneric("delete_variable_collection", function(study, entity, category) standardGeneric("delete_variable_collection"))

#' create_variable_collection
#'
#' create a variable collection
#' 
#' @param object a Study object
#' @param ... key-value pairs defining collection metadata, see example below, all must be provided
#' @returns a new Study object with the variable collection added to its `collections` tibble
#' 
#' @examples
#' study <- study %>% create_variable_collection(
#'   entity = "observation",
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
setMethod("create_variable_collection", "Study", function(study, ...) {
  collections <- study@collections
  updates <- list(...)

  updates_fields <- names(updates)
  collections_fields <- names(collections)
  
  # check that the `collections` tibbles hasn't been corrupted
  if (!identical(collections_fields, names(empty_collections))) {
    stop("Internal error: study@collections is corrupted")
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
    filter(
      category == updates$category,
      entity == updates$entity
    ) %>% nrow()
  ) {
    stop(glue("Error: variable collection '{updates$category}' for entity '{updates$entity}' already exists"))
  }

  # Check the entity exists
  if (study %>% get_entity(updates$entity) %>% is.null()) {
   stop(glue("variable collection cannot be added because entity '{updates$entity}' does not exist in study")) 
  }

  # Check the category exists in that entity
  if (
    study %>%
    get_entity(updates$entity) %>%
    get_category_metadata() %>%
    filter(variable == updates$category) %>%
    nrow() != 1
  ) {
    stop(glue("variable collection cannot be added because category '{updates$category}' does not exist in entity '{updates$entity}'"))
  }
    
  # Finally, add the row!  
  collections <- collections %>%
    bind_rows(
      tibble(!!!updates)
    ) 
  
  initialize(study, collections = collections)
})


#' delete_variable_collection
#'
#' delete a variable collection
#' 
#' @param object a Study object
#' @param entity a character string containing the entity name that the colllection should belong to
#' @param category a character string containing the category name that the collection corresponds to
#' @returns a new Study object with the variable collection removed
#' 
#' @export
setMethod("delete_variable_collection", "Study", function(study, entity, category) {
  collections <- study@collections
  
  new_collections <- collections %>%
    filter(category != !!category | entity != !!entity)
  
  # error if we didn't remove anything
  if (nrow(new_collections) == nrow(collections)) {
    stop(glue("Error: variable collection '{category}' for entity '{entity}' not found and therefore not deleted"))
  }
  
  initialize(study, collections = new_collections)
})
