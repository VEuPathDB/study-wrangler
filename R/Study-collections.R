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
  if (!identical(collections_fields, names(empty_collections))) {
    stop("Internal error: study@collections has wrong columns")
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
  
  # Check we don't already have it (checking 'category' only at the mo)
  if (collections %>% filter(category == updates$category) %>% nrow()) {
    stop(glue("Error: variable collection '{updates$category}' already exists"))
  }
  
  collections <- bind_rows(
    collections,
    tibble(!!!updates)
  )
  
  initialize(study, collections = collections)
})
