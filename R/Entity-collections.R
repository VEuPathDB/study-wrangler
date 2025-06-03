# main key column is called 'category' (character)
# but that's not included here (because it can't have a default)
# (this is analogous to `variable_metadata_defaults`)
collection_metadata_defaults = tibble(
  member = NA_character_,
  member_plural = NA_character_,
  label = NA_character_,
  is_proportion = FALSE,
  is_compositional = FALSE,
  normalization_method = NA_character_,
  # Note that the GUS plugin here https://github.com/VEuPathDB/ApiCommonData/blob/50f31d647a91e282df298b3894617bd6dcbe09ca/Load/plugin/perl/LoadDatasetSpecificEntityGraph.pm#L861
  # formerly used to require the human-annotated display_range_min and max
  # to be declared as `range_min` and `range_max` in the collections.yaml file.
  # However, to the best of my knowledge, no collections.yaml file ever did this in practice.
  # Therefore I think it's best to be consistent with regular variables and use
  # the longer form for the human-curated overrides.
  display_range_max = NA_character_,
  display_range_min = NA_character_
)

empty_collections <- collection_metadata_defaults %>%
  bind_rows(tibble(category = NA_character_)) %>%
  relocate(category) %>%
  slice(0)

#' @export
setGeneric("create_variable_collection", function(entity, category_name, ...) standardGeneric("create_variable_collection"))
#' @export
setGeneric("delete_variable_collection", function(entity, category_name) standardGeneric("delete_variable_collection"))
#' @export
setGeneric("set_collection_metadata", function(entity, category_name, ...) standardGeneric("set_collection_metadata"))
#' @export
setGeneric("get_collection_metadata", function(entity) standardGeneric("get_collection_metadata"))
#' @export
setGeneric("get_hydrated_collection_metadata", function(entity) standardGeneric("get_hydrated_collection_metadata"))



#'
#'
#' create_variable_collection
#'
#' create a variable collection
#' 
#' @param entity an Entity object
#' @param category_name character value that is the same as the "variable category" that should already exist
#' @param ... key-value pairs defining collection metadata, defaults or NAs will be used if not provided.
#'        The `label` field can be omitted if the `category_name` exists and has a non-null `display_name`.
#'        In that case the variable category's `display_name` will be used as the collection label.
#' @returns a new Entity object with the variable collection added to its `collections` tibble
#' 
#' @examples
#' 
#' # TO DO: update with full collection_metadata_defaults
#' 
#' entity <- entity %>% create_variable_collection(
#'   category_name = "integer.measures",
#'   member = "measurement",
#'   memberPlural = "measurements",
#'   label = "integer-based anatomical measurements",
#'   isProportion = FALSE,
#'   isCompositional = FALSE,
#'   normalizationMethod = "none"
#' )
#' 
#' @export
setMethod("create_variable_collection", "Entity", function(entity, category_name, ...) {
  collections <- entity@collections
  updates <- list(...)
  
  if (missing(category_name)) {
    stop("Error: must provide `category_name` argument to `create_variable_collection(entity, category_name, ...)`")
  }
  
  # silently attempt to get the category's display_name from variable metadata
  category_display_name <- entity %>%
    get_category_metadata() %>%
    filter(variable %in% !!category_name) %>%
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
    filter(category == !!category_name) %>% nrow()
  ) {
    stop(glue("Error: variable collection '{category_name}' already exists in entity"))
  }

  # Check the category exists in that entity
  if (
    entity %>%
    get_category_metadata() %>%
    filter(variable == category_name) %>%
    nrow() != 1
  ) {
    stop(glue("variable collection cannot be added because category '{category_name}' does not exist in entity"))
  }
    
  # Finally, add the row!  
  collections <- collections %>%
    bind_rows(
      # blend the provided values in `updates` into the default values (overriding them)
      # and add the category column
      list_modify(collection_metadata_defaults, category = category_name, !!!updates)
    ) %>% relocate(category) # it's nice to have the category as the first column

  initialize(entity, collections = collections)
})


#' delete_variable_collection
#'
#' delete a variable collection
#' 
#' @param entity an Entity object
#' @param category_name a character string containing the category name that the collection corresponds to
#' @returns a new Entity object with the variable collection removed
#' 
#' @export
setMethod("delete_variable_collection", "Entity", function(entity, category_name) {
  collections <- entity@collections
  new_collections <- collections %>%
    filter(category != !!category_name)
  if (nrow(new_collections) == nrow(collections)) {
    stop(glue("Error: variable collection '{category_name}' not found and therefore not deleted"))
  }
  initialize(entity, collections = new_collections)
})


#' get the variable collection metadata tibble
#'
#' @export
setMethod("get_collection_metadata", "Entity", function(entity) {
  return(entity@collections)
})


#' get the variable collection metadata tibble with filled-in
#' calculated and derived values (e.g. range_min/max and unit)
#'
#' @export
setMethod("get_hydrated_collection_metadata", "Entity", function(entity) {
  collections <- entity %>% get_collection_metadata()
  variable_metadata <- entity %>% get_hydrated_variable_and_category_metadata()

  # final hydrated columns need to be
  ### from `collections`
  # member = NA_character_,
  # member_plural = NA_character_,
  # label = NA_character_,
  # is_proportion = FALSE,
  # is_compositional = FALSE,
  # normalization_method = NA_character_,
  # display_range_min = NA_character_  # min
  # display_range_max = NA_character_, # max
  ### summarised from child variables
  # num_members
  # range_min # min
  # range_max # max
  # impute_zero # unique
  # data_type # unique
  # data_shape # unique
  # unit # unique
  # precision # min (I'd have thought max but the ApiCommonData implementation has min)
  
  
  
  return(collections)
})

#' setter similar to `set_variable_metadata()` for variable collections
#' 
#' @param entity an Entity object
#' @param category_name a string value identical to a variable category name
#' @param ... key=value arguments where key is a collection metadata column name
#'        e.g. `member_plural='genes'`
#' @returns modified entity
#' @export
setMethod("set_collection_metadata", "Entity", function(entity, category_name, ...) {
  updates <- list(...)
  collections <- entity %>% get_collection_metadata()

  # Validate all keys are valid collection metadata fields
  valid_fields <- names(collection_metadata_defaults)
  invalid_keys <- setdiff(names(updates), valid_fields)
  if (length(invalid_keys) > 0) {
    stop(glue("Error: invalid field(s): {toString(invalid_keys)}"))
  }

  # Locate the row for the collection_name
  row_number <- collections %>%
    mutate(row_num = row_number()) %>%
    filter(category == category_name) %>%
    pull(row_num)

  if (length(row_number) == 0) {
    stop(glue("Error: metadata not found for collection '{category_name}'"))
  }
  if (length(row_number) > 1) {
    stop(glue("Error: multiple metadata rows found for collection '{category_name}'"))
  }

  # Update the specified row and columns
  walk2(
    names(updates),
    updates,
    function(x, y) {
      tryCatch({
        default_val <- collection_metadata_defaults[[x]]
        if (is.list(default_val)) {
          collections[[row_number, x]] <<- list(y)
        } else {
          collections[row_number, x] <<- y
        }
      }, error = function(e) {
        stop(e)
      })
    }
  )
  quoted_names = names(updates) %>% map(~ glue("'{.x}'"))
  if (!entity@quiet) message(glue("Made metadata update(s) to {paste0(quoted_names, collapse=', ')} for collection '{category_name}'"))

  # Return modified entity
  return(entity %>% initialize(collections = collections))
})
