# main key column is called 'category' (character)
# but that's not included here (because it can't have a default)
# (this is analogous to `variable_metadata_defaults`)
collection_metadata_defaults = tibble(
  stable_id = NA_character_,
  member = NA_character_,
  member_plural = NA_character_,
  display_name = NA_character_,
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
#'        The `display_name` field can be omitted if the `category_name` exists and has a non-null `display_name`.
#'        In that case the variable category's `display_name` will be used as the collection `display_name`
#' @returns a new Entity object with the variable collection added to its `collections` tibble
#' 
#' @examples
#' 
#' # TO DO: update with full collection_metadata_defaults
#' 
#' entity <- entity %>% create_variable_collection(
#'   category_name = "integer.measures",
#'   member = "measurement",
#'   member_plural = "measurements",
#'   display_name = "integer-based anatomical measurements",
#'   is_proportion = FALSE,
#'   is_compositional = FALSE,
#'   normalization_method = "none"
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
  # and, if successful, patch that in to `updates` as `display_name` if that hasn't been provided
  if (!is.null(category_display_name) && is.null(updates$display_name)) {
    updates$display_name <- category_display_name
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


#' Retrieve hydrated variable collection metadata
#'
#' Fetches collection‐level metadata and augments it with computed or derived fields
#' (e.g., \code{range_min}, \code{range_max}, \code{unit}). If there are no collections,
#' this returns an empty tibble without the extra “hydrated” columns. Consumers should
#' handle the empty‐result case explicitly.
#'
#' @param entity An \code{Entity} object containing collection and variable metadata.
#' @return A tibble with one row per collection/category and the following columns:
#'   \itemize{
#'     \item \code{category}: Collection identifier (the parent variable category).
#'     \item \code{stable_id}: If not provided to create_variable_collection, then a temporary ID will be generated.
#'     \item \code{member}: unchanged from user-supplied collection metadata
#'     \item \code{member_plural}: unchanged
#'     \item \code{display_name}: unchanged
#'     \item \code{is_proportion}: unchanged
#'     \item \code{is_compositional}: unchanged
#'     \item \code{normalization_method}: unchanged
#'     \item \code{unit}: Measurement unit (uniform across child variables).
#'     \item \code{impute_zero}: Logical flag indicating zero‐imputation strategy.
#'     \item \code{data_type}: Uniform data type (e.g., “integer”, “double”).
#'     \item \code{data_shape}: Uniform data shape descriptor (e.g., “scalar”, “vector”).
#'     \item \code{precision}: Minimum precision across all member variables.
#'     \item \code{range_min}: Minimum of \code{range_min} across child variables.
#'     \item \code{range_max}: Maximum of \code{range_max} across child variables.
#'     \item \code{display_range_min}: Minimum of collection-level \code{display_range_min} AND \code{display_range_min} across child variables.
#'     \item \code{display_range_max}: Maximum of collection-level \code{display_range_max} AND \code{dislpay_range_max} across child variables.
#'   }
#'
#' @export
setMethod("get_hydrated_collection_metadata", "Entity", function(entity) {
  # 1. Pull the base collection‐level metadata and the detailed variable metadata
  collections       <- entity %>% get_collection_metadata()
  variable_metadata <- entity %>% get_hydrated_variable_and_category_metadata()
  
  # 2. If there are no collections, return immediately (avoids warnings from summarizing empty groups)
  if (nrow(collections) == 0) {
    return(collections)
  }
  
  # 3. Join, select the necessary columns, and summarize per collection
  hydrated_collections <- collections %>%
    left_join(variable_metadata, join_by(category == parent_variable)) %>%
    select(
      category, stable_id.x,
      member, member_plural, display_name.x,
      is_proportion, is_compositional, normalization_method,
      display_range_min.x, display_range_min.y,
      display_range_max.x, display_range_max.y,
      range_min, range_max,
      impute_zero, data_type, data_shape, unit, precision
    ) %>%
    rename(
      display_name = display_name.x,
      stable_id = stable_id.x
    ) %>%
    group_by(category) %>%
    summarise(
      # For fields guaranteed to be uniform within each category (by `validate(entity)`),
      # just take the first
      across(
        c(stable_id, member, member_plural, display_name,
          is_proportion, is_compositional, normalization_method,
          unit, impute_zero, data_type, data_shape),
        first
      ),
      
      # For numeric summaries, compute min/max across all child rows.
      # Use na.rm = TRUE to avoid warnings if there happen to be NAs.
      precision = if (first(data_type) == "date") {
        NA
      } else {
        min(precision, na.rm = TRUE)
      },

      # now do range_min/range_max depending on data_type
      range_min = if (first(data_type) == "date") {
        # compare as strings because they should all be ISO-8601 format
        min(range_min, na.rm = TRUE)
      } else {
        ## numeric case
        format(min(as.numeric(range_min), na.rm = TRUE))
      },
      
      range_max = if (first(data_type) == "date") {
        max(range_max, na.rm = TRUE)
      } else {
        format(max(as.numeric(range_max), na.rm = TRUE))
      },
      
      # take the widest range of both collection and child variable annotated ranges
      # (if provided - otherwise return NA if all are NA)
      display_range_min = {
        is_date <- first(data_type) == "date"
        vals    <- if (is_date) {
          c(display_range_min.x, display_range_min.y)
        } else {
          as.numeric(c(display_range_min.x, display_range_min.y))
        }
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          # now it's safe to take min()
          out <- min(vals, na.rm = TRUE)
          if (is_date) out else format(out)
        }
      },
      
      display_range_max = {
        is_date <- first(data_type) == "date"
        vals    <- if (is_date) {
          c(display_range_max.x, display_range_max.y)
        } else {
          as.numeric(c(display_range_max.x, display_range_max.y))
        }
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          out <- max(vals, na.rm = TRUE)
          if (is_date) out else format(out)
        }
      },
      
      num_members = n(),
      
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      stable_id = if_else(
        is.na(stable_id),
        prefixed_alphanumeric_id(prefix = "COL_", length = 8, seed_string = category),
        stable_id
      )
    ) %>%
    ungroup()
  
  
  return(hydrated_collections)
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
