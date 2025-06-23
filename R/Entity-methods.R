#' File: Entity metadata management methods
#'
#' Generics (attempting to remove redundancy from documentation)
#' 
setGeneric("infer_missing_data_types", function(entity, ...) standardGeneric("infer_missing_data_types"))
setGeneric("infer_missing_data_shapes", function(entity) standardGeneric("infer_missing_data_shapes"))
#' @export
setGeneric("redetect_columns", function(entity, columns, ...) standardGeneric("redetect_columns"))
#' @export
setGeneric("redetect_columns_as_variables", function(entity, columns) standardGeneric("redetect_columns_as_variables"))
#' @export
setGeneric("redetect_column_as_id", function(entity, column) standardGeneric("redetect_column_as_id"))
#' @export
setGeneric("set_entity_metadata", function(entity, ...) standardGeneric("set_entity_metadata"))
#' @export
setGeneric("set_entity_name", function(entity, name) standardGeneric("set_entity_name"))
#' @export
setGeneric("get_entity_name", function(entity) standardGeneric("get_entity_name"))
#' @export
setGeneric("get_description", function(entity) standardGeneric("get_description"))
#' @export
setGeneric("set_stable_id", function(entity, stable_id) standardGeneric("set_stable_id"))
#' @export
setGeneric("get_stable_id", function(entity) standardGeneric("get_stable_id"))
#' @export
setGeneric("get_entity_id_column", function(entity) standardGeneric("get_entity_id_column"))
#' @export
setGeneric("get_display_name", function(entity) standardGeneric("get_display_name"))
#' @export
setGeneric("get_display_name_plural", function(entity) standardGeneric("get_display_name_plural"))
#' @export
setGeneric("sync_variable_metadata", function(entity) standardGeneric("sync_variable_metadata"))
#' @export
setGeneric("set_variable_metadata", function(entity, variable_name, ...) standardGeneric("set_variable_metadata"))
#' @export
setGeneric("get_variable_metadata", function(entity) standardGeneric("get_variable_metadata"))
#' @export
setGeneric("get_id_column_metadata", function(entity, ...) standardGeneric("get_id_column_metadata"))
#' @export
setGeneric("get_category_metadata", function(entity, ...) standardGeneric("get_category_metadata"))
#' @export
setGeneric("get_variable_and_category_metadata", function(entity, ...) standardGeneric("get_variable_and_category_metadata"))
#' @export
setGeneric("get_data", function(entity) standardGeneric("get_data"))
#' @export
setGeneric("modify_data", function(entity, ...) standardGeneric("modify_data"))
#' @export
setGeneric("set_variable_display_names_from_provider_labels", function(entity) standardGeneric("set_variable_display_names_from_provider_labels"))
#' @export
setGeneric("set_parents", function(entity, names, id_columns) standardGeneric("set_parents"))
#' @export
setGeneric("get_parents", function(entity) standardGeneric("get_parents"))
#' @export
setGeneric("get_parent_name", function(entity) standardGeneric("get_parent_name"))
#' @export
setGeneric("get_parent_id_column", function(entity) standardGeneric("get_parent_id_column"))
#' @export
setGeneric("get_children", function(entity) standardGeneric("get_children"))
#' @export
setGeneric("pretty_tree", function(entity) standardGeneric("pretty_tree"))
#' @export
setGeneric("check_parent_child_join", function(parent, child) standardGeneric("check_parent_child_join"))
#' @export
setGeneric("remove_children", function(entity) standardGeneric("remove_children"))
#' @export
setGeneric("set_variable_as_date", function(entity, column_name) standardGeneric("set_variable_as_date"))
# not exported
setGeneric("get_hydrated_variable_and_category_metadata", function(entity) standardGeneric("get_hydrated_variable_and_category_metadata"))
#' @export
setGeneric("set_variable_ordinal_levels", function(entity, variable_name, levels) standardGeneric("set_variable_ordinal_levels"))
#' @export
setGeneric("create_variable_category", function(entity, category_name, children, ...) standardGeneric("create_variable_category"))
#' @export
setGeneric("delete_variable_category", function(entity, category_name) standardGeneric("delete_variable_category"))
#' @export
setGeneric("set_variables_multivalued", function(entity, ...) standardGeneric("set_variables_multivalued"))
#' @export
setGeneric("set_variables_univalued", function(entity, variable_names) standardGeneric("set_variables_univalued"))
#' @export
setGeneric("sync_ordinal_data", function(entity) standardGeneric("sync_ordinal_data"))


#' infer_missing_data_types
#' 
#' Infers `data_type` metadata for columns where this is currently `NA`.
#' 
#' @param entity an Entity object.
#' @param .allowed_data_types character vector, optional. Specifies the types that are allowed when inferring `data_type`. Must be one or more of "date", "integer", "number", or "id". Defaults to `NULL` (no restriction).
#' @param .disallowed_data_types character vector, optional. Specifies the types that are disallowed when inferring `data_type`. Must be one or more of "date", "integer", "number", or "id". Defaults to `NULL` (no restriction).
#' @returns Modified entity with updated `data_type` metadata for columns where it was previously `NA`.
setMethod("infer_missing_data_types", "Entity",
function(
  entity,
  .allowed_data_types = NULL,
  .disallowed_data_types = NULL
) {
  variables <- entity@variables
  data <- entity@data

  # use infer_column_data_type(data_column) to fill in NAs in variables$data_type column
  variables <- variables %>%
    rowwise() %>% # performance is not critical here
    mutate(
      data_type = fct_mutate(
        data_type,
        is.na(data_type),
        infer_data_type(
          if (data_type %in% c('category')) 
            NA
          else if (is_multi_valued) 
            expand_multivalued_data_column(data, variable, is_multi_valued, multi_value_delimiter, .type_convert = TRUE) %>% pull(variable)
          else
            data %>% pull(variable),
          .allowed_data_types = .allowed_data_types,
          .disallowed_data_types = .disallowed_data_types
        )
      )
    ) %>%
    ungroup() # remove special row-wise grouping

  # clone and modify original entity argument
  return(entity %>% initialize(variables = variables))
})

#' infer_missing_data_shapes
#' 
#' Infers `data_shape` metadata for columns where this is currently `NA`
#' Also converts non-continuous columns to factors and sets their `vocabulary`
#' 
#' @param entity an Entity object
#' @returns modified entity
setMethod("infer_missing_data_shapes", "Entity", function(entity) {
  variables <- entity@variables
  
  # only infer for data_shape == NA and non-ID cols
  cols_to_infer <- variables %>%
    filter(is.na(data_shape)) %>%
    filter(!is.na(data_type)) %>%
    filter(!data_type %in% c('id', 'category')) %>%
    pull(variable)
  
  # infer data_shape as continuous or categorical
  # (further refinement to 'ordinal' will require user-input)
  variables <- variables %>% mutate(
    data_shape = if_else(
      variable %in% cols_to_infer,
      case_when(
        data_type %in% c('number', 'integer', 'date') ~ fct_mutate(data_shape, 'continuous'),
        TRUE ~ fct_mutate(data_shape, 'categorical')
      ),
      data_shape
    )
  )

  # clone and modify original entity argument
  return(entity %>% initialize(variables=variables))
})


#' redetect_columns
#' 
#' Redoes the automatic type detection for specific columns
#' 
#' This function re-applies type inference to the specified columns, updating their metadata.
#' 
#' @param entity an Entity object
#' @param columns a character vector of column names
#' @param .allowed_data_types character vector, optional. Specifies the types that are allowed when inferring `data_type`. Must be one or more of "date", "integer", "number", or "id". Defaults to `NULL` (no restriction).
#' @param .disallowed_data_types character vector, optional. Specifies the types that are disallowed when inferring `data_type`. Must be one or more of "date", "integer", "number", or "id". Defaults to `NULL` (no restriction).
#' @returns Modified entity
setMethod("redetect_columns", "Entity", function(entity, columns, .allowed_data_types = NULL, .disallowed_data_types = NULL) {
  data <- entity@data
  variables <- entity@variables

  # Early return if `columns` is empty
  if (missing(columns) || length(columns) == 0) {
    if (!entity@quiet) message("No column names provided. No changes made.")
    return(entity)
  }
    
  # Check that all `columns` column names exist as columns in `data`
  missing_columns <- setdiff(columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(glue("Error: the following data columns do not exist in this entity: {paste(missing_columns, collapse = ', ')}"))
  }
  
  # Check that all `columns` exist as rows in `variables` metadata
  missing_metadata <- setdiff(columns, variables$variable)
  if (length(missing_metadata) > 0) {
    stop(glue("Error: the following columns are missing from entity metadata: {paste(missing_metadata, collapse = ', ')}"))
  }

  # Set `variables$data_type` to NA where `variable %in% columns`
  if (!entity@quiet) message("Redoing type detection")
  variables <- variables %>%
    mutate(
      data_type = fct_mutate(
        data_type,
        variable %in% columns,
        NA
      )
    )
  
  # reform entity object and then redo column inference
  entity <- entity %>%
    initialize(variables=variables) %>%
    infer_missing_data_types(
      .allowed_data_types = .allowed_data_types,
      .disallowed_data_types = .disallowed_data_types
    ) %>%
    infer_missing_data_shapes()

  return(entity)
})

#' redetect_columns_as_variables
#' 
#' Redoes the automatic type detection for specific columns but will never
#' guess ID type for those columns
#'  
#' @param entity an Entity object
#' @param columns a character vector of column names
#' @returns modified entity
setMethod("redetect_columns_as_variables", "Entity", function(entity, columns) {
  return(redetect_columns(entity, columns, .disallowed_data_types = c("id")))
})

#' redetect_column_as_id
#' 
#' Attempts to redetect one named column as a primary ID column (i.e. all values are
#' unique) but note that this will fall back to data_type: "string" if the
#' column values aren't all unique 
#'  
#' @param entity an Entity object
#' @param column name of the column to be redetected
#' @returns modified entity
setMethod("redetect_column_as_id", "Entity", function(entity, column) {
  if (length(column) > 1)
    stop("Error: you can only `redetect_column_as_id()` for one column.")
  return(redetect_columns(entity, columns = column, .allowed_data_types = c("id")))
})


#' set_entity_metadata
#' 
#' Sets metadata such as `name`, `display_name`, etc (see "Entity-class.R")
#' 
#' @param entity an Entity object
#' @param ... key = value pairs for setting metadata
#' @returns modified entity
#' @export
setMethod("set_entity_metadata", "Entity", function(entity, ...) {
  metadata <- list(...)
  
  # Validate metadata keys
  validate_object_metadata_names('Entity', metadata)
  
  if (length(metadata) == 0)
    return(entity)
  
  # Merge new metadata with the existing entity slots
  current_metadata <- as_list(entity)
  updated_metadata <- modifyList(current_metadata, metadata)
  
  # Apply defaults
  updated_metadata <- apply_entity_metadata_defaults(updated_metadata, verbose = TRUE)
  
  # Clone and modify the entity
  return(do.call(initialize, c(entity, updated_metadata)))
})

#' set_entity_name
#' 
#' Sets `name` metadata AND SHOULD PROBABLY SET entity_name metadata for
#' id columns at entity_level == 0
#' 
#' @param entity an Entity object
#' @param name a string value to set the name to
#' @returns modified entity
#' @export
setMethod("set_entity_name", "Entity", function(entity, name) {
  if (validate_entity_name(name)) {
    
    if (!entity@quiet) message(glue("Adding entity name '{name}'..."))
    # set it with the general purpose method that also
    # sets display_name* with sensible fallbacks
    entity <- entity %>% set_entity_metadata(name = name)
    
    # set entity_name for all level 0 columns
    variables <- entity@variables %>%
      mutate(
        entity_name = case_when(
          entity_level == 0 ~ name,
          TRUE ~ entity_name
        )
      )
    
    entity <- entity %>% initialize(variables=variables)
  } else {
    warning(glue("Warning: Entity name is missing or not plain alphanumeric"))
  }
  return(entity)
})

#' get_entity_name
#' 
#' Gets the name of the entity
#' 
#' @param entity an Entity object
#' @returns a single character string (name of the entity)
#' @export
setMethod("get_entity_name", "Entity", function(entity) {
  return(entity@name)
})

#' get_description
#' 
#' Gets the description of the entity
#' 
#' @param entity an Entity object
#' @returns a single character string (description of the entity)
#' @export
setMethod("get_description", "Entity", function(entity) {
  return(entity@description)
})


#' set_stable_id
#' 
#' Sets `stable_id` metadata
#' 
#' @param entity an Entity object
#' @param stable_id a string value to set the stable_id to
#' @returns modified entity
#' @export
setMethod("set_stable_id", "Entity", function(entity, stable_id) {
  if (validate_stable_id(stable_id)) {
    if (!entity@quiet) message(glue("Adding stable_id '{stable_id}'..."))
    entity <- entity %>% set_entity_metadata(stable_id = stable_id)
  } else {
    warning(glue("Warning: Entity's stable_id is missing or not plain alphanumeric"))
  }
  return(entity)
})

#' get_stable_id
#' 
#' Gets the stable_id of the entity.
#' 
#' If no stable_id has been assigned at object construction time or with
#' `set_stable_id(entity, stable_id)` then a placeholder will be generated
#' on the fly (with a `message()` to inform) - but it requires entity@name
#' to be set (it will `stop()` if it's not available)
#' 
#' @param entity an Entity object
#' @returns a single character string (name of the entity)
#' @export
setMethod("get_stable_id", "Entity", function(entity) {
  stable_id <- entity@stable_id
  if (is_truthy(stable_id)) {
    return(stable_id)
  }
  if (is_truthy(entity@name)) {
    if (!is_truthy(entity@quiet)) message_without_dupes$send(glue("Generating temporary stable_id for entity '{entity@name}'"))
    return(prefixed_alphanumeric_id(prefix = "ENT_", seed_string = entity@name, length = 8))
  }
  stop(glue("Could not generate temporary stable_id for entity- entity_name required"))
})


#' get_entity_id_column
#' 
#' Gets the name of this entity's ID column
#' 
#' @param entity an Entity object
#' @returns a single character string (name of the column), or NULL if there any issues
#' @export
setMethod("get_entity_id_column", "Entity", function(entity) {
  variables <- entity@variables

  my_id_columns <- variables %>% 
    filter(data_type == "id") %>% 
    filter(entity_level == 0) %>% 
    pull(variable)

  if (length(my_id_columns) == 1) {
    return(my_id_columns)
  }
    
  return(NULL)
})


#' get_display_name
#'
#' Gets the display name of the entity
#'
#' @param entity an Entity object
#' @returns a single character string (display name of the entity)
#' @export
setMethod("get_display_name", "Entity", function(entity) {
  return(entity@display_name)
})

#' get_display_name_plural
#'
#' Gets the plural display name of the entity
#'
#' @param entity an Entity object
#' @returns a single character string (plural display name of the entity)
#' @export
setMethod("get_display_name_plural", "Entity", function(entity) {
  return(entity@display_name_plural)
})


#' sync_variable_metadata
#' 
#' Fixes `data` and `variables` metadata if they aren't aligned with each other.
#' 
#' 1. If there are data column(s) with no metadata, add metadata rows with default values
#' 2. If there are metadata row(s) with no data, remove those rows
#' 
#' `data` is not touched!
#' 
#' 
#' @param entity an Entity object
#' @returns modified entity
#' @export
setMethod("sync_variable_metadata", "Entity", function(entity) {
  data <- entity@data
  variables <- if ("variable" %in% colnames(entity@variables)) {
    entity@variables
  } else {
    message("Reinitializing empty or corrupted variable metadata...")
    tibble(variable = character(0), data_type = factor())
  }

  actual_variables <- variables %>% filter(is.na(data_type) | data_type != 'category') %>% pull(variable)
  
  missing_variables <- setdiff(colnames(data), actual_variables)
  extra_variables <- setdiff(actual_variables, colnames(data))

  # Early return if no mismatches
  if (length(missing_variables) == 0 && length(extra_variables) == 0) {
    if (!entity@quiet) message("No metadata synchronization needed.")
    return(entity)
  }
  
  if (length(missing_variables)) {
    # create new row(s) in variables for these newly appeared data columns
    # we will assume that the column names are "clean"
    missing_metadata <-
      tibble(
        variable=missing_variables,
      ) %>%
      expand_grid(variable_metadata_defaults) %>%
      # and we'll set the provider_label to the same name because that's all we have
      # and the entity@name if we have it
      mutate(
        provider_label = missing_variables %>% map(list),
        entity_name = entity@name,
      )

    variables <- bind_rows(variables, missing_metadata)
    if (!entity@quiet) message(paste(
      "Synced variables metadata by adding defaults for:",
      paste(missing_variables, collapse = ", ")
    ))
  }
    
  if (length(extra_variables)) {
    variables <- variables %>%
      filter(!variable %in% extra_variables)
    if (!entity@quiet) message(paste(
      "Synced metadata by removing these variables with no data:",
      paste(extra_variables, collapse = ", ")
    ))
  }
  
  return(
    entity %>%
      initialize(variables=variables) %>%
      infer_missing_data_types() %>%
      infer_missing_data_shapes()
  )
})

  
#' set_variable_metadata
#' 
#' Sets metadata for a named variable (or category)
#' 
#' @param entity an Entity object
#' @param variable_name a string value of the column name in `entity@data`
#' @param ... key=value arguments where key is a variable metadata column name
#'        e.g. `data_shape='ordinal'`
#' @returns modified entity
#' @export
setMethod("set_variable_metadata", "Entity", function(entity, variable_name, ...) {
  updates <- list(...)
  variables <- entity@variables
  
  # Ensure all keys are valid column names
  invalid_keys <- setdiff(names(updates), names(variables))
  if (length(invalid_keys) > 0) {
    stop(glue("Error: invalid column(s): {toString(invalid_keys)}"))
  }
  
  # Locate the rows (hopefully just one) for the variable_name
  # (using tidyverse style here on purpose)
  row_number <- variables %>%
    mutate(row_num = row_number()) %>%
    filter(variable == variable_name) %>%
    pull(row_num)
  
  if (length(row_number) == 0) {
    stop(glue("Error: metadata not found for variable '{variable_name}'"))
  }
  if (length(row_number) > 1) {
    stop(glue("Error: multiple metadata rows found for variable '{variable_name}'"))
  }
  
  # name of variable to use in fix-it command suggestions  
  global_varname = find_global_varname(entity, 'entity')
  
  # Update the specified row and columns
  walk2(
    names(updates), 
    updates,
    function(x, y) {
      tryCatch({
        if (is.list(y)) {
          # For list columns, assign as a list
          
          # First check if we're assigning into a list-column of factors
          # as bad factor levels aren't caught automatically by R
          default_val <- variable_metadata_defaults[[x]]
          if (is.list(default_val) && is.factor(unlist(default_val))) {
            lvls <- levels(unlist(default_val))
            
            # Check for invalid values in y
            invalid_values <- unlist(y) %>% keep(~ !is.na(.x) && !(.x %in% lvls))
            if (length(invalid_values) > 0) {
              stop(to_lines(
                glue("Error: Cannot assign value(s) '{paste(invalid_values, collapse = ', ')}' to metadata field '{x}'"),
                glue("because they are not among the allowed factor levels."),
                glue("Allowed values are: {paste(lvls, collapse = ', ')}")
              ), call. = FALSE)
            }
          }
          
          # Safe to assign now
          variables[[row_number, x]] <<- list(y)
        } else {
          # For non-list columns, assign directly
          variables[row_number, x] <<- y
        }
      }, error = function(e) {
        if (is_truthy(e$parent$message)) {
          if (grepl("Can't convert.+to.+factor", e$parent$message)) {
            stop(to_lines(
              glue("Error: Cannot assign value '{y}' to metadata field '{x}' because it takes a factor"),
              glue("value and '{y}' (type: {typeof(y)}) is not allowed."),
              glue("Allowed values are: {variables %>% pull(x) %>% levels() %>% paste(collapse = ', ')}")
            ), call. = FALSE)
          } else if (grepl("Can't convert.+to.+integer", e$parent$message)) {
            stop(to_lines(
              glue("Error: Cannot assign value '{y}' to metadata field '{x}' because it takes an integer"),
              glue("value and '{y}' (type: {typeof(y)}) is not allowed.")
            ), call. = FALSE)
          } else if (grepl("Can't convert.+to.+double", e$parent$message)) {
            stop(to_lines(
              glue("Error: Cannot assign value '{y}' to metadata field '{x}' because it takes a number"),
              glue("value and '{y}' (type: {typeof(y)}) is not allowed.")
            ), call. = FALSE)
          } else if (grepl("Can't convert.+to.+character", e$parent$message)) {
            stop(to_lines(
              glue("Error: Cannot assign value '{y}' to metadata field '{x}' because it takes a string"),
              glue("value and '{y}' (type: {typeof(y)}) is not allowed.")
            ), call. = FALSE)
          } else if (grepl("Can't convert.+to.+list", e$parent$message)) {
            stop(to_lines(
              glue("Error: Cannot assign value '{y}' to metadata field '{x}' because it takes a list"),
              glue("value and '{y}' (type: {typeof(y)}) is not allowed."),
              glue("Even single values must be wrapped in a `list()`, for example:"),
              indented(
                glue("{global_varname} <- {global_varname} %>% set_variable_metadata('{variable_name}', {x} = list('{y}'))")
              )
            ), call. = FALSE)
          }
        }
        stop(e)
      })
    }
  )
  quoted_names = names(updates) %>% map(~ glue("'{.x}'"))
  if (!entity@quiet) message(glue("Made metadata update(s) to {paste0(quoted_names, collapse=', ')} for '{variable_name}'"))

  # return modified entity
  return(entity %>% initialize(variables=variables))
})


#' get_variable_metadata
#' 
#' Returns a metadata tibble for variables only (not ID columns)
#' 
#' Treat this data as read-only (use set_xxx methods to make changes)
#' 
#' @param entity an Entity object
#' @returns tibble of metadata for variables
setMethod("get_variable_metadata", "Entity", function(entity) {
  return(
    entity@variables %>%
      filter(!data_type %in% c('id','category')) %>%
      select(!starts_with('entity_')) %>%
      arrange(display_order)
  )
})

#' get_category_metadata
#' 
#' Returns a metadata tibble for category 'variables' only (not ID columns or actual variables)
#' 
#' Treat this data as read-only (use set_xxx methods to make changes)
#' 
#' @param entity an Entity object
#' @returns tibble of metadata for category columns
setMethod("get_category_metadata", "Entity", function(entity, ...) {
  return(
    entity@variables %>%
      filter(data_type == 'category') %>%
      select(!starts_with('entity_')) %>%
      arrange(display_order)
  )
})

#' get_variable_and_category_metadata
#' 
#' Returns a metadata tibble for category 'variables' only (not ID columns or actual variables)
#' 
#' Treat this data as read-only (use set_xxx methods to make changes)
#' 
#' @param entity an Entity object
#' @returns tibble of metadata for category columns
setMethod("get_variable_and_category_metadata", "Entity", function(entity, ...) {
  return(
    entity@variables %>%
      filter(data_type != 'id') %>%
      select(!starts_with('entity_')) %>%
      arrange(display_order)
  )
})

#' get_id_column_metadata
#' 
#' Returns a metadata tibble for variables only (not ID columns)
#' 
#' Treat this data as read-only (use set_xxx methods to make changes)
#' 
#' @param entity an Entity object
#' @returns tibble of metadata for id_columns
setMethod("get_id_column_metadata", "Entity", function(entity, ...) {
  return(
    entity@variables %>%
      filter(data_type == 'id') %>%
      select(variable, starts_with('entity_'), provider_label) %>%
      arrange(entity_level)
  )
})


#' get_data
#' 
#' Returns the data tibble as-is
#' 
#' Treat this as read-only. If you need to make changes to the data use `modify_data()`
#' 
#' @param entity an Entity object
#' @returns data tibble
#' @export
setMethod("get_data", "Entity", function(entity) {
  return(entity@data)
})


#' modify_data
#' 
#' Returns a new entity object with data modified by the pipeline passed to it.
#' This allows tidyverse-style manipulation of an Entity object's `data` slot.
#' 
#' Example usage:
#' ```R
#' households <- households %>%
#'   modify_data(mutate(Construction.material = fct_recode(Construction.material, 'Concrete' = 'Concrte')))
#' ```
#' 
#' @param entity an Entity object
#' @param ... a tidyverse pipeline to modify the data slot of the entity object
#' @returns a new Entity object with the modified data slot
#' @export
setMethod("modify_data", "Entity", function(entity, ...) {
  if (missing(...)) {
    stop("The `...` argument is missing. Please provide a pipeline to modify the data.", call. = FALSE)
  }
  new_data <- entity@data %>% ...
  initialize(entity, data = new_data)
})

#'
#' set_variable_display_names_from_provider_labels
#' 
#' copies provider_labels into any unset variable display_names 
#' 
#' @param entity an Entity object
#' @returns modified entity
#' @export
setMethod("set_variable_display_names_from_provider_labels", "Entity", function(entity) {
  variables <- entity@variables
  
  # Define the logical mask for rows that need updating
  mask <- variables %>%
    mutate(
      data_type != 'id' & is.na(display_name),
      .keep = "none"
    ) %>%
    pull()

  # Update the display_name for rows matching the mask
  variables <- variables %>%
    mutate(display_name = if_else(
      mask,
      map_chr(provider_label, ~ if (length(.x) > 0) .x[[1]] else NA_character_),
      display_name
    ))

  if (!entity@quiet) message(glue("Copied provider_label over to display_name for {sum(mask, na.rm = TRUE)} variables"))
  return(entity %>% initialize(variables=variables))
})

#' set_parents
#' 
#' Sets metadata for parent ID columns of this entity
#' 
#' @param entity an Entity object
#' @param names a character vector of the entity names of parent, grandparent, etc
#' @param columns a character vector of the column names containing parent_id, grandparent_id, etc
#' @returns modified entity
#' @export
setMethod("set_parents", "Entity", function(entity, names, id_columns) {
  data <- entity@data
  variables <- entity@variables

  # Early return if `names` and `id_columns` are empty
  if (length(names) == 0 && length(id_columns) == 0) {
    if (!entity@quiet) message("No parent entity relationships provided. No changes made.")
    return(entity)
  }

  # Check that length of `names` and `id_columns` are the same
  if (length(names) != length(id_columns)) {
    stop("Error: 'names' and 'id_columns' must have the same length.")
  }
  
  # Check that all `id_columns` column names exist as columns in `data`
  missing_columns <- setdiff(id_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(glue("Error: the following data columns do not exist in this entity: {paste(missing_columns, collapse = ', ')}"))
  }
  
  # Check that all `id_columns` exist as rows in `variables` metadata
  missing_metadata <- setdiff(id_columns, variables$variable)
  if (length(missing_metadata) > 0) {
    stop(glue("Error: the following columns are missing from entity metadata: {paste(missing_metadata, collapse = ', ')}"))
  }
  
  # Generalized mutation to update `variables`
  variables <- variables %>%
    mutate(
      data_type = fct_mutate(data_type, variable %in% id_columns, 'id'),
      data_shape = fct_mutate(data_shape, variable %in% id_columns, NA),
      entity_name = if_else(variable %in% id_columns, names[match(variable, id_columns)], entity_name),
      entity_level = if_else(variable %in% id_columns, -match(variable, id_columns), entity_level),
    )
  
  if (!entity@quiet) message("Parent entity relationships and columns have been set")
  
  # Return modified entity
  return(entity %>% initialize(variables = variables))
})


#' gets_parents
#' 
#' Gets metadata for parent ID columns of this entity
#' 
#' @param entity an Entity object
#' @returns list of `names` (character vector) and `columns` (character vector)
#' @export
setMethod("get_parents", "Entity", function(entity) {
  # Get metadata for ID columns and filter for parents
  parent_metadata <- entity %>%
    get_id_column_metadata() %>%
    filter(entity_level < 0) %>%
    arrange(desc(entity_level)) # Reverse the order to match set_parents()
  
  # Extract names and columns for parent entities
  names <- parent_metadata$entity_name
  id_columns <- parent_metadata$variable
  
  return(list(names = names, id_columns = id_columns))
})

#' get_parent_name
#' 
#' Gets the name of the immediate parent entity
#' 
#' @param entity an Entity object
#' @returns a single character string (name of the immediate parent) or NULL if no parents exist
#' @export
setMethod("get_parent_name", "Entity", function(entity) {
  parents <- entity %>% get_parents()
  
  # Return the first name if it exists, otherwise return NULL
  if (length(parents$names) > 0) {
    return(parents$names[1])
  } else {
    return(NULL)
  }
})

#' get_parent_id_column
#' 
#' Gets the name of the immediate parent entity
#' 
#' @param entity an Entity object
#' @returns a single character string (column name in the data tibble) or NULL if no parents exist
#' @export
setMethod("get_parent_id_column", "Entity", function(entity) {
  parents <- entity %>% get_parents()
  
  # Return the first name if it exists, otherwise return NULL
  if (length(parents$id_columns) > 0) {
    return(parents$id_columns[1])
  } else {
    return(NULL)
  }
})

#' get_children
#' 
#' Gets the list of child entities. Only applicable in the context of a Study
#' where a tree has been constructed
#' 
#' @param entity an Entity object
#' @returns a list of Entity objects
#' @export
setMethod("get_children", "Entity", function(entity) {
  return(entity@children)
})

#'
#' pretty_tree
#'
#' @param entity an entity object
#' @returns vector of character (lines to print or cat)
#' @export
setMethod("pretty_tree", "Entity", function(entity) {
  # This will be our main entry point.
  # We'll call a helper function that recursively gathers all lines.
  lines <- recursive_ascii_tree(
    entity,
    prefix = "",
    is_last = TRUE,
    is_root = TRUE,
    get_label_fn = get_entity_name,
    get_children_fn = get_children
  )
  return(lines)
})

#' set_quiet for Entity
#' 
#' Sets an internal flag to enable or disable quiet mode.
#' 
#' @param object an Entity object
#' @param quiet logical; TRUE to suppress messages, FALSE to enable them
#' @returns a new Entity object with the modified quiet slot
#' @export
setMethod("set_quiet", "Entity", function(object, quiet) {
  initialize(object, quiet = quiet)
})

#' quiet for Entity
#' 
#' Enables quiet mode on an Entity object.
#' 
#' @param object an Entity object
#' @returns a new Entity object with quiet mode enabled
#' @export
setMethod("quiet", "Entity", function(object) {
  set_quiet(object, quiet = TRUE)
})

#' verbose for Entity
#' 
#' Enables verbose mode on an Entity object.
#' 
#' @param object an Entity object
#' @returns a new Entity object with quiet mode disabled
#' @export
setMethod("verbose", "Entity", function(object) {
  set_quiet(object, quiet = FALSE)
})

#'
#' check_parent_child_join
#'
#' @param parent An Entity object
#' @param child An Entity object that is a direct child of the parent
#' 
#' Performs a join of the parent and child tibbles and returns
#' FALSE if there are any child rows that can't be joined to the parent
#'
#' @export
setMethod(
  "check_parent_child_join",
  signature(parent = "Entity", child = "Entity"),
  function(parent, child) {
    
    # Extract data
    child_data <- child %>% get_data() # tibble
    parent_data <- parent %>% get_data() # tibble
    
    # Identify columns
    child_id_column_name <- child %>% get_entity_id_column()   # child's primary key
    child_join_column_name <- child %>% get_parent_id_column() # child's foreign key to parent
    parent_join_column_name <- parent %>% get_entity_id_column() # parent's primary key
    
    # Perform a right join from parent_data to child_data so that all child rows appear
    # If a child's foreign key doesn't match a parent's primary key, parent columns will be NA.
    joined_data <- parent_data %>% 
      right_join(
        child_data, 
        by = setNames(child_join_column_name, parent_join_column_name),
        suffix = c('.x', '.y'), # with these two args, join column names will be 
        keep = TRUE # Household.Id.x and Household.Id.y
      )
    parent_join_column_name.x <- paste0(parent_join_column_name, '.x')
    child_join_column_name.y <- paste0(child_join_column_name, '.y')
    
    # Find rows where the parent match failed (i.e. parent's PK column is NA)
    missing_parents <- joined_data %>%
      filter(if_any(all_of(parent_join_column_name.x), is.na)) %>%
      select(all_of(c(child_join_column_name.y, child_id_column_name))) %>%
      rename(setNames(child_join_column_name.y, child_join_column_name))

    is_valid <- nrow(missing_parents) == 0
    return(list(
      is_valid = is_valid,
      missing_mappings = if (!is_valid) missing_parents else NULL
    ))
  }
)

#'
#' remove children from entity (in Study context)
#' 
#' 
#' @param object an Entity object
#' @returns a new Entity object with children removed
#' @export
setMethod("remove_children", "Entity", function(entity) {
  entity@children <- list()
  return(entity)
})


#' Set Variable as Date
#'
#' Convenience function to mark a column as a date type and ensure conversion succeeds.
#'
#' @param entity An Entity object.
#' @param column_name The name of the column to be converted.
#' @returns Modified Entity object.
setMethod("set_variable_as_date", "Entity", function(entity, column_name) {
  global_varname <- find_global_varname(entity, 'entity')

  # bail if this is a multi-valued variable
  delimiter <- entity %>% get_variable_metadata() %>%
    filter(variable == column_name, is_multi_valued) %>%
    select(multi_value_delimiter)
  if (nrow(delimiter) == 1) {
    stop(to_lines(
      "This is a multi-valued variable and `set_variable_as_date()` is not appropriate.",
      "If the values, after delimiter-based expansion, are all YYYY-MM-DD compliant, then",
      "you may use the following command to configure the variable as a date:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variables_multivalued('{column_name}' = '{delimiter}')"))
    ))
    
  }
  
  # Pre-check and convert column
  data <- entity@data
  data <- check_and_convert_to_date(data, column_name)
  
  # Update metadata and return the modified entity
  entity <- entity %>%
    set_variable_metadata(column_name, data_type = "date") %>%
    initialize(data = data)
  
  return(entity)
})

#' get_hydrated_variable_and_category_metadata
#' 
#' Returns a metadata tibble for variables with sensible defaults
#' imputed for
#' * vocabulary 
#' * stable_id
#' * parent_stable_id
#' * precision for number and integer vars
#' 
#' Treat this data as read-only (use set_xxx methods to make changes)
#' 
#' @param entity an Entity object
#' @returns tibble of metadata for variables
#' not exported
setMethod("get_hydrated_variable_and_category_metadata", "Entity", function(entity) {
  data <- entity %>% get_data()
  global_varname <- find_global_varname(entity, 'entity')
  
  entity_stable_id <- if (is_truthy(entity@name)) entity %>% get_stable_id() else {
    message_without_dupes$send(to_lines(c(
      "Warning: because this entity has no `name` (required), a placeholder entity ID has been generated.",
      "You can make things more stable by providing an entity name as follows:",
      indented(glue("{global_varname} <- {global_varname} %>% set_entity_name('your_entity_name')"))
    )))
    '__PLACEHOLDER_ENTITY_ID__'
  }
  
  if (entity %>% get_variable_metadata() %>% nrow() == 0) {
    return(empty_variable_metadata)
  }
  
  safe_fn <- function(x, fn) {
    if (!is.factor(x) & !is.character(x)) {
      fn(x, na.rm = TRUE)
    } else {
      NA
    }
  }
  
  safe_fivenum <- function(x, is_date = FALSE) {
    if (is.factor(x)) {
      # Return NA if input is a factor
      return(rep(NA_real_, 5))
    } else if (is_date) {
      # Handle date input by converting to numeric and back to Date
      stats <- as.Date(stats::fivenum(as.numeric(x)), origin = "1970-01-01")
      return(stats)
    } else if (is.numeric(x)) {
      # Standard numeric input
      return(stats::fivenum(x, na.rm = TRUE))
    } else {
      # Return NA for unsupported types
      return(rep(NA_real_, 5))
    }
  }
  
  # start with actual-variable metadata and fill in data-derived values
  metadata <- entity %>%
    get_variable_metadata() %>%
    rowwise() %>% # because functions applied below aren't vectorized
    mutate(
      # handle multi-valued data columns
      column_data = expand_multivalued_data_column(
        data,
        variable,
        is_multi_valued,
        multi_value_delimiter,
        data_type
      ) %>% pull(variable) %>% list(),
      vocabulary = if_else(
        has_values & data_shape != 'continuous',
        list(column_data %>% as.factor() %>% levels()),
        NA
      ),
      precision = case_when(
        !has_values ~ NA,
        data_type == 'integer' ~ 0L,
        data_type == 'number' ~ column_data %>% max_decimals(),
        TRUE ~ NA
      ),
      # do this for all actual variables
      distinct_values_count = if_else(
        has_values,
        column_data %>% n_distinct(),
        NA
      ),
      mean = if_else(
        has_values & data_shape == 'continuous',
        column_data %>% safe_fn(mean) %>% as.character(),
        NA_character_
      ),
      bin_width_computed = if_else(
        has_values & data_shape == 'continuous',
        column_data %>% safe_fn(findBinWidth) %>% as.character(),
        NA_character_
      ),
      # Use fivenum() for continuous data to get summary statistics
      summary_stats = if_else(
        has_values & data_shape == 'continuous',
        list(safe_fivenum(column_data, is_date = data_type == "date")),
        list(rep(NA_real_, 5))
      ),
      range_min = if_else(has_values & data_shape == 'continuous', as.character(summary_stats[[1]]), NA_character_),
      lower_quartile = if_else(has_values & data_shape == 'continuous', as.character(summary_stats[[2]]), NA_character_),
      median = if_else(has_values & data_shape == 'continuous', as.character(summary_stats[[3]]), NA_character_),
      upper_quartile = if_else(has_values & data_shape == 'continuous', as.character(summary_stats[[4]]), NA_character_),
      range_max = if_else(has_values & data_shape == 'continuous', as.character(summary_stats[[5]]), NA_character_),
      column_data = NULL, # don't need this any more
      summary_stats = NULL  # nor this
    ) %>% 
    ungroup()
  
  # append category metadata and add fallback stable_id if needed
  metadata <- metadata %>%
    bind_rows(entity %>% get_category_metadata()) %>%
    rowwise() %>%
    mutate(
      stable_id = if_else(
        is.na(stable_id),
        prefixed_alphanumeric_id(prefix = "VAR_", length = 8, seed_string = variable),
        stable_id
      )
    ) %>%
    ungroup()
    
  # now that the stable_ids are available for each variable (or category)
  # we can do a self-join to set the parent_stable_id
  metadata <- metadata %>%
    left_join(
      metadata %>% select(variable, stable_id) %>% rename(parent_stable_id = stable_id),
      join_by(parent_variable == variable)
    ) %>%
    # and replace any NA parent_stable_ids with the entity's stable_id
    mutate(
      parent_stable_id = if_else(
        is.na(parent_stable_id),
        entity_stable_id,
        parent_stable_id
      )
    )

  return(metadata)
})



#' set_variable_ordinal_levels
#' 
#' Declares a variable formally as an ordinal variable and sets the levels
#' (order of values)
#' 
#' @param entity an Entity object
#' @param variable_name a string value of the column name in `entity@data`
#' @param levels a list or character vector of the values, in order
#' 
#' Will throw an error if `levels` does not include all levels seen in the data
#' 
#' @returns modified entity
#' @export
setMethod("set_variable_ordinal_levels", "Entity", function(entity, variable_name, levels) {
  global_varname = find_global_varname(entity, "entity")
  levels <- as.list(levels)
  variables <- entity@variables
  data <- entity@data
  
  
  # check `variable_name` is OK
  if (variables %>% filter(variable == variable_name) %>% nrow() != 1) {
    if (variable_name %in% names(data)) {
      stop(to_lines(c(
        glue("Variable '{variable_name}' has no metadata. Run the following to fix it:"),
        indented(glue("{global_varname} <- {global_varname} %>% sync_variable_metadata()"))
      )))
    } else {
      stop(glue("No such variable '{variable_name}' - did you make a typo?"))
    }
  }
  
  # check data_type is string or integer
  if (variables %>%
      filter(
        variable == variable_name,
        data_type %in% c('string', 'integer')
      ) %>%
      nrow() != 1) {
    stop(glue("Only variables with data_type 'string' or 'integer' can be ordinals, sorry."))
  }
  
  # make sure the data column is a factor
  data <- data %>% mutate("{variable_name}" := as.factor(!!sym(variable_name)))

  # the data levels must be a subset of `levels`
  # (extra levels are OK)
  data_levels <- data %>% pull(variable_name) %>% levels()
  missing_levels = setdiff(data_levels, levels)
  if (length(missing_levels) > 0) {
    stop(to_lines(c(
      "The levels you provide must include all the observed levels in the data.",
      glue("You must also include these levels: {paste0(missing_levels, collapse=', ')}")
    )))
  }
  
  # update the entity data column with the levels
  entity@data <- entity@data %>%
    mutate("{variable_name}" := factor(!!sym(variable_name), levels = levels))
  
  # set the appropriate metadata and return
  former_quiet_state <- entity@quiet
  entity <- entity %>%
    quiet() %>%
    set_variable_metadata(
      variable_name,
      data_shape = 'ordinal',
      ordinal_levels = levels
    )
  entity@quiet <- former_quiet_state
  if (!entity@quiet) message(glue("Successfully set '{variable_name}' as an ordinal variable with levels: {paste0(levels, collapse=', ')}"))
  return(entity)
})


#' create_variable_category
#' 
#' Checks that all members of the `children` vector or list are values in variables$variable
#' 
#' Creates a new row in the variables metadata with
#'   data_type = 'category',
#'   has_values = FALSE
#' 
#' For all the children rows in `variables`, set parent_variable to `category_name`
#' 
#' @param entity an Entity object
#' @param category_name a string value internal name for the category
#' @param children a list or character vector of the internal names of the variables
#'        (or categories) that belong to the new category
#' @param ... metadata to add to the new category,
#'        e.g. display_name = 'my category', definition = 'my fave variables', etc 
#' 
#' @returns modified entity
#' @export
#' @details
#' - Fails if `category_name` already exists or if any `children` do not exist.
#' - Sets `data_type = 'category'` and `has_values = FALSE` for the new category.
#' - Updates `parent_variable` for all children.
#'
#' @examples
#' entity <- entity %>%
#'   create_variable_category(
#'     category_name = "house_vars",
#'     children = c("Owns.property", "Construction.material"),
#'     display_name = "House-related",
#'     definition = "Things about the house"
#'   )
setMethod("create_variable_category", "Entity", function(entity, category_name, children, ...) {
  global_varname = find_global_varname(entity, "entity")
  variables <- entity@variables

  # 0. Check it's not already in existence
  if (category_name %in% variables$variable) {
    stop(glue("Category cannot be created because a variable, category or ID column of the same name exists already."))
  }
  
  # 1. Check if all children exist
  missing_children <- setdiff(children, variables$variable)
  if (length(missing_children) > 0) {
    stop(glue("Category cannot be created because these children variables/categories do not exist: {paste(missing_children, collapse = ', ')}"))
  }
  
  # 2. Append a new row for the category
  new_row <- variable_metadata_defaults %>%
    mutate(
      has_values = FALSE,
      variable = category_name,
      data_type = factor('category'),
      entity_name = entity %>% get_entity_name()
    )
  variables <- bind_rows(variables, new_row)  

  # 3. set `parent_variable` to `category_name` for `children`
  variables <- variables %>%
    mutate(
      parent_variable = if_else(
        variable %in% children,
        category_name,
        parent_variable
      )
    )
  
  # update the entity with the modified variables
  entity@variables <- variables
  
  message(to_lines(
    glue("Successfully created category '{category_name}'. If you need to revert, use:"),
    indented(glue("{global_varname} <- {global_varname} %>% delete_variable_category('{category_name}')"))
  ))

  if (missing(...))
    return(entity)
  else
    return(entity %>% set_variable_metadata(category_name, ...))
})


#' delete_variable_category
#' 
#' Deletes a category from the variables metadata of an entity.
#' 
#' Ensures that the category is not referenced as a parent by any other variable or category.
#' If the category is safe to delete, it removes the corresponding row from the variables metadata
#' and clears the `parent_variable` field of any variables that reference the deleted category.
#' 
#' @param entity an Entity object
#' @param category_name a string value internal name of the category to be deleted
#' 
#' @returns modified entity
#' @export
setMethod("delete_variable_category", "Entity", function(entity, category_name) {
  global_varname <- find_global_varname(entity, "entity")
  variables <- entity@variables
  
  # 0. Check if the category exists
  if (!category_name %in% variables$variable) {
    stop(glue("Category '{category_name}' does not exist in this entity."))
  }
  
  # 1. Check if the category already has a parent
  is_orphan <- variables %>%
    filter(variable == category_name) %>%
    pull(parent_variable) %>%
    is.na() %>%
    all()
  if (!is_orphan) {
    stop(glue("Category '{category_name}' cannot be deleted because it belongs to another category."))
  }
  
  # 2. Remove the category from variables metadata
  variables <- variables %>% filter(variable != category_name)
  
  # 3. Clear `parent_variable` for variables referencing this category
  variables <- variables %>%
    mutate(
      parent_variable = if_else(parent_variable == category_name, NA_character_, parent_variable)
    )
  
  # Update the entity with the modified variables
  entity@variables <- variables
  
  message(glue("Category '{category_name}' has been deleted."))
  return(entity)
})


#' set_variables_multivalued
#' 
#' Sets the specified variables as multivalued by updating their metadata annotations.
#' Specifically, sets `is_multi_valued` to TRUE and assigns the provided `multi_value_delimiter`.
#' 
#' It also attempts to determine the appropriate data_type for the variable, based on
#' the expanded data.
#' 
#' @param entity An Entity object
#' @param ... Named arguments where names are variable names and values are their delimiters
#' 
#' @returns Modified entity
#' @export
setMethod("set_variables_multivalued", "Entity", function(entity, ...) {
  variables <- entity@variables
  data <- entity@data
  global_varname <- find_global_varname(entity, 'entity')

  # Parse named arguments
  multivalued_vars <- list(...)

  # Validate input
  if (is_empty(names(multivalued_vars))) {
    stop(to_lines(
      "Error: incorrect args for `set_variables_multivalued()`. Correct usage is:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variables_multivalued('variable.1.name' = 'delimiter.1', 'variable.2.name' = 'delimiter.2')"))
    ))
  }
  
  invalid_vars <- setdiff(names(multivalued_vars), variables$variable)
  if (length(invalid_vars) > 0) {
    stop(glue("The following variables do not exist: {paste(invalid_vars, collapse = ', ')}"))
  }
  
  ordinal_vars <- variables %>% filter(variable %in% names(multivalued_vars) & data_shape == 'ordinal')
  if (nrow(ordinal_vars) > 0) {
    stop(glue("The following variables cannot be multi-valued because they are ordinal: {paste(ordinal_vars$variable, collapse = ', ')}"))
  }

  # Update metadata
  variables <- variables %>%
    rowwise() %>%
    mutate(
      is_multi_valued = if_else(
        variable %in% names(multivalued_vars),
        TRUE,
        is_multi_valued
      ),
      multi_value_delimiter = if_else(
        variable %in% names(multivalued_vars),
        pluck(multivalued_vars, variable, .default = NA_character_),
        multi_value_delimiter
      ),
      # set type and shape to NA to trigger type/shape inference below
      data_type = fct_mutate(
        data_type,
        variable %in% names(multivalued_vars),
        NA
      ),
      data_shape = fct_mutate(
        data_shape,
        variable %in% names(multivalued_vars),
        NA
      ),
    ) %>%
    ungroup()

  # Update the entity
  entity@variables <- variables
  
  entity <- entity %>%
    infer_missing_data_types(.disallowed_data_types = c('id')) %>%
    infer_missing_data_shapes()
  
  if (!entity@quiet) {
    variable_names <- names(multivalued_vars)

    # get the data_types and data_shapes in same order as variable_names
    filtered_metadata <- entity %>%
      get_variable_metadata() %>%
      filter(variable %in% variable_names) %>%
      arrange(match(variable, variable_names)) %>%
      select(data_type, data_shape)
      
    data_types <- filtered_metadata %>% pull(data_type)
    data_shapes <- filtered_metadata %>% pull(data_shape)
    
    message(to_lines(
      glue("Successfully marked the following variables as multi-valued: {paste(variable_names, collapse = ', ')}"),
      glue("Their data_type/data_shape was detected as: {paste0(variable_names, ': ', data_types, '/', data_shapes, collapse=', ')}")
    ))
  }
  return(entity)
})

#' set_variables_univalued
#' 
#' Resets the specified variables as univalued by updating their metadata annotations.
#' Specifically, sets `is_multi_valued` to FALSE and removes the `multi_value_delimiter`.
#' 
#' @param entity An Entity object
#' @param variable_names A character vector of variable names to reset as univalued
#' 
#' @returns Modified entity
#' @export
setMethod("set_variables_univalued", "Entity", function(entity, variable_names) {
  variables <- entity@variables
  data <- entity@data
  
  # Validate input
  invalid_vars <- setdiff(variable_names, variables$variable)
  if (length(invalid_vars) > 0) {
    stop(glue("The following variables do not exist: {paste(invalid_vars, collapse = ', ')}"))
  }
  
  # Update metadata
  # setting data_type to NA so it can be re-inferred
  variables <- variables %>%
    mutate(
      is_multi_valued = if_else(variable %in% variable_names, FALSE, is_multi_valued),
      multi_value_delimiter = if_else(variable %in% variable_names, NA_character_, multi_value_delimiter),
      data_type = fct_mutate(
        data_type,
        variable %in% variable_names,
        NA
      ),
      data_shape = fct_mutate(
        data_shape,
        variable %in% variable_names,
        NA
      ),
    )

  # Update the entity
  entity@variables <- variables
  
  entity <- entity %>%
    infer_missing_data_types(.disallowed_data_types = c('id')) %>%
    infer_missing_data_shapes()
  
  if (!entity@quiet) {
    # get the data_types and data_shapes in same order as variable_names
    filtered_metadata <- entity %>%
      get_variable_metadata() %>%
      filter(variable %in% variable_names) %>%
      arrange(match(variable, variable_names)) %>%
      select(data_type, data_shape)
    
    data_types <- filtered_metadata %>% pull(data_type)
    data_shapes <- filtered_metadata %>% pull(data_shape)
    
    message(to_lines(
      glue("Successfully marked the following variables as uni-valued: {paste(variable_names, collapse = ', ')}"),
      glue("Their data_type/data_shape was detected as: {paste0(variable_names, ': ', data_types, '/', data_shapes, collapse=', ')}")
    ))
  }

  return(entity)
})


#' sync_ordinal_data
#'
#' Converts character columns to factors for `data_shape == 'ordinal'` variables,
#' applying `levels = ordinal_levels` from the metadata.
#'
#' @param entity an Entity object
#' @returns modified entity
#' @export
setMethod("sync_ordinal_data", "Entity", function(entity) {
  variables <- entity@variables
  data <- entity@data
  
  # Identify ordinal variables that are character columns in the data
  ordinal_vars <- variables %>%
    filter(data_shape == "ordinal") %>%
    pull(variable)
  
  # find the data columns that need to be converted to factors
  # (only character and integer allowed)
  suitable_ordinal_columns <-
    ordinal_vars[
      ordinal_vars %in% colnames(data) &
      map_lgl(data[ordinal_vars], ~ is.character(.x) | is.integer(.x))
    ]

  if (length(suitable_ordinal_columns) == 0) {
    if (!entity@quiet) message("No character columns detected for ordinal variables. No changes made.")
    return(entity)
  }
  
  # Retrieve ordinal levels from metadata
  ordinal_levels_map <- variables %>%
    filter(variable %in% suitable_ordinal_columns) %>%
    select(variable, ordinal_levels) %>%
    deframe() # Converts to named list
  
  # Apply conversion to factor with specified levels
  entity@data <- entity@data %>%
    mutate(across(all_of(suitable_ordinal_columns), ~ factor(.x, levels = ordinal_levels_map[[cur_column()]])))
  
  if (!entity@quiet) {
    message(glue(
      "Converted character columns to factors for ordinal variables: {paste(suitable_ordinal_columns, collapse = ', ')}"
    ))
  }
  
  return(entity)
})

