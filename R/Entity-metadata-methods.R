library(glue)

#' File: Entity metadata management methods
#'
#' Generics (attempting to remove redundancy from documentation)
#' 
setGeneric("infer_missing_data_types", function(entity, ...) standardGeneric("infer_missing_data_types"))
setGeneric("infer_missing_data_shapes", function(entity) standardGeneric("infer_missing_data_shapes"))
#' @export
setGeneric("redo_type_detection_as_variables_only", function(entity, columns) standardGeneric("redo_type_detection_as_variables_only"))
#' @export
setGeneric("set_entity_metadata", function(entity, ...) standardGeneric("set_entity_metadata"))
#' @export
setGeneric("set_entity_name", function(entity, name) standardGeneric("set_entity_name"))
#' @export
setGeneric("sync_variable_metadata", function(entity) standardGeneric("sync_variable_metadata"))
#' @export
setGeneric("set_variable_metadata", function(entity, ...) standardGeneric("set_variable_metadata"))
#' @export
setGeneric("get_variable_metadata", function(entity, ...) standardGeneric("get_variable_metadata"))
#' @export
setGeneric("get_id_column_metadata", function(entity, ...) standardGeneric("get_id_column_metadata"))
#' @export
setGeneric("get_data", function(entity) standardGeneric("get_data"))
#' @export
setGeneric("set_data", function(entity, ...) standardGeneric("set_data"))
#' @export
setGeneric("set_variable_display_names_from_provider_labels", function(entity) standardGeneric("set_variable_display_names_from_provider_labels"))
#' @export
setGeneric("set_parents", function(entity, names, columns) standardGeneric("set_parents"))



#' infer_missing_data_types
#' 
#' Infers `data_type` metadata for columns where this is currently `NA`.
#' 
#' @param entity an Entity object.
#' @param .no_id_check logical, optional. If `TRUE`, skips the `n_distinct()` check in `infer_data_type`, 
#' and no columns will be inferred as `id`. Defaults to `FALSE`.
#' @returns modified entity.
setMethod("infer_missing_data_types", "Entity", function(entity, .no_id_check = FALSE) {
  
  variables <- entity@variables
  data <- entity@data
  
  # use infer_column_data_type(data_column) to fill in NAs in variables$data_type column
  variables <- variables %>%
    rowwise() %>% # performance is not critical here
    mutate(
      data_type = fct_mutate(
        data_type,
        is.na(data_type),
        infer_data_type(data, variable, .no_id_check = .no_id_check),
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
  data <- entity@data
  variables <- entity@variables
  
  # only infer for data_shape == NA and non-ID cols
  cols_to_infer <- variables %>%
    filter(is.na(data_shape)) %>%
    filter(!is.na(data_type)) %>%
    filter(data_type != 'id') %>%
    pull(variable)
  
  # infer data_shape as continuous or categorical
  # (further refinement to 'ordinal' will require user-input)
  variables <- variables %>% mutate(
    data_shape = if_else(
      variable %in% cols_to_infer,
      case_when(
        data_type %in% c('number', 'integer', 'date') ~ 'continuous',
        .default = 'categorical'
      ),
      data_shape
    )
  )

  # mutate non-continuous columns into factors only for
  # columns that we previously inferred (`cols_to_infer`)
  factor_vars <- variables %>%
    filter(variable %in% cols_to_infer) %>%
    filter(data_shape != "continuous") %>% pull(variable)

  data <- data %>%
    mutate(across(all_of(factor_vars), as.factor))
  
  # # Set the vocabulary metadata for the factor variables
  # variables <- variables %>%
  #   rowwise() %>% # for simplicity, not speed
  #   mutate(vocabulary = list(
  #     if (variable %in% factor_vars) {
  #       levels(data[[variable]])
  #     } else {
  #       vocabulary
  #     }
  #   )) %>%
  #   ungroup()
  
  # clone and modify original entity argument
  return(entity %>% initialize(data=data, variables=variables))
})


#' redo_type_detection_as_variables_only
#' 
#' Redoes the automatic type detection for specific columns but will never
#' guess ID type for those columns
#'  
#' @param entity an Entity object
#' @param columns a character vector of column names
#' @returns modified entity
setMethod("redo_type_detection_as_variables_only", "Entity", function(entity, columns) {
  data <- entity@data
  variables <- entity@variables

  # Early return if `columns` is empty
  if (missing(columns) || length(columns) == 0) {
    message("No column names provided. No changes made.")
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
  message("Redoing type detection")
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
    infer_missing_data_types(.no_id_check=TRUE) %>%
    infer_missing_data_shapes()

  return(entity)
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
  validate_entity_metadata_names(metadata)
  
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
    
    message(glue("Adding entity name '{name}'..."))
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
  variables <- entity@variables
  
  missing_variables <- setdiff(colnames(data), variables$variable)
  extra_variables <- setdiff(variables$variable, colnames(data))

  # Early return if no mismatches
  if (length(missing_variables) == 0 && length(extra_variables) == 0) {
    message("No metadata synchronization needed.")
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
        provider_label = missing_variables,
        entity_name = entity@name,
      )

    variables <- bind_rows(variables, missing_metadata)
    message(paste(
      "Synced variables metadata by adding defaults for:",
      paste(missing_variables, collapse = ", ")
    ))
  }
    
  if (length(extra_variables)) {
    variables <- variables %>%
      filter(!variable %in% extra_variables)
    message(paste(
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
#' Sets metadata for a named variable
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
    
  # Update the specified row and columns
  walk2(
    names(updates), 
    updates,
    \(x, y) variables[row_number, x] <<- y
  )
  message(glue("Made metadata update(s) to '{names(updates)}' for '{variable_name}'"))

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
#' @export
setMethod("get_variable_metadata", "Entity", function(entity, ...) {
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
#' @export
setMethod("get_id_column_metadata", "Entity", function(entity, ...) {
  return(
    entity@variables %>%
      filter(data_type == 'id') %>%
      select(variable, starts_with('entity_')) %>%
      arrange(entity_level)
  )
})

#' get_data
#' 
#' Returns the data tibble as-is
#' 
#' Treat this as read-only. If you need to make changes to the data use `set_data()`
#' 
#' @param entity an Entity object
#' @returns data tibble
#' @export
setMethod("get_data", "Entity", function(entity) {
  return(entity@data)
})


#' set_data
#' 
#' Returns a new entity object with data modified by the pipeline passed to it.
#' This allows tidyverse-style manipulation of an Entity object's `data` slot.
#' 
#' Example usage:
#' ```R
#' households <- households %>%
#'   set_data(mutate(Construction.material = fct_recode(Construction.material, 'Concrete' = 'Concrte')))
#' ```
#' 
#' @param entity an Entity object
#' @param ... a tidyverse pipeline to modify the data slot of the entity object
#' @returns a new Entity object with the modified data slot
#' @export
setMethod("set_data", "Entity", function(entity, ...) {
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
  mask <- variables %>% transmute(data_type != 'id' & is.na(display_name)) %>% pull()

  # Update the display_name for rows matching the mask
  variables <- variables %>%
    mutate(display_name = if_else(mask, provider_label, display_name))
  
  message(glue("Copied provider_label over to display_name for {sum(mask, na.rm = TRUE)} variables"))
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
setMethod("set_parents", "Entity", function(entity, names, columns) {
  data <- entity@data
  variables <- entity@variables

  # Early return if `names` and `columns` are empty
  if (length(names) == 0 && length(columns) == 0) {
    message("No parent entity relationships provided. No changes made.")
    return(entity)
  }

  # Check that length of `names` and `columns` are the same
  if (length(names) != length(columns)) {
    stop("Error: 'names' and 'columns' must have the same length.")
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
  
  # Generalized mutation to update `variables`
  variables <- variables %>%
    mutate(
      data_type = fct_mutate(data_type, variable %in% columns, 'id'),
      entity_name = if_else(variable %in% columns, names[match(variable, columns)], entity_name),
      entity_level = if_else(variable %in% columns, -match(variable, columns), entity_level)
    )
  
  message("Parent entity relationships and columns have been set")
  
  # Return modified entity
  return(entity %>% initialize(variables = variables))
})
