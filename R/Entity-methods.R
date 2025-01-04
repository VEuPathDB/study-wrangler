library(glue)

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
setGeneric("set_variable_metadata", function(entity, ...) standardGeneric("set_variable_metadata"))
#' @export
setGeneric("get_variable_metadata", function(entity, ...) standardGeneric("get_variable_metadata"))
#' @export
setGeneric("get_id_column_metadata", function(entity, ...) standardGeneric("get_id_column_metadata"))
#' @export
setGeneric("get_data", function(entity) standardGeneric("get_data"))
#' @export
setGeneric("modify_data", function(entity, ...) standardGeneric("modify_data"))
#' @export
setGeneric("set_variable_display_names_from_provider_labels", function(entity) standardGeneric("set_variable_display_names_from_provider_labels"))
#' @export
setGeneric("set_parents", function(entity, names, columns) standardGeneric("set_parents"))
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
          data,
          variable,
          .allowed_data_types = .allowed_data_types,
          .disallowed_data_types = .disallowed_data_types)
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
    message(glue("Generating temporary stable_id for entity '{entity@name}'"))
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
    tibble(variable = character(0))
  }

  missing_variables <- setdiff(colnames(data), variables$variable)
  extra_variables <- setdiff(variables$variable, colnames(data))

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
        provider_label = missing_variables,
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
  if (!entity@quiet) message(glue("Made metadata update(s) to '{names(updates)}' for '{variable_name}'"))

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
  mask <- variables %>% transmute(data_type != 'id' & is.na(display_name)) %>% pull()

  # Update the display_name for rows matching the mask
  variables <- variables %>%
    mutate(display_name = if_else(mask, provider_label, display_name))
  
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
setMethod("set_parents", "Entity", function(entity, names, columns) {
  data <- entity@data
  variables <- entity@variables

  # Early return if `names` and `columns` are empty
  if (length(names) == 0 && length(columns) == 0) {
    if (!entity@quiet) message("No parent entity relationships provided. No changes made.")
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
  columns <- parent_metadata$variable
  
  return(list(names = names, columns = columns))
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
  if (length(parents$columns) > 0) {
    return(parents$columns[1])
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
#' @param entity
#' @returns vector of character (lines to print or cat)
#' @export
setMethod("pretty_tree", "Entity", function(entity) {
  # This will be our main entry point.
  # We'll call a helper function that recursively gathers all lines.
  lines <- format_entity(entity, prefix = "", is_last = TRUE, is_root = TRUE)
  return(lines)
})

#'
#' helper for pretty_tree
#'
format_entity <- function(entity, prefix, is_last, is_root = FALSE) {
  # Determine the prefix for this entity line
  line_prefix <- if (is_root) {
    prefix
  } else if (is_last) {
    paste0(prefix, "└── ")
  } else {
    paste0(prefix, "├── ")
  }
  
  this_line <- paste0(line_prefix, get_entity_name(entity))
  
  # For children, we need to decide on the next prefix. If this entity is the last child,
  # the next prefix for its children is prefix + "    " (4 spaces),
  # otherwise it's prefix + "|   ".
  children_prefix <- if (is_root) {
    prefix
  }  else if (is_last) {
    paste0(prefix, "    ")
  } else {
    paste0(prefix, "│   ")
  }
  
  children <- get_children(entity)
  
  if (length(children) == 0) {
    # No children, just return the current line.
    return(this_line)
  } else {
    # Recursively format each child. The last child's is_last = TRUE.
    lines_for_children <- mapply(
      FUN = function(child, is_last_child) {
        format_entity(child, prefix = children_prefix, is_last = is_last_child)
      },
      child = children,
      is_last_child = seq_along(children) == length(children),
      SIMPLIFY = FALSE
    )
    
    # Combine this entity's line with all children's lines.
    return(c(this_line, unlist(lines_for_children)))
  }
}


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
  # Pre-check and convert column
  data <- entity@data
  data <- check_and_convert_to_date(data, column_name)
  
  # Update metadata and return the modified entity
  entity <- entity %>%
    set_variable_metadata(column_name, data_type = "date") %>%
    initialize(data = data)
  
  return(entity)
})
