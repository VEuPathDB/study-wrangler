#' Basic Entity Validators
#'
#' Core validation functions for entity metadata, columns, and data integrity.
#'
#' @name entity-validators-basic
NULL

#' Validator: Check metadata is not empty
#' @keywords internal
validate_entity_metadata_not_empty <- function(entity) {
  variables <- entity@variables
  if (nrow(variables) == 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      "Variables' metadata is empty. Ensure metadata is correctly populated.",
      "To reset the metadata to defaults, use the following command:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% sync_variable_metadata()"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = message
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check data has columns
#' @keywords internal
validate_entity_data_has_columns <- function(entity) {
  data <- entity@data
  if (ncol(data) == 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = "Data contains no columns. Ensure data is correctly delimited and reload."
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check column alignment between data and metadata
#' @keywords internal
validate_entity_column_alignment <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  true_variables <- variables %>% filter(has_values) %>% pull(variable)
  missing_variables <- setdiff(colnames(data), true_variables)
  extra_variables <- setdiff(true_variables, colnames(data))
  
  issues <- c()
  fatal <- FALSE
  
  if (length(missing_variables) > 0) {
    issues <- c(issues, paste("Variable metadata is missing for these data columns:", 
                             paste(missing_variables, collapse = ", ")))
    fatal <- TRUE
  }
  
  if (length(extra_variables) > 0) {
    issues <- c(issues, paste("These variables have metadata but no data columns:",
                             paste(extra_variables, collapse = ", ")))
    fatal <- TRUE
  }
  
  if (length(issues) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      paste(issues, collapse = "\n"),
      "To synchronize variable metadata with data columns, use the following command:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% sync_variable_metadata()"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = fatal,
      message = message
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check for required metadata columns
#' @keywords internal
validate_entity_required_metadata <- function(entity) {
  variables <- entity@variables
  required_metadata_cols <- c('data_shape')
  
  variables_with_critical_NAs <- variables %>%
    filter(!data_type %in% c('id', 'category')) %>%
    select('variable', all_of(required_metadata_cols)) %>%
    filter(if_any(all_of(required_metadata_cols), is.na))
  
  if (nrow(variables_with_critical_NAs) > 0) {
    # Format the message to include variable details like the original
    message <- paste(
      "Error: NAs found in critical variable metadata columns:",
      paste(capture.output(kable(variables_with_critical_NAs)), collapse = "\n"),
      "~~~~",
      "This error is not expected to occur. Please contact the developers.",
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = message
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check metadata values respect types and factor levels
#' @keywords internal
validate_entity_metadata_types <- function(entity) {
  variables <- entity@variables
  
  validate_column <- function(col_name, col_values, default_val) {
    if (is.integer(default_val)) {
      bad <- col_values[!is.na(col_values) & !is.integer(col_values)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are integers", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.character(default_val)) {
      bad <- col_values[!is.na(col_values) & !is.character(col_values)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are character/string", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.logical(default_val)) {
      bad <- col_values[!is.na(col_values) & !(col_values %in% c(TRUE, FALSE))]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are logical", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.factor(default_val)) {
      lvls <- levels(default_val)
      bad <- col_values[!is.na(col_values) & !(col_values %in% lvls)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Contains values outside factor levels", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.list(default_val) && length(default_val) == 1 && is.factor(default_val[[1]])) {
      # list of factor values
      lvls <- levels(default_val[[1]])
      all_values <- col_values %>% unlist() %>% keep(~ !is.na(.x))
      bad <- all_values[!is.na(all_values) & !(all_values %in% lvls)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Contains values outside allowed multi-factor levels", Values = paste(unique(bad), collapse = ", ")))
    }
    NULL
  }
  
  issues <- names(variable_metadata_defaults) %>%
    map(~ validate_column(.x, variables[[.x]], variable_metadata_defaults[[.x]])) %>%
    compact() %>%
    bind_rows()

  if (nrow(issues) > 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = paste0(
        "Variable metadata contains illegal values:\n",
        paste(kable(issues), collapse = "\n")
      )
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check data_type is not NA for any variable
#' @keywords internal
validate_entity_data_type_not_na <- function(entity) {
  variables <- entity@variables
  
  missing_data_type <- variables %>%
    filter(is.na(data_type)) %>% pull(variable)
  
  if (length(missing_data_type) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      paste0("Metadata data_type must not be NA for these variables:\n",
             paste(missing_data_type, collapse = ", ")),
      "To re-detect data types for these variables, use the following command:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% redetect_columns_as_variables(c('", 
             paste(missing_data_type, collapse = "', '"), "'))"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = message
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check entity has a name
#' @keywords internal
validate_entity_has_name <- function(entity) {
  if (is.na(entity@name) || entity@name == '') {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      "Entity is missing required 'name' metadata",
      "To set an entity name, use the following command (replace 'my_entity' with an appropriate name):",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_entity_name('my_entity')"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = message
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check for units on non-numeric variables
#' @keywords internal
validate_entity_units_on_numeric_only <- function(entity) {
  non_numeric_vars_with_units <- entity %>%
    get_variable_metadata() %>%
    filter(
      !data_type %in% c("integer", "number"),
      data_shape != "ordinal",  # Allow units for ordinal variables
      !is.na(unit)
    ) %>%
    pull(variable)
  
  if (length(non_numeric_vars_with_units) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create commands to remove units from each variable
    fix_commands <- paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('", 
                          non_numeric_vars_with_units, "', unit = NA)")
    
    message <- paste(
      paste0("These non-numeric variables should not have units: ", 
             paste0(non_numeric_vars_with_units, collapse=', ')),
      "To remove units from these variables, use the following commands:",
      paste(fix_commands, collapse = "\n"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = message
    ))
  }

  list(valid = TRUE)
}

#' Validator: Check for unique generated stable_ids
#' @keywords internal
validate_entity_unique_stable_ids <- function(entity) {
  # Get variable and category metadata (just the variable names and existing stable_ids)
  metadata <- entity@variables %>%
    filter(data_type != 'id')

  if (nrow(metadata) == 0) {
    return(list(valid = TRUE))
  }

  # Generate stable_ids using the same logic as get_hydrated_variable_and_category_metadata()
  # but without computing expensive stats
  generated_stable_ids <- metadata %>%
    rowwise() %>%
    mutate(
      generated_stable_id = if_else(
        is.na(stable_id),
        prefixed_alphanumeric_id(prefix = "VAR_", length = 8, seed_string = variable),
        stable_id
      )
    ) %>%
    ungroup()

  # Check for duplicates in the generated stable_ids
  duplicate_ids <- generated_stable_ids %>%
    group_by(generated_stable_id) %>%
    filter(n() > 1) %>%
    arrange(generated_stable_id) %>%
    select(variable, generated_stable_id) %>%
    ungroup()

  if (nrow(duplicate_ids) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')

    # Create a summary of duplicates
    duplicate_summary <- duplicate_ids %>%
      group_by(generated_stable_id) %>%
      summarise(
        variables = paste(variable, collapse = ", "),
        .groups = "drop"
      )

    message <- paste(
      "Duplicate stable_ids detected in variable metadata.",
      "This can occur when variable names hash to the same ID or have identical names.",
      "",
      "Affected variables:",
      paste(capture.output(kable(duplicate_summary)), collapse = "\n"),
      "",
      "To resolve this, you must manually set unique stable_ids for affected variables:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('variable_name', stable_id = 'VAR_unique_id')"),
      sep = "\n"
    )

    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = message
    ))
  }

  list(valid = TRUE)
}