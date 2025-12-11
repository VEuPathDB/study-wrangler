#' Entity Data Type Validators
#'
#' Validation functions for entity data type consistency.
#'
#' @name entity-validators-data-types
NULL

#' Validator: Check integer variables contain integers
#' @keywords internal
validate_entity_integer_data_types <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  integer_columns <- variables %>%
    filter(data_type == "integer" & !is_multi_valued) %>% pull(variable)
  
  if (length(integer_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  not_integers <- data %>%
    select(all_of(integer_columns)) %>%
    summarise(
      across(
        everything(),
        # we allow factors as long as they are all-integer
        ~ !(is.integer(.) | (is.factor(.) & all(as.integer(.) == ., na.rm = TRUE)))
      )
    ) %>%
    unlist() %>% as.logical()
  
  if (any(not_integers)) {
    problem_columns <- integer_columns[not_integers]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create individual messages for each problematic column (like original)
    messages <- sapply(problem_columns, function(col_name) {
      paste0("The column '", col_name, "' is declared as 'integer' but contains non-integer values.")
    })
    
    message <- paste(
      paste(messages, collapse = "\n"),
      "To re-detect data types for these columns (may change to 'number'), use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% redetect_columns_as_variables(c('", 
             paste(problem_columns, collapse = "', '"), "'))"),
      "Or to explicitly change metadata to 'number' type, use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata(c('", 
             paste(problem_columns, collapse = "', '"), "'), data_type = 'number')"),
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

#' Validator: Check number variables contain numeric values
#' @keywords internal
validate_entity_number_data_types <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  number_columns <- variables %>%
    filter(data_type == "number" & !is_multi_valued) %>% pull(variable)
  
  if (length(number_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  not_numbers <- data %>%
    select(all_of(number_columns)) %>%
    summarise(across(everything(), ~ !is.numeric(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_numbers)) {
    problem_columns <- number_columns[not_numbers]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create individual messages for each problematic column (like original)
    messages <- sapply(problem_columns, function(col_name) {
      paste0("The column '", col_name, "' is declared as 'number' but contains non-numeric values.")
    })
    
    message <- paste(
      paste(messages, collapse = "\n"),
      "To re-detect data types for these columns, use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% redetect_columns_as_variables(c('", 
             paste(problem_columns, collapse = "', '"), "'))"),
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

#' Validator: Check date variables are R date type
#' @keywords internal
validate_entity_date_data_types <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  date_columns <- variables %>%
    filter(data_type == "date" & !is_multi_valued) %>% pull(variable)

  if (length(date_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  not_dates <- data %>%
    select(all_of(date_columns)) %>%
    summarise(across(everything(), ~ !is.Date(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_dates)) {
    problem_columns <- date_columns[not_dates]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create individual messages for each problematic column (like original)
    messages <- sapply(problem_columns, function(col_name) {
      paste0("The column '", col_name, "' is declared as 'date' but R does not currently recognise it as a date.")
    })
    
    # Create individual fix commands for each column
    fix_commands <- sapply(problem_columns, function(col_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% modify_data(mutate(", 
             col_name, " = as.Date(", col_name, ")))")
    })
    
    message <- paste(
      paste(messages, collapse = "\n"),
      "To convert these columns to Date type, use:",
      paste(fix_commands, collapse = "\n"),
      "Note: You may need to wrangle non-standard and/or potentially ambiguous date formats in addition to the `as.Date()` casting.",
      "For example, if dates are in MM-DD-YYYY format, use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% modify_data(mutate(", 
             problem_columns[1], " = as.Date(", problem_columns[1], ", format = '%m-%d-%Y')))"),
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

#' Validator: Check multi-valued variables are character type
#' @keywords internal
validate_entity_multi_valued_character <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  multi_valued_columns <- variables %>%
    filter(is_multi_valued) %>% pull(variable)
  
  if (length(multi_valued_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  not_character_cols <- data %>%
    select(all_of(multi_valued_columns)) %>%
    summarise(across(everything(), ~ !is.character(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_character_cols)) {
    problem_columns <- multi_valued_columns[not_character_cols]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create commands to remove multi-valued metadata from each variable
    fix_commands <- paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('", 
                          problem_columns, "', is_multi_valued = FALSE, multi_value_delimiter = NA)")
    
    message <- paste(
      paste0("These columns are declared as multi-valued but their R data type is not 'character': ",
             paste(problem_columns, collapse = ", ")),
      "To remove multi-valued metadata from these variables, use:",
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

#' Validator: Check ordinal variables are factors
#' @keywords internal
validate_entity_ordinal_factors <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  factor_columns <- variables %>%
    filter(data_shape == "ordinal") %>% pull(variable)
  
  if (length(factor_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  not_factors <- data %>%
    select(all_of(factor_columns)) %>%
    summarise(across(everything(), ~ !is.factor(.))) %>%
    unlist() %>% as.logical()

  if (any(not_factors)) {
    problem_columns <- factor_columns[not_factors]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')

    # TO DO: probably recommend set_variable_ordinal_levels() instead    
    # Create individual fix commands for each column
    fix_commands <- sapply(problem_columns, function(col_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% modify_data(mutate(", 
             col_name, " = as.factor(", col_name, ")))")
    })
    
    message <- paste(
      paste0("These variables have data_shape 'ordinal' but their data columns are not R factors: ",
             paste(problem_columns, collapse = ", ")),
      "To convert these columns to factors, use:",
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

#' Validator: Check string variables contain character values
#' @keywords internal
validate_entity_string_data_types <- function(entity) {
  data <- entity@data
  variables <- entity@variables

  # For string type variables, allow factors ONLY if they're ordinal
  # Otherwise they must be character
  string_columns <- variables %>%
    filter(data_type == "string" & !is_multi_valued & data_shape != "ordinal") %>% pull(variable)

  if (length(string_columns) == 0) {
    return(list(valid = TRUE))
  }

  not_strings <- data %>%
    select(all_of(string_columns)) %>%
    summarise(across(everything(), ~ !is.character(.))) %>%
    unlist() %>% as.logical()

  if (any(not_strings)) {
    problem_columns <- string_columns[not_strings]
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')

    # Create individual messages for each problematic column
    messages <- sapply(problem_columns, function(col_name) {
      actual_type <- class(data[[col_name]])[1]
      paste0("The column '", col_name, "' is declared as 'string' but contains ", actual_type, " values.")
    })

    # Create individual fix commands for each column
    fix_commands <- sapply(problem_columns, function(col_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% modify_data(mutate(",
             col_name, " = as.character(", col_name, ")))")
    })

    message <- paste(
      paste(messages, collapse = "\n"),
      "To convert these columns to character type, use:",
      paste(fix_commands, collapse = "\n"),
      "Or to re-detect data types for these columns (may change to 'integer' or 'number'), use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% redetect_columns_as_variables(c('",
             paste(problem_columns, collapse = "', '"), "'))"),
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

#' Validator: Check string variables have appropriate data_shape
#' @keywords internal
validate_entity_string_data_shapes <- function(entity) {
  variables <- entity@variables

  # String variables should only have data_shape: categorical, ordinal, or binary
  # NOT continuous (which is only for number, integer, date)
  invalid_string_shapes <- variables %>%
    filter(data_type == "string" & data_shape == "continuous") %>%
    pull(variable)

  if (length(invalid_string_shapes) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')

    # Create individual messages for each problematic column
    messages <- sapply(invalid_string_shapes, function(col_name) {
      paste0("Variable '", col_name, "' has data_type 'string' but data_shape 'continuous'. String variables should have data_shape 'categorical', 'ordinal', or 'binary'.")
    })

    # Create individual fix commands for each column
    fix_commands <- sapply(invalid_string_shapes, function(col_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('",
             col_name, "', data_shape = 'categorical')")
    })

    message <- paste(
      paste(messages, collapse = "\n"),
      "To fix these variables, set an appropriate data_shape:",
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

#' Validator: Check ordinal levels consistency
#' @keywords internal
validate_entity_ordinal_levels <- function(entity) {
  variables <- entity@variables
  
  ordinal_issues <- variables %>%
    mutate(
      num_levels = map_int(ordinal_levels, length),
      issue = case_when(
        data_shape == 'ordinal' & !data_type %in% c("integer", "string") ~ 
          paste0("Variable '", variable, "' data_shape 'ordinal' is not compatible with data_type '", data_type, "'"), 
        data_shape == 'ordinal' & num_levels == 0 ~ 
          paste0("Ordinal variable '", variable, "' has no ordinal_levels defined but requires them."),
        data_shape != 'ordinal' & num_levels > 0 ~ 
          paste0("Non-ordinal variable '", variable, "' has ", num_levels, " ordinal_levels but should have none."),
        TRUE ~ "OK"
      )
    ) %>%
    filter(issue != 'OK') %>%
    pull(issue)
  
  if (length(ordinal_issues) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      paste(ordinal_issues, collapse = "\n"),
      "To create an ordinal variable without these issues, use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_ordinal_levels('variable_name', c('level1', 'level2', 'level3'))"),
      "Or to start from scratch (reset ordinal metadata and re-detect), use:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('variable_name', ordinal_levels = list()) %>% redetect_columns_as_variables('variable_name')"),
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