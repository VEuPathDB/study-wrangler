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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These columns are declared as 'integer' but contain non-integer values: ",
        paste(problem_columns, collapse = ", ")
      )
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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These columns are declared as 'number' but contain non-numeric values: ",
        paste(problem_columns, collapse = ", ")
      )
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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These columns are declared as 'date' but R does not currently recognise them as dates: ",
        paste(problem_columns, collapse = ", ")
      )
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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These columns are declared as multi-valued but their R data type is not 'character': ",
        paste(problem_columns, collapse = ", ")
      )
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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These variables have data_shape 'ordinal' but their data columns are not R factors: ",
        paste(problem_columns, collapse = ", ")
      )
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
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste(ordinal_issues, collapse = "\n")
    ))
  }
  
  list(valid = TRUE)
}