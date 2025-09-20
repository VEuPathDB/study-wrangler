#' Entity ID Validators
#'
#' Validation functions for entity ID columns and relationships.
#'
#' @name entity-validators-ids
NULL

#' Validator: Check for NA values in ID columns
#' @keywords internal
validate_entity_id_columns_no_na <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  id_columns <- variables %>% filter(data_type == "id") %>% pull(variable)
  
  if (length(id_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  na_in_ids <- data %>%
    select(all_of(id_columns)) %>%
    summarise(across(everything(), ~ any(is.na(.)))) %>%
    unlist() %>% as.logical()
  
  if (any(na_in_ids)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste("ID columns contain NA values:", 
                     paste(id_columns[na_in_ids], collapse = ", "))
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check for duplicate values in ID columns
#' @keywords internal
validate_entity_id_columns_no_duplicates <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  my_id_columns <- variables %>% 
    filter(data_type == "id") %>% 
    filter(entity_level == 0) %>% 
    pull(variable)
  
  if (length(my_id_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  dupes_in_ids <- data %>%
    select(all_of(my_id_columns)) %>%
    summarise(across(everything(), anyDuplicated)) %>%
    unlist() %>% as.logical()
  
  if (any(dupes_in_ids)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste("ID columns contain duplicates:", 
                     paste(my_id_columns[dupes_in_ids], collapse = ", "))
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check entity has required ID column
#' @keywords internal
validate_entity_has_id_column <- function(entity) {
  variables <- entity@variables
  
  my_id_variable <- variables %>%
    filter(data_type == 'id') %>%
    filter(entity_level == 0)
  
  if (nrow(my_id_variable) == 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = "This entity appears to have no ID column. It must have a column with a unique value in each row."
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check only one ID column per entity level
#' @keywords internal
validate_entity_one_id_per_level <- function(entity) {
  variables <- entity@variables
  
  id_col_contraventions <- variables %>%
    filter(data_type == "id") %>%
    group_by(entity_level) %>%
    filter(n() > 1) %>%
    summarise(id_columns = paste0(variable, collapse = ", "), .groups = "drop")
  
  if (nrow(id_col_contraventions) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "There are multiple ID columns per entity level:\n",
        paste(capture.output(kable(id_col_contraventions)), collapse = "\n"),
        "\nEntity level 0 is this entity. Level -1 is the parent entity, -2 is the grandparent, etc.",
        "\nIt is likely that one or more variable columns have been incorrectly detected as ID columns."
      )
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check ID column entity_name matches entity name
#' @keywords internal
validate_entity_id_entity_name_match <- function(entity) {
  variables <- entity@variables
  
  # Get contraventions to check if there are multiple ID columns at level 0
  id_col_contraventions <- variables %>%
    filter(data_type == "id") %>%
    group_by(entity_level) %>%
    filter(n() > 1) %>%
    summarise(id_columns = paste0(variable, collapse = ", "), .groups = "drop")
  
  my_id_variable <- variables %>%
    filter(data_type == 'id') %>%
    filter(entity_level == 0)
  
  # Only check if entity has name, no contraventions at level 0, and has ID variable
  if (!is.na(entity@name) &&
      entity@name != "" &&
      all(id_col_contraventions$entity_level != 0) &&
      nrow(my_id_variable) > 0) {
    
    if (my_id_variable %>% pull(entity_name) %>% coalesce("") != entity@name) {
      return(list(
        valid = FALSE,
        fatal = FALSE,
        message = paste0(
          "ID column '", my_id_variable %>% pull(variable), "' has incorrect `entity_name`.\n",
          "It is '", my_id_variable %>% pull(entity_name), "' and should be '", entity@name, "'"
        )
      ))
    }
  }
  
  list(valid = TRUE)
}