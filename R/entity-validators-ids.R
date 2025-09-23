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
  id_columns <- entity %>% get_id_column_metadata() %>% pull(variable)
  
  if (length(id_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  na_in_ids <- data %>%
    select(all_of(id_columns)) %>%
    summarise(across(everything(), ~ any(is.na(.)))) %>%
    unlist() %>% as.logical()
  
  if (any(na_in_ids)) {
    message <- paste(
      paste("ID columns contain NA values:", 
            paste(id_columns[na_in_ids], collapse = ", ")),
      "This indicates a data quality issue that requires manual inspection.",
      "ID columns must have a unique, non-NA value for every row.",
      "Consider:",
      "  - Filtering out rows with NA values in ID columns",
      "  - Finding or creating a different column to use as the ID",
      "  - Generating a new serial ID column if no suitable ID exists",
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

#' Validator: Check for duplicate values in ID columns
#' @keywords internal
validate_entity_id_columns_no_duplicates <- function(entity) {
  data <- entity@data
  my_id_columns <- entity %>% get_id_column_metadata() %>% 
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
    message <- paste(
      paste("ID columns contain duplicates:", 
            paste(my_id_columns[dupes_in_ids], collapse = ", ")),
      "This indicates that the column may not be suitable as an ID column.",
      "ID columns must have unique values for every row.",
      "Consider:",
      "  - Using a different column that has unique values",
      "  - Creating a composite key from multiple columns",
      "  - Generating a new serial ID column if no suitable unique column exists",
      "  - Investigating why duplicates exist in your data",
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

#' Validator: Check entity has required ID column
#' @keywords internal
validate_entity_has_id_column <- function(entity) {
  my_id_variable <- entity %>% get_id_column_metadata() %>%
    filter(entity_level == 0)
  
  if (nrow(my_id_variable) == 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      "This entity appears to have no ID column.",
      "It must have a column with a unique value in each row.",
      "You can create a simple numeric ID using the convenience function:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% create_serial_id_column('ID')"),
      "Or manually create one as follows:",
      paste0("    ", global_varname, " <- ", global_varname, " %>%"),
      paste0("        modify_data(mutate(ID = row_number())) %>%"),
      paste0("        sync_variable_metadata() %>%"),
      paste0("        redetect_column_as_id('ID')"),
      paste0("Then `validate(", global_varname, ")` again"),
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

#' Validator: Check only one ID column per entity level
#' @keywords internal
validate_entity_one_id_per_level <- function(entity) {
  id_columns <- entity %>% get_id_column_metadata()
  
  id_col_contraventions <- id_columns %>%
    group_by(entity_level) %>%
    filter(n() > 1) %>%
    summarise(id_columns = paste0(variable, collapse = ", "), .groups = "drop")
  
  if (nrow(id_col_contraventions) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    message <- paste(
      "There are multiple ID columns per entity level:",
      paste(capture.output(kable(id_col_contraventions)), collapse = "\n"),
      "",
      "Entity level 0 is this entity. Level -1 is the parent entity, -2 is the grandparent, etc.",
      "It is likely that one or more variable columns have been incorrectly detected as ID columns.",
      "",
      "To fix incorrect ID detection, re-detect the problematic columns as regular variables:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% redetect_columns_as_variables(c('column1', 'column2'))"),
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

#' Validator: Check ID column entity_name matches entity name
#' @keywords internal
validate_entity_id_entity_name_match <- function(entity) {
  id_columns <- entity %>% get_id_column_metadata()
  
  # Get contraventions to check if there are multiple ID columns at level 0
  id_col_contraventions <- id_columns %>%
    group_by(entity_level) %>%
    filter(n() > 1) %>%
    summarise(id_columns = paste0(variable, collapse = ", "), .groups = "drop")
  
  my_id_variable <- id_columns %>%
    filter(entity_level == 0)
  
  # Only check if entity has name, no contraventions at level 0, and has ID variable
  if (!is.na(entity@name) &&
      entity@name != "" &&
      all(id_col_contraventions$entity_level != 0) &&
      nrow(my_id_variable) > 0) {
    
    if (my_id_variable %>% pull(entity_name) %>% coalesce("") != entity@name) {
      # Get global variable name for fix-it suggestions
      global_varname <- find_global_varname(entity, 'entity')
      id_column_name <- my_id_variable %>% pull(variable)
      
      message <- paste(
        paste0("ID column '", id_column_name, "' has incorrect `entity_name`."),
        paste0("It is '", my_id_variable %>% pull(entity_name), "' and should be '", entity@name, "'"),
        "To fix the entity_name in the ID column metadata, use:",
        paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('", 
               id_column_name, "', entity_name = '", entity@name, "')"),
        sep = "\n"
      )
      
      return(list(
        valid = FALSE,
        fatal = FALSE,
        message = message
      ))
    }
  }
  
  list(valid = TRUE)
}