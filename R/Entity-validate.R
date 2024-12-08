#' Validate Generic
#'
#' Defines the S4 generic for the validate function.
#' 
#' @param entity The object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setGeneric("validate", function(entity, ...) standardGeneric("validate"))

#' Validate an Entity Object
#'
#' Performs various checks on data completeness and consistency 
#' 
#'
#' @param entity An Entity object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Entity", function(entity, quiet = FALSE) {
  # Extract data and variables
  data <- entity@data
  variables <- entity@variables

  # Initialize validation results and messages
  is_valid <- TRUE
  feedback <- character()

  add_feedback <- function(message) {
    # special <<- operator updates variable in "parent context"
    feedback <<- c(feedback, message)
    is_valid <<- FALSE
  }
    
  give_feedback <- function(fatal_message = NULL) {
    if (quiet) return()
    if (length(feedback) > 0) {
      message("Validation issues found:\n", paste(feedback, collapse = "\n"))
    }
    if (is.character(fatal_message)) {
      warning("Fatal issue encountered:\n", fatal_message)
    } else if (length(feedback) == 0) {
      message("Entity is valid.")
    }
  }
  
  # Fatal Validation 1: Check if metadata is empty
  if (nrow(variables) == 0) {
    give_feedback(fatal_message = "Variables' metadata is empty. Ensure metadata is correctly populated.")
    return(FALSE)
  }
  
  # Fatal Validation 2: Check if data has no columns
  if (ncol(data) == 0) {
    give_feedback(fatal_message = "Data contains no columns. Ensure data is correctly formatted.")
    return(FALSE)
  }
  
  # Validation 3: Check column alignment
  missing_variables <- setdiff(colnames(data), variables$variable)
  extra_variables <- setdiff(variables$variable, colnames(data))
  
  if (length(missing_variables) > 0) {
    add_feedback(paste(
      "Variable metadata is missing for these data columns:",
       paste(missing_variables, collapse = ", "),
      "\n[add default metadata with `entity <- entity %>% sync_variable_metadata()`]"
    ))
  }
  
  if (length(extra_variables) > 0) {
    add_feedback(paste(
      "These variables have metadata but no data columns:",
      paste(extra_variables, collapse = ", "),
      "\n[remove the metadata with `entity <- entity %>% sync_variable_metadata()`]"
    ))
  }

  # Validation 4: Check for NA values in 'id' columns
  id_columns <- variables$variable[variables$data_type == "id"]
  na_in_ids <- sapply(data[id_columns], function(col) sum(is.na(col)))
  
  if (any(na_in_ids > 0)) {
    add_feedback(paste("ID columns contain NA values:", 
                       paste(names(id_columns)[na_in_ids > 0], collapse = ", ")))
  }
  
  # Validation 5: Check for duplicated values in 'id' columns
  dupes_in_ids <- sapply(data[id_columns], anyDuplicated)
  if (any(dupes_in_ids)) {
    add_feedback(paste("ID columns contain duplicates:", 
                       paste(names(id_columns)[dupes_in_ids > 0], collapse = ", ")))
  }
  
  # Output feedback to the user
  give_feedback()  
  
  # Return overall validation status
  return(is_valid)
})
