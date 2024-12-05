#' Validate Generic
#'
#' Defines the S4 generic for the validate function.
#' 
#' @param object The object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setGeneric("validate", function(object) standardGeneric("validate"))

#' Validate an Entity Object
#'
#' Performs various checks on data completeness and consistency 
#' 
#'
#' @param object An Entity object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Entity", function(object) {
  # Extract data and metadata
  data <- object@data
  metadata <- object@metadata
  
  # Initialize validation results and messages
  is_valid <- TRUE
  feedback <- character()

  add_feedback <- function(message) {
    # special <<- operator updates variable in "parent context"
    feedback <<- c(feedback, message)
    is_valid <<- FALSE
  }
    
  give_feedback <- function(fatal_message = NULL) {
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
  if (nrow(metadata) == 0) {
    give_feedback(fatal_message = "Metadata is empty. Ensure metadata is correctly populated.")
    return(FALSE)
  }
  
  # Fatal Validation 2: Check if data has no columns
  if (ncol(data) == 0) {
    give_feedback(fatal_message = "Data contains no columns. Ensure data is correctly formatted.")
    return(FALSE)
  }
  
  # Validation 3: Check column alignment
  missing_columns <- setdiff(colnames(data), metadata$variable)
  extra_metadata <- setdiff(metadata$variable, colnames(data))
  
  if (length(missing_columns) > 0) {
    add_feedback(paste("Data columns missing in metadata:", paste(missing_columns, collapse = ", ")))
  }
  
  if (length(extra_metadata) > 0) {
    add_feedback(paste("Metadata rows missing in data columns:", paste(extra_metadata, collapse = ", ")))
  }
  
  # Validation 4: Check for NA values in 'id' columns
  id_columns <- metadata$variable[metadata$data_type == "id"]
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
