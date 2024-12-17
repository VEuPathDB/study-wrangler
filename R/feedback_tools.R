create_feedback_tools <- function(quiet = FALSE) {
  # Internal state, private to the closures
  is_valid <- TRUE
  feedback <- character()
  
  add_feedback <- function(message) {
    feedback <<- c(feedback, message)
    is_valid <<- FALSE
  }
  
  give_feedback <- function(fatal_message = NULL) {
    if (!quiet && length(feedback) > 0) {
      message("Validation issues found:\n", paste(feedback, collapse = "\n"))
    }
    if (is.character(fatal_message)) {
      warning("Fatal issue encountered:\n", fatal_message, call. = FALSE)
    } else if (!quiet && length(feedback) == 0) {
      message("Entity is valid.")
    }
  }
  
  # A getter to check validity.
  get_is_valid <- function() is_valid
  
  # Return a list of the closures.
  list(
    add_feedback = add_feedback,
    give_feedback = give_feedback,
    get_is_valid = get_is_valid
  )
}
