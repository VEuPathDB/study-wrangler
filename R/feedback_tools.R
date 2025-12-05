create_feedback_tools <- function(quiet = FALSE, success_message = "Validation successful!") {
  # Internal state, private to the closures
  is_valid <- TRUE
  feedback <- character()
  advisory_feedback <- character()
  
  add_feedback <- function(message, advisory = FALSE) {
    if (advisory) {
      advisory_feedback <<- c(advisory_feedback, message)
    } else {
      feedback <<- c(feedback, message)
      is_valid <<- FALSE
    }
  }
  
  give_feedback <- function(fatal_message = NULL) {
    if (!quiet && length(feedback) > 0) {
      n <- length(feedback)
      header <- if (n == 1) {
        "Validation issues found (1 issue):"
      } else {
        paste0("Validation issues found (", n, " issues):")
      }
      numbered_feedback <- paste0("[", seq_along(feedback), "] ", feedback)
      warning(header, "\n\n", paste(numbered_feedback, collapse = "\n\n"))
    }
    if (!quiet && length(advisory_feedback) > 0) {
      n_adv <- length(advisory_feedback)
      header <- if (n_adv == 1) {
        "Advisory messages (1 message):"
      } else {
        paste0("Advisory messages (", n_adv, " messages):")
      }
      numbered_advisory <- paste0("[", seq_along(advisory_feedback), "] ", advisory_feedback)
      message(header, "\n\n", paste(numbered_advisory, collapse = "\n\n"))
    }
    if (is.character(fatal_message)) {
      warning("Fatal issue encountered:\n", fatal_message, call. = FALSE)
    } else if (!quiet && length(feedback) == 0) {
      message(success_message)
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
