create_feedback_tools <- function(quiet = FALSE, success_message = "Validation successful!", format = "full") {
  # Internal state, private to the closures
  is_valid <- TRUE
  feedback <- list()
  advisory_feedback <- list()

  # Pick the appropriate message text based on format
  pick_message <- function(entry) {
    if (format == "upload" && !is.null(entry$upload_message) && nzchar(entry$upload_message)) {
      entry$upload_message
    } else {
      entry$message
    }
  }

  add_feedback <- function(message, advisory = FALSE, upload_message = NULL) {
    entry <- list(message = message, upload_message = upload_message)
    if (advisory) {
      advisory_feedback <<- c(advisory_feedback, list(entry))
    } else {
      feedback <<- c(feedback, list(entry))
      is_valid <<- FALSE
    }
  }

  give_feedback <- function(fatal_message = NULL, fatal_upload_message = NULL) {
    if (!quiet && length(feedback) > 0) {
      n <- length(feedback)
      header <- if (n == 1) {
        "Validation issues found (1 issue):"
      } else {
        paste0("Validation issues found (", n, " issues):")
      }
      texts <- vapply(feedback, pick_message, character(1))
      numbered_feedback <- paste0("[", seq_along(texts), "] ", texts)
      warning(header, "\n\n", paste(numbered_feedback, collapse = "\n\n"))
    }
    if (!quiet && length(advisory_feedback) > 0) {
      n_adv <- length(advisory_feedback)
      header <- if (n_adv == 1) {
        "Advisory messages (1 message):"
      } else {
        paste0("Advisory messages (", n_adv, " messages):")
      }
      adv_texts <- vapply(advisory_feedback, pick_message, character(1))
      numbered_advisory <- paste0("[", seq_along(adv_texts), "] ", adv_texts)
      message(header, "\n\n", paste(numbered_advisory, collapse = "\n\n"))
    }
    if (is.character(fatal_message)) {
      fatal_text <- if (format == "upload" && !is.null(fatal_upload_message) && nzchar(fatal_upload_message)) {
        fatal_upload_message
      } else {
        fatal_message
      }
      warning("Fatal issue encountered:\n", fatal_text, call. = FALSE)
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

#' Get the original column label for a variable (for upload messages)
#'
#' Returns the first provider_label value for the variable, falling back
#' to the variable name if provider_label is not set.
#'
#' @param entity An Entity object
#' @param variable_name The internal variable name(s) — can be a character vector
#' @return Character vector of original column labels
#' @keywords internal
column_label <- function(entity, variable_name) {
  vapply(variable_name, function(vn) {
    pl <- entity@variables %>%
      filter(variable == vn) %>%
      pull(provider_label)
    if (length(pl) > 0 && length(pl[[1]]) > 0 && nzchar(pl[[1]][[1]])) {
      pl[[1]][[1]]
    } else {
      vn
    }
  }, character(1), USE.NAMES = FALSE)
}
