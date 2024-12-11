library(glue)
library(knitr)

# skimr shortens all factor names to three characters by default.
# Create a custom skimmer that doesn't do this and trims top counts to 5.
skim <- skimr::skim_with(
  factor = skimr::sfl(
    top_counts = function(x) {
      tab <- table(x)
      top <- sort(tab, decreasing = TRUE)
      # Select the top 5 levels
      top5 <- head(top, 5)
      # Summarize the rest
      if (length(top) > 5) {
        others <- sum(top[-(1:5)])
        top5 <- c(top5, `<Others>` = others)
      }
      # Format output
      paste0(names(top5), ": ", top5, collapse = ", ")
    }
  )
)

# Convert R column types to `data_type` metadata annotation
infer_data_type <- function(data, column_name, .no_id_check = FALSE) {
  column <- data %>% pull(column_name)
  
  if (inherits(column, "Date") || inherits(column, "POSIXct")) {
    return("date")
  } 
  
  if (!.no_id_check && n_distinct(column) == length(column)) {
    return("id") # Guess ID type only works for primary keys  
  } 
  
  if (is.integer(column)) { 
    return("integer")
  } 
  
  if (is.numeric(column)) {
    return("number")
  } 
  
  return("string")
}

#'
#' void context function that will stop() if a metadata name isn't an Entity slot
#' 
#' usage:
#' 
#' myfunction <- function(arg1, arg2, ...) {
#'   metadata = list(...)
#'   validate_entity_metadata_names(metadata) # will bail if there's a problem
#'   # continue doing things with metadata
#' }
#'
validate_entity_metadata_names <- function(metadata) {
  disallowed_keys <- c("data", "variables")
  valid_keys <- setdiff(slotNames("Entity"), disallowed_keys)
  invalid_keys <- setdiff(names(metadata), valid_keys)
  if (length(invalid_keys) > 0) {
    stop("Error: these entity_from_file() args are not valid Entity metadata names: ", toString(invalid_keys))
  }
}


#'
#' set `display_name` and `display_name_plural` using name
#'
apply_entity_metadata_defaults <- function(metadata, verbose = FALSE) {
  if (!is.na(metadata$name) && is.na(metadata$display_name)) {
    metadata$display_name <- metadata$name
    if (verbose) message(glue("Note: added default display_name, '{metadata$display_name}', for entity"))
  }
  if (!is.na(metadata$display_name) && is.na(metadata$display_name_plural)) {
    metadata$display_name_plural <- paste0(metadata$display_name, "s")
    if (verbose) message(glue("Note: added default display_name_plural, '{metadata$display_name_plural}', for entity"))
  }
  metadata
}

#'
#' provide an as_list method for all S4 objects - not sure why this isn't
#' in base R!
#'
# Define a generic function for as_list
setGeneric("as_list", function(object) standardGeneric("as_list"))

# Define the method for S4 objects
setMethod("as_list", "ANY", function(object) {
  if (!isS4(object)) {
    stop("Error: the input is not an S4 object.")
  }
  slots <- slotNames(object)
  setNames(lapply(slots, function(slot_name) slot(object, slot_name)), slots)
})

#'
#' validate_entity_name
#' 
#' returns TRUE/FALSE
#'
validate_entity_name <- function(name) {
  is.character(name) && 
    length(name) > 0 && 
    grepl("^[a-zA-Z0-9]+$", name)
}


#' Custom function to force plain text output from skim()
#'
#' usage: `capture_skim(skim(tbl))`
#' 
capture_skim <- function(x, include_summary = TRUE, ...) {
  output <- character(0)

  if (skimr::is_skim_df(x) && nrow(x) > 0) {
    if (include_summary) {
      summary_output <- capture.output(print(summary(x), ...))
      output <- c(output, summary_output)
    }
    by_type <- skimr::partition(x)
    # Process each partition as a plain-text data frame
    partition_output <- purrr::imap(
      by_type, 
      ~ {
        # Rename skim_variable to variable using tidyverse
        df <- .x %>% 
          dplyr::rename(variable = skim_variable)

        # Add header for the partition
        header <- paste("\nSummary of", .y, "variables:")
        body <- kable_signif(df, digits = 3)
        
        # Combine header and body
        paste0(c(header, body), collapse="\n")
      }
    )
    output <- c(output, unlist(partition_output))
  } else {
    # Fall back to the default method for non-skim_df objects
    warning("Input to capture_skim() is not a skim_df object; falling back to render with default print method.")
    output <- c(output, capture.output(NextMethod("print", x)))
  }
  
  # return the entire output
  paste(output, collapse = "\n")
}

#'
#' wrapper for `kable()` that effectively passes the `digits` arg to `signif()`
#' instead of the default `round()` behaviour
#'
kable_signif <- function(x, digits = 3, ...) {
  # Apply signif() to numeric columns
  numeric_cols <- sapply(x, is.numeric)
  x[numeric_cols] <- lapply(x[numeric_cols], function(col) signif(col, digits))
  knitr::kable(x, ...)
}
