library(glue)
library(knitr)

#' skimr shortens all factor names to three characters by default.
#' Create a custom skimmer that doesn't do this and trims top counts to 5.
#' @export
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
infer_data_type <- function(data, column_name, 
                            .allowed_data_types = NULL, 
                            .disallowed_data_types = NULL) {
  # Define allowed types as an enum-like factor
  # (excluding "string" which is always the default fallback type)
  valid_data_types <- c("date", "integer", "number", "id")
  
  # Validate .allowed_data_types
  if (!is.null(.allowed_data_types) && 
      !all(.allowed_data_types %in% valid_data_types)) {
    stop("Invalid value(s) in `.allowed_data_types`: ", 
         paste(setdiff(.allowed_data_types, valid_data_types), collapse = ", "))
  }
  
  # Validate .disallowed_data_types
  if (!is.null(.disallowed_data_types) && 
      !all(.disallowed_data_types %in% valid_data_types)) {
    stop("Invalid value(s) in `.disallowed_data_types`: ", 
         paste(setdiff(.disallowed_data_types, valid_data_types), collapse = ", "))
  }
  
  # Extract column
  column <- data %>% pull(column_name)
  
  check_this_type <- function(type) {
    if (is_empty(.disallowed_data_types) && is_empty(.allowed_data_types)) {
      return(TRUE)
    }
    if (type %in% .disallowed_data_types) return(FALSE)
    if (type %in% .allowed_data_types) return(TRUE)
    return(FALSE)
  }
  
  # Dates and numbers come first so they can't be detected as IDs
  return(
    if (check_this_type("date") && is_date_column(column)) {
      "date"
    } else if (check_this_type("integer") && is.integer(column)) {
      "integer"
    } else if (check_this_type("number") && is.numeric(column)) {
      "number"
    } else if (check_this_type("id") && n_distinct(column) == length(column)) {
      "id" # Guess ID type only works for primary keys
    } else {
      "string" # Default fallback
    }
  )
}

is_date_column <- function(column) {
  return(inherits(column, "Date") || inherits(column, "POSIXct"))
}

#'
#' void context function that will stop() if a metadata name isn't a slot
#' in the specified object class
#' 
#' @examples
#' 
#' myfunction <- function(arg1, arg2, ...) {
#'   metadata = list(...)
#'   validate_object_metadata_names('Entity', metadata) # will bail if there is a problem
#'   # continue doing things with metadata
#' }
#'
validate_object_metadata_names <- function(class_name, metadata) {
  disallowed_keys <- c("data", "variables", "root_entity")
  valid_keys <- setdiff(slotNames(class_name), disallowed_keys)
  invalid_keys <- setdiff(names(metadata), valid_keys)
  if (length(invalid_keys) > 0) {
    stop(glue("Error: these entity_from_file() args are not valid {class_name} metadata names: {toString(invalid_keys)}"))
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
#' @export
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
        # Rename skim_variable to variable
        df <- .x %>% 
          dplyr::rename(variable = skim_variable)

        # Add header for the partition
        header <- paste("Summary of", .y, "variables:")
        body <- kable_signif(df, digits = 3)
        margin_bottom <- ''
        
        # Combine header, body and margin
        paste0(c(header, body, margin_bottom), collapse="\n")
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


#'
#' flatten_entities
#' 
#' depth-first recursive flatten of entity tree - returns a list of entities, root-first
#'
flatten_entities <- function(entity) {
  # Collect the current entity
  entities <- list(entity)
  
  # Recursively collect child entities
  for (child in get_children(entity)) {
    entities <- c(entities, flatten_entities(child))
  }
  
  return(entities)
}


#'
#' Get the caller's name of an object passed in to a function
#' (directly or via magrittr pipe) from an interactive 'global' environment
#'
#' Note that if a copy-without-modification has been done, e.g.
#' `copy <- original`
#' then this function could return either name.
#'
#'
# find_global_varname <- function(obj, fallback = "object") {
#   objs <- ls(globalenv())
#   matches <- objs %>%
#     keep(~ data.table::address(get(.x, envir = globalenv())) == address(obj))
#   
#   if (is_empty(matches)) fallback else matches[1]
# }

find_global_varname <- function(obj, fallback = "object") {
  env <- globalenv()
  objs <- ls(env) # List objects in the global environment (console or notebook)

  matches <- objs %>%
    keep(~ data.table::address(get(.x, envir = env)) == data.table::address(obj))
  
  if (is_empty(matches)) fallback else matches[1]
}

#'
#' simple helper to format a heading for the console-based reports
#'
heading <- function(heading) {
  return(glue("\n\n### {heading} ###\n", .trim = FALSE))
}

#'
#' super simple wrapper to aid with outputting to the terminal
#'
#' all args must be character type
#'
to_lines <- function(...) {
  character_vector <- c(...)
  paste0(paste0(character_vector, "\n"), collapse = "")
}

#'
#' prepends two spaces to a vector of characters
#'
#' (nest it if you need more levels)
#' 
indented <- function(...) {
  character_vector <- c(...)
  paste0("  ", character_vector)
}


#'
#' Check and Convert Column to Date with Error Handling
#'
#' helper for set_variable_as_date(entity, column_name)
#'
#'
#' @param data A tibble or data frame containing the data.
#' @param column_name The name of the column to be converted.
#' @returns The modified data if conversion succeeds, or stops with an error if it fails.
check_and_convert_to_date <- function(data, column_name) {
  column <- data %>% pull(column_name) # Extract column by name

  # Safely attempt conversion on each element
  safe_as_date <- function(x) {
    # Use our own regular expression to strictly validate the YYYY-MM-DD format
    # because as.Date()'s `format` argument allows partial matching (e.g., "31-12-31" with "%Y-%m-%d").
    # This ensures only fully qualified dates like "2024-12-31" are processed.
    ifelse(
      str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$"),
      tryCatch(as.Date(x), error = function(e) NA),
      NA
    )
  }
  # final as.Date() is necessary
  result <- column %>% map(safe_as_date) %>% unlist() %>% as.Date()

  failed <- is.na(result) & !is.na(column)
  
  if (all(failed)) {
    example_value <- column[failed][1]
    stop(glue::glue(
      "All rows in column '{column_name}' failed to convert to date.\n",
      "Example invalid value: '{example_value}'.\n",
      "Ensure the column contains only 'YYYY-MM-DD' format dates (and NAs)."
    ))
  } else if (any(failed)) {
    num_failures <- sum(failed)
    failed_examples <- head(column[failed], 10)
    failed_summary <- paste0(
      "Problematic values:\n",
      paste(seq_along(failed_examples), ":", failed_examples, collapse = "\n")
    )
    stop(glue::glue(
      "{num_failures} rows in column '{column_name}' failed to convert to date.\n",
      "{failed_summary}\n",
      "Ensure these values are corrected before conversion."
    ))
  }
  
  # If all rows succeed, replace the column tidily
  data <- data %>%
    mutate("{column_name}" := result)
  
  return(data)
}


#'
#' Generates a random-looking alphanumeric ID (never starting with a digit) 
#'
#' Deterministic if provided with a seed string.
#' 
#' Be aware that this function does not restore the prior RNG state.
#'
generate_alphanumeric_id <- function(length = 11, seed_string = NULL) {
  # Seed the RNG if a seed string is provided
  if (!is.null(seed_string)) {
    char_values <- utf8ToInt(seed_string)
    
    # Define a sequence of prime numbers (at least as long as the string)
    primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)
    
    # If the string is longer than the primes array, repeat the primes
    if (length(char_values) > length(primes)) {
      primes <- rep(primes, length.out = length(char_values))
    }
    
    # Multiply each char value by the corresponding prime
    seed <- sum(char_values * primes[seq_along(char_values)]) %% .Machine$integer.max
    set.seed(seed)
  }
  
  chars <- c(letters, LETTERS, 0:9)  # Define allowed characters
  non_digit_chars <- c(letters, LETTERS)  # Characters to ensure non-digit start
  
  # Loop until the first character is non-digit
  repeat {
    result <- paste0(sample(chars, length, replace = TRUE), collapse = "")
    if (substr(result, 1, 1) %in% non_digit_chars) {
      return(result)
    }
  }
}


#'
#' convenience function for JavaScript people!
#'
#' (do we already have something like this in veupathUtils?)
#' 
is_truthy <- function(x) {
  if (length(x) > 1) {
    stop("is_truthy() only handles scalar (single-value) inputs.")
  }
  if (is.null(x)) {
    return(FALSE)  # NULL is always falsey
  }
  if (is.na(x)) {
    return(FALSE)  # NA is always falsey
  }
  if (is.character(x)) {
    return(nzchar(x))  # Non-empty strings are truthy
  }
  # Factors behave like their character equivalents
  if (is.factor(x)) {
    return(nzchar(as.character(x)))
  }
  return(as.logical(x) %in% TRUE)  # Coerce to logical and check for TRUE
}

