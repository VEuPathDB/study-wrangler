#' skimr shortens all factor names to three characters by default.
#' Create a custom skimmer that doesn't do this and trims top counts to 5.
#' @export
skim <- local({
  .skim_custom <- NULL
  function(...) {
    if (is.null(.skim_custom)) {
      .skim_custom <<- skimr::skim_with(
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
    }
    .skim_custom(...)
  }
})

# Convert R column types to `data_type` metadata annotation
infer_data_type <- function(column, 
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
  
  check_this_type <- function(type) {
    if (is_empty(.disallowed_data_types) && is_empty(.allowed_data_types)) {
      return(TRUE)
    }
    if (type %in% .disallowed_data_types) return(FALSE)
    if (is_empty(.allowed_data_types)) return(TRUE)
    if (type %in% .allowed_data_types) return(TRUE)
    return(FALSE)
  }

  # Dates and numbers come first so they can't be detected as IDs
  inferred_data_type =
    if (check_this_type("date") && is.Date(column)) {
      "date"
    } else if (check_this_type("integer") && is.integer(column)) {
      "integer"
    } else if (check_this_type("number") && is.numeric(column)) {
      "number"
    } else if (check_this_type("id") && n_distinct(column) == length(column) && !any(is.na(column))) {
      "id" # Guess ID type only works for primary keys
    } else {
      "string" # Default fallback
    }
  
  return(inferred_data_type)
}

#' convert_to_type
#' 
#' Converts a character vector into the appropriate type based on a provided `data_type`.
#' Supports common data types: "date", "integer", "number", and "string".
#' 
#' @param x A character vector to be converted
#' @param data_type A string indicating the target data type ("date", "integer", "number", "string")
#' 
#' @returns A vector of the appropriate type
convert_to_type <- function(x, data_type) {
  if (data_type == "date") {
    return(as.Date(x, origin = "1970-01-01"))
  } else if (data_type == "integer") {
    return(as.integer(x))
  } else if (data_type %in% c("number","longitude")) {
    return(as.numeric(x))
  } else if (data_type == "string") {
    return(as.character(x))
  } else {
    stop(glue("Unsupported data_type: {data_type}"))
  }
}


#' type_convert_quietly
#' 
#' Converts character data in a tibble to appropriate types, suppressing messages and handling
#' specific warnings about invalid dates gracefully. Detects column types using `readr::type_convert()`.
#' If a column contains problematic date values, a warning is issued, and the column is left as character type.
#' 
#' @param data A tibble where all columns are character vectors
#' @param guess_integer Logical. If TRUE (default), converts numbers to integer where appropriate
#' 
#' @returns A tibble with converted column types
#' @export
type_convert_quietly <- function(data, guess_integer = TRUE, global_varname = 'entity') {
  convert_column <- function(column, column_name) {
    tryCatch(
      {
        suppressMessages(readr::type_convert(tibble(value = column), guess_integer = guess_integer)$value)
      },
      warning = function(w) {
        if (grepl("expected valid date", conditionMessage(w))) {
          warning(
            to_lines(
              glue("Column '{column_name}' contains invalid dates."),
              "Column will be treated as a regular string variable until the dates are cleaned up",
              "and then formalised as dates with:",
              indented(glue("{global_varname} <- {global_varname} %>% set_variable_as_date('{column_name}')")),
              conditionMessage(w)
            ),
            call. = FALSE
          )
          return(column) # Return original character column
        } else {
          stop(w) # Stop for other unexpected warnings
        }
      }
    )
  }
  
  data %>%
    mutate(across(everything(), ~ convert_column(.x, cur_column())))
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

#'
#' validate_stable_id
#' 
#' returns TRUE/FALSE
#'
validate_stable_id <- function(id) {
  is.character(id) && 
    length(id) > 0 && 
    grepl("^[a-zA-Z][a-zA-Z0-9_]+$", id)
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
#' Generates a truncated SHA1 digest of the seed_string argument 
#'
generate_alphanumeric_id <- function(seed_string, length = 11) {
  if (missing(seed_string)) stop("generate_alphanumeric_id() requires a seed string argument")
  return(substring(digest(seed_string, alg="sha1", serialize=FALSE), 1, length))
}

#'
#' Wraps `generate_alphanumeric_id()` to add a prefix
#'
#' Ensures that the final ID never starts with a digit (even after the prefix).
#'
#' @param prefix A character string to prepend to the generated ID. Defaults to an empty string.
#' @param length The length of the variable part of the ID (excluding the prefix). Defaults to 11.
#' @param seed_string Optional seed string for deterministic ID generation.
#' @return A prefixed alphanumeric ID.
#' @export
#'
prefixed_alphanumeric_id <- function(prefix = NULL, length = 11, seed_string = NULL) {
  # Ensure the prefix is non-empty and valid
  if (!is.character(prefix)) {
    stop("The `prefix` argument must be a single character string.")
  }
  
  # Generate the variable part of the ID
  variable_part <- generate_alphanumeric_id(length = length, seed_string = seed_string)
  
  # Combine prefix and variable part
  full_id <- paste0(prefix, variable_part)
  
  # Ensure the final ID does not start with a digit
  if (grepl("^[0-9]", full_id)) {
    stop("The prefix combined with the generated ID results in a digit starting the final ID. Please adjust the prefix.")
  }
  
  return(full_id)
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

#'
#' utility function that tracks and outputs only unique messages
#'
#' usage: `message_without_dupes$send("this message will appear only once")`
#' 
#' and: `message_without_dupes$reset()`
#' which resets the state so you can see the same messages again.
#'
#'
message_without_dupes <- local({
  seen_messages <- character()
  
  list(
    send = function(msg) {
      if (!msg %in% seen_messages) {
        message(msg)
        seen_messages <<- c(seen_messages, msg)
      }
    },
    reset = function() {
      seen_messages <<- character()  # Clear the list of seen messages
    }
  )
})


#'
#' returns an integer count of the maximum number of digits after the decimal
#' place in the numbers of the given vector
#'
max_decimals <- function(x) {
  if (!is.numeric(x)) return(0L)
  
  # Format the numbers with a fixed precision, avoiding scientific notation
  formatted_numbers <- format(x, scientific = FALSE, digits = 15, trim = TRUE)
  
  # Extract the fractional part by removing the integer part and trailing zeros
  fractional_strings <- sub("^-?\\d*\\.?|0+$", "", formatted_numbers)
  
  # Count the number of decimal places
  nchar_fractions <- nchar(fractional_strings)
  
  # Handle cases where there are no fractional parts
  nchar_fractions[fractional_strings == ""] <- 0
  
  return(as.integer(max(nchar_fractions)))
}


#'
#' helper for pretty_tree(entity)
#'
recursive_ascii_tree <- function(
    entity,
    prefix,
    is_last,
    is_root = FALSE,
    get_label_fn,
    get_children_fn
) {
  # Determine the prefix for this entity line
  line_prefix <- if (is_root) {
    prefix
  } else if (is_last) {
    paste0(prefix, "└── ")
  } else {
    paste0(prefix, "├── ")
  }
  
  this_line <- paste0(line_prefix, get_label_fn(entity))
  
  # For children, we need to decide on the next prefix. If this entity is the last child,
  # the next prefix for its children is prefix + "    " (4 spaces),
  # otherwise it's prefix + "|   ".
  children_prefix <- if (is_root) {
    prefix
  }  else if (is_last) {
    paste0(prefix, "    ")
  } else {
    paste0(prefix, "│   ")
  }
  
  children <- get_children_fn(entity)
  
  if (length(children) == 0) {
    # No children, just return the current line.
    return(this_line)
  } else {
    # Recursively format each child. The last child's is_last = TRUE.
    lines_for_children <- mapply(
      FUN = function(child, is_last_child) {
        recursive_ascii_tree(
          child,
          prefix = children_prefix,
          is_last = is_last_child,
          get_label_fn = get_label_fn,
          get_children_fn = get_children_fn
        )
      },
      child = children,
      is_last_child = seq_along(children) == length(children),
      SIMPLIFY = FALSE
    )
    
    # Combine this entity's line with all children's lines.
    return(c(this_line, unlist(lines_for_children)))
  }
}



#' expand_multivalued_data_column
#' 
#' Expands data from a multi-valued or single-valued column.
#' For multi-valued data, splits the input by the specified delimiter.
#' For single-valued data, returns the original column.
#' In both cases, a single-column tibble is returned.
#' 
#' If `.type_convert` is TRUE, the returned tibble will be automatically type-converted
#' by `readr::type_convert()`
#' 
#' @param data a tibble of data
#' @param variable the name of the column to process
#' @param is_multi_valued Logical indicating whether the column is multi-valued
#' @param multi_value_delimiter The delimiter used to split multi-valued data (if applicable)
#' @param .type_convert Logical (default FALSE) - automatically type the expanded tibble before returning
#' 
#' @returns A tibble with just the `variable` column, expanded to one value per row
#'
#' @note This has been rewritten with base R for speed. See original tidy version below
#' 
expand_multivalued_data_column <- function(
    data,
    variable,
    is_multi_valued,
    multi_value_delimiter,
    .type_convert = FALSE
) {
  # pull vector once
  vec <- data[[variable]]
  
  if (is_multi_valued) {
    # coerce to character and split on the delimiter
    vec_chr   <- as.character(vec)
    parts     <- strsplit(vec_chr, split = multi_value_delimiter, fixed = TRUE)
    expanded  <- unlist(parts, use.names = FALSE)
    out_tib   <- tibble(!!variable := expanded)
    
    if (.type_convert) {
      out_tib <- type_convert_quietly(out_tib, global_varname = find_global_varname(data, 'entity'))
    }
    
  } else {
    # just wrap the original column
    out_tib <- tibble(!!variable := vec)
  }
  
  out_tib
}

expand_multivalued_data_column_original_slow_tidy_version <- function(data, variable, is_multi_valued, multi_value_delimiter, .type_convert = FALSE) {
  one_col_tibble <- data %>% select(all_of(variable))
  if (is_multi_valued) {
    one_col_tibble <- one_col_tibble %>%
      mutate(across(everything(), as.character)) %>%
      separate_longer_delim(all_of(variable), delim = multi_value_delimiter)
    if (.type_convert) {
      one_col_tibble <- one_col_tibble %>% type_convert_quietly(global_varname = find_global_varname(data, 'entity'))
    }
    return(one_col_tibble)
  } else {
    return(one_col_tibble)  # Return the original column as a tibble
  }
}

#' Convert a tibble to a list of row-wise sparse objects
#'
#' This function converts a tibble into a list of named lists, where each row
#' becomes a list object with column names as keys. Any `NA` values in atomic
#' vectors are omitted, while list-columns remain unchanged.
#'
#' @param tibble A tibble or data frame to convert.
#' @return A list of lists, where each list represents a row with `NA` values removed.
#' @examples
#' library(tibble)
#' df <- tibble(
#'   name = c("Alice", "Bob"),
#'   age = c(25, NA),
#'   city = c("New York", "London"),
#'   tags = list(c("vip", "member"), NULL) # Example of a list-column
#' )
#' tibble_to_sparse_object(df)
#'
#' @export
tibble_to_sparse_object <- function(tibble) {
  tibble %>% pmap(
    ~ list(...) %>%
      discard(~ length(.x) == 1 && is.na(.x))
  )
}

write_pretty_yaml <- function(data, file) {
  data %>%
    # convert to basic extra-indented yaml
    yaml::as.yaml(indent.mapping.sequence = TRUE) %>%
    # Add line breaks before simple alphanumeric root-level keys for lists
    str_replace_all("(\n\\w+:(?: \\[\\])?\n)", "\n\\1") %>%
    writeLines(file)
}
