#' EDA Entity Basic Validators
#'
#' Validation functions for EDA Entities.
#'
#' @name entity-validators-eda-basic
NULL

#' Validator: Check display_name is not null for any variable or category
#' @keywords internal
validate_eda_variable_display_name_not_null <- function(entity) {
  # Get both variables and categories
  variables_and_categories <- entity %>% get_variable_and_category_metadata()

  # Find variables/categories with null/NA display_name
  null_display_name_items <- variables_and_categories %>%
    filter(is.na(display_name) | is.null(display_name)) %>%
    pull(variable)

  if (length(null_display_name_items) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')

    # Separate variables from categories for clearer messaging
    categories <- entity %>% get_category_metadata() %>% pull(variable)
    null_display_name_vars <- null_display_name_items[!null_display_name_items %in% categories]
    null_display_name_cats <- null_display_name_items[null_display_name_items %in% categories]

    # Build message components
    message_parts <- character()

    if (length(null_display_name_vars) > 0) {
      message_parts <- c(message_parts,
        paste0("EDA validation requires display_name for all variables. Missing display_name for: ",
               paste(null_display_name_vars, collapse = ", ")))
    }

    if (length(null_display_name_cats) > 0) {
      message_parts <- c(message_parts,
        paste0("EDA validation requires display_name for all categories. Missing display_name for: ",
               paste(null_display_name_cats, collapse = ", ")))
    }

    # Create individual fix commands for each item
    fix_commands <- sapply(null_display_name_items, function(item_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('",
             item_name, "', display_name = 'Your Display Name Here')")
    })

    message <- paste(
      paste(message_parts, collapse = "\n"),
      "To set display_name, use:",
      paste(fix_commands, collapse = "\n"),
      sep = "\n"
    )

    # Add provider_label suggestion only if there are variables (not categories)
    if (length(null_display_name_vars) > 0) {
      message <- paste(
        message,
        "",
        "Or, for variables with provider_label metadata, you can automatically use those values:",
        paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_display_names_from_provider_labels()"),
        sep = "\n"
      )
    }

    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = message
    ))
  }

  list(valid = TRUE)
}

#' Validator: Check string variable values do not exceed 1000 characters
#'
#' The EDA backend stores string values in VARCHAR(1000). Values longer than
#' 1000 characters will cause import failures.
#' @keywords internal
validate_entity_string_value_length <- function(entity) {
  data <- entity@data
  variables <- entity@variables

  string_vars <- variables %>%
    filter(data_type == "string") %>%
    select(variable, is_multi_valued, multi_value_delimiter)

  if (nrow(string_vars) == 0) return(list(valid = TRUE))

  exceeds_limit <- function(col_name, is_mv, delim) {
    vals <- data[[col_name]]
    raw_lengths <- nchar(vals)
    if (max(raw_lengths, na.rm = TRUE) <= 1000) return(FALSE)

    if (!is_mv) return(TRUE)

    # Two-stage check for multi-valued: only split rows where the raw cell
    # exceeds 1000 chars (a necessary condition for any component to exceed it).
    long_rows <- vals[!is.na(vals) & raw_lengths > 1000]
    any(sapply(strsplit(long_rows, delim, fixed = TRUE), function(parts) any(nchar(parts) > 1000)))
  }

  too_long <- string_vars %>%
    filter(pmap_lgl(list(variable, is_multi_valued, multi_value_delimiter), exceeds_limit)) %>%
    pull(variable)

  if (length(too_long) == 0) return(list(valid = TRUE))

  global_varname <- find_global_varname(entity, 'entity')

  messages <- sapply(too_long, function(col_name) {
    is_mv <- string_vars %>% filter(variable == col_name) %>% pull(is_multi_valued)
    delim  <- string_vars %>% filter(variable == col_name) %>% pull(multi_value_delimiter)
    if (is_mv) {
      vals <- unlist(strsplit(data[[col_name]][!is.na(data[[col_name]])], delim, fixed = TRUE))
    } else {
      vals <- data[[col_name]]
    }
    max_len <- max(nchar(vals), na.rm = TRUE)
    paste0("Variable '", col_name, "' has values up to ", max_len,
           " characters long (EDA backend limit is 1000).")
  })

  fix_commands <- sapply(too_long, function(col_name) {
    paste0("    ", global_varname, " <- ", global_varname,
           " %>% modify_data(mutate(", col_name, " = substr(", col_name, ", 1, 1000)))")
  })

  message <- paste(
    paste(messages, collapse = "\n"),
    "If appropriate for your data, truncate these values to 1000 characters:",
    paste(fix_commands, collapse = "\n"),
    sep = "\n"
  )

  list(valid = FALSE, fatal = FALSE, message = message)
}

#' Validator: Check string variable values do not contain newline characters
#'
#' Newlines in string values corrupt the TSV-based VDI import format.
#' For multi-valued columns, checking the raw (unsplit) cell is sufficient:
#' if no raw cell contains a newline, no component can either.
#' @keywords internal
validate_entity_string_value_newlines <- function(entity) {
  data <- entity@data
  variables <- entity@variables

  string_cols <- variables %>%
    filter(data_type == "string") %>%
    pull(variable)

  if (length(string_cols) == 0) return(list(valid = TRUE))

  has_newlines <- string_cols[vapply(string_cols, function(col) {
    any(grepl("\n|\r", data[[col]], perl = TRUE), na.rm = TRUE)
  }, logical(1))]

  if (length(has_newlines) == 0) return(list(valid = TRUE))

  global_varname <- find_global_varname(entity, 'entity')

  messages <- paste0("Variable '", has_newlines,
                     "' contains newline characters, which will corrupt VDI import.")

  fix_commands <- sapply(has_newlines, function(col_name) {
    paste0("    ", global_varname, " <- ", global_varname,
           " %>% modify_data(mutate(", col_name,
           " = gsub(\"\\n|\\r\", \" \", ", col_name, ")))")
  })

  message <- paste(
    paste(messages, collapse = "\n"),
    "If appropriate for your data, replace newlines with spaces:",
    paste(fix_commands, collapse = "\n"),
    sep = "\n"
  )

  list(valid = FALSE, fatal = FALSE, message = message)
}
