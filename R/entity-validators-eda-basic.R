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
