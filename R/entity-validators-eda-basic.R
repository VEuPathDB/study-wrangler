#' EDA Entity Basic Validators
#'
#' Validation functions for EDA Entities.
#'
#' @name entity-validators-eda-basic
NULL

#' Validator: Check display_name is not null for any variable
#' @keywords internal
validate_eda_variable_display_name_not_null <- function(entity) {
  variables <- entity %>% get_variable_metadata()

  # Find variables with null/NA display_name
  null_display_name_vars <- variables %>%
    filter(is.na(display_name) | is.null(display_name)) %>%
    pull(variable)

  if (length(null_display_name_vars) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create individual fix commands for each variable
    fix_commands <- sapply(null_display_name_vars, function(var_name) {
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('",
             var_name, "', display_name = 'Your Display Name Here')")
    })

    message <- paste(
      paste0("EDA validation requires display_name for all variable columns. Missing display_name for: ",
             paste(null_display_name_vars, collapse = ", ")),
      "To set display_name for these variables, use:",
      paste(fix_commands, collapse = "\n"),
      "",
      "Or, if provider_label metadata is set, you can automatically use those values:",
      paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_display_names_from_provider_labels()"),
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = message
    ))
  }

  list(valid = TRUE)
}
