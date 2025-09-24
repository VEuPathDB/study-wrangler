#' EDA Entity Basic Validators
#'
#' Validation functions for EDA Entities.
#'
#' @name entity-validators-eda-basic
NULL

#' Validator: Check display_name is not null for any variable
#' @keywords internal
validate_eda_variable_display_name_not_null <- function(entity) {
  variables <- entity@variables

  # Find variables with null/NA display_name
  null_display_name_vars <- variables %>%
    filter(is.na(display_name) | is.null(display_name)) %>%
    pull(variable)

  if (length(null_display_name_vars) > 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = paste0("EDA validation requires display_name for all variables. Missing display_name for: ",
                      paste(null_display_name_vars, collapse = ", "))
    ))
  }

  list(valid = TRUE)
}
