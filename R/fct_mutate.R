#'
#' fct_mutate
#'
#' Mutate Factor Values Row-Wise
#'
#' A utility function to update factor values row-wise based on a condition,
#' while ensuring the integrity of factor levels. By default, new values must
#' already be part of the factor's levels. If `.auto_expand = TRUE`, the function
#' will add the new value(s) to the levels automatically.
#'
#' @param factor A factor to be updated.
#' @param condition A logical vector of the same length as `factor`, indicating
#'   which rows should be updated. In a \emph{special two argument form}, it can be
#'   omitted. See Example 4.
#' @param true_value A character string specifying the new value to assign to
#'   rows where `condition` is `TRUE`. By default, this value must already be a
#'   valid level in `factor` unless `.auto_expand = TRUE`.
#' @param false_value A optional character string specifying the new value to assign to
#'   rows where `condition` is `FALSE` (value remains unchanged if not provided).
#'   This value must also already be a valid level in `factor` unless
#'   `.auto_expand = TRUE`.
#' @param .auto_expand A logical indicating whether to automatically expand the
#'   levels of `factor` to include `true_value` (and optionally `false_value`)
#'   if it is not already a level. Defaults to `FALSE`.
#'
#' @return A factor with updated values and levels, if applicable.
#'
#' @details
#' - When `.auto_expand = FALSE` (default), the function ensures safety by
#'   requiring `true_value` to be an existing level. This prevents unintended
#'   modifications to the factor's structure.
#' - When `.auto_expand = TRUE`, the function adds `true_value` to the factor's
#'   levels using `forcats::fct_expand()` before performing the mutation.
#' - The above also applies to `false_value` if provided.
#' - The function preserves the order of factor levels and does not introduce
#'   unwanted conversions between character and factor types.
#'
#' @examples
#' # Example 1: Safe mutation with existing level
#' f <- factor(c("numeric", "character"), levels = c("numeric", "character", "id"))
#' fct_mutate(f, condition = f == "numeric", true_value = "id")
#'
#' # Example 2: Error when new value is not a level (default behavior)
#' \dontrun{
#' f <- factor(c("numeric", "character"))
#' fct_mutate(f, condition = f == "numeric", true_value = "id")
#' }
#'
#' # Example 3: Auto-expansion of levels
#' f <- factor(c("numeric", "character"))
#' fct_mutate(f, condition = f == "numeric", true_value = "id", .auto_expand = TRUE)
#'
#' # Example 4: Two argument form
#' 
#' # For the simple case where you just want to change all values in a factor vector,
#' # you can omit the condition (which will default to TRUE), e.g.
#' exam_results <- exam_results %>% mutate(marked = fct_mutate(marked, "no"))
#'
fct_mutate <- function(factor, condition, true_value, false_value = NULL, .auto_expand = FALSE) {
  if (!is.factor(factor)) stop("Error: 'factor' must be a factor.")
  
  # handle two-anonymous argument call: fct_mutate(factor, value)
  if (missing(true_value)) {
    true_value <- condition
    condition <- TRUE
  }
  # handle case where true_value is named
  if (missing(condition)) {
    condition <- TRUE
  }
  
  # Check if condition is logical and handle recycling
  if (!is.logical(condition)) {
    stop("Error: 'condition' must be a logical vector or a single logical value.")
  }
  if (length(condition) == 1) {
    condition <- rep(condition, length(factor))
  }
  
  # Ensure lengths match
  if (length(factor) != length(condition)) {
    stop(glue::glue("Error: 'factor' and 'condition' must have the same length or 'condition' must have length 1."))
  }
  
  # Check and expand levels for true_value (NAs are always allowed)
  if (!is.na(true_value)) {
    if (!.auto_expand && !(true_value %in% levels(factor))) {
      stop(glue::glue("Error: Value '{true_value}' is not an existing level in the factor."))
    }
    if (.auto_expand) {
      factor <- forcats::fct_expand(factor, true_value)
    }
  } else {
    true_value <- NA_character_
  }
  
  # Check and expand levels for false_value if it is provided
  if (!is.null(false_value)) {
    if (!is.na(false_value)) { # NAs are always allowed
      if (!.auto_expand && !(false_value %in% levels(factor))) {
        stop(glue::glue("Error: Value '{false_value}' is not an existing level in the factor."))
      }
      if (.auto_expand) {
        factor <- forcats::fct_expand(factor, false_value)
      }
    } else {
      false_value <- NA_character_
    }
  }
  
  # Apply the mutation row-wise
  levels <- levels(factor)
  if (is.null(false_value)) {
    factor <- if_else(condition,
                      forcats::fct(true_value, levels = levels, na = NA_character_),
                      factor)
  } else {
    factor <- if_else(condition,
                      forcats::fct(true_value, levels = levels, na = NA_character_),
                      forcats::fct(false_value, levels = levels, na = NA_character_))
  }
  return(factor)
}
