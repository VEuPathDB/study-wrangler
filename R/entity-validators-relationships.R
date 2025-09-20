#' Entity Relationship Validators
#'
#' Validation functions for entity variable relationships and graph structure.
#'
#' @name entity-validators-relationships
NULL

#' Validator: Check parent_variable references exist
#' @keywords internal
validate_entity_parent_variable_exists <- function(entity) {
  variables <- entity@variables
  
  variable_relationships <- variables %>% select(variable, parent_variable)
  
  bad_parents <- variable_relationships %>%
    filter(!is.na(parent_variable)) %>%
    left_join(
      variable_relationships,
      join_by(parent_variable == variable),
      keep = TRUE, # Retain both left and right versions of overlapping column names (e.g., variable.left, variable.right)
      suffix = c(".left", ".right")
    ) %>%
    filter(is.na(variable.right)) %>%
    pull(variable.left) 
  
  if (length(bad_parents) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These variables or categories have 'parent_variable' values that do not exist: ",
        paste0(bad_parents, collapse=", "),
        "\nThe values for 'parent_variable' should be variable names."
      )
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check for circular paths in variable tree
#' @keywords internal
validate_entity_no_circular_graph <- function(entity) {
  variables <- entity@variables
  
  variable_relationships <- variables %>% select(variable, parent_variable)
  
  graph <- igraph::graph_from_data_frame(
    variable_relationships %>% replace_na(list(parent_variable = '_root_'))
  )
  
  if (!igraph::is_acyclic(graph)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = "Illegal circular path detected in the parent_variable -> variable graph."
    ))
  }
  
  list(valid = TRUE)
}