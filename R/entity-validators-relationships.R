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
  
  variables_with_bad_parents <- variable_relationships %>%
    filter(!is.na(parent_variable)) %>%
    left_join(
      variable_relationships,
      join_by(parent_variable == variable),
      keep = TRUE, # Retain both left and right versions of overlapping column names (e.g., variable.left, variable.right)
      suffix = c(".left", ".right")
    ) %>%
    filter(is.na(variable.right)) %>%
    pull(variable.left) 
  
  if (length(variables_with_bad_parents) > 0) {
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Create commands to remove invalid parent_variable references
    remove_commands <- paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('", 
                             variables_with_bad_parents, "', parent_variable = NA)")
    
    # Create command to create a new category with all these variables as children
    create_command <- paste0("    ", global_varname, " <- ", global_varname, " %>% create_variable_category('new_category_name', children = c('", 
                            paste(variables_with_bad_parents, collapse = "', '"), "'))")
    
    message <- paste(
      paste0("These variables or categories have 'parent_variable' values that do not exist: ",
             paste0(variables_with_bad_parents, collapse=", ")),
      "The values for 'parent_variable' should be variable names that exist in the entity.",
      "",
      "To fix this, you can remove the invalid parent_variable references:",
      paste(remove_commands, collapse = "\n"),
      "",
      "Or create a new variable category/grouping with these variables as children:",
      create_command,
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = message
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
    # Get global variable name for fix-it suggestions
    global_varname <- find_global_varname(entity, 'entity')
    
    # Find variables that have parent_variable relationships (potential cycle contributors)
    variables_with_parents <- variable_relationships %>%
      filter(!is.na(parent_variable)) %>%
      pull(variable)
    
    # Create commands to remove parent_variable relationships
    remove_commands <- paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('", 
                             variables_with_parents, "', parent_variable = NA)")
    
    message <- paste(
      "Illegal circular path detected in the parent_variable -> variable graph.",
      "Circular relationships prevent proper hierarchical organization of variables.",
      "",
      "To fix this, remove parent_variable relationships from these variables:",
      paste(remove_commands, collapse = "\n"),
      "",
      "Then recreate the hierarchy carefully to avoid circular references.",
      sep = "\n"
    )
    
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = message
    ))
  }
  
  list(valid = TRUE)
}