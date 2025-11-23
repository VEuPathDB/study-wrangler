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

    # Get the missing parent_variable values
    missing_parents <- variable_relationships %>%
      filter(variable %in% variables_with_bad_parents) %>%
      pull(parent_variable) %>%
      unique()

    # Build detailed mapping of which variables reference which missing parents
    parent_to_children <- variable_relationships %>%
      filter(variable %in% variables_with_bad_parents) %>%
      group_by(parent_variable) %>%
      summarise(children = list(variable), .groups = "drop")

    # Create detailed listing
    detail_lines <- purrr::map_chr(seq_len(nrow(parent_to_children)), function(i) {
      parent <- parent_to_children$parent_variable[i]
      children <- parent_to_children$children[[i]]
      paste0("  '", parent, "' is referenced by: ", paste0(children, collapse = ", "))
    })

    # Create YAML fix suggestion for each missing parent
    yaml_suggestions <- purrr::map_chr(seq_len(nrow(parent_to_children)), function(i) {
      parent <- parent_to_children$parent_variable[i]
      paste0(
        "  - category: ", parent, "\n",
        "    display_name: \"<display name for ", parent, ">\"\n",
        "    definition: \"<definition for ", parent, ">\""
      )
    })

    # Create commands to remove invalid parent_variable references
    remove_commands <- paste0("    ", global_varname, " <- ", global_varname, " %>% set_variable_metadata('",
                             variables_with_bad_parents, "', parent_variable = NA)")

    # Create R command to create a new category with all these variables as children
    create_commands <- purrr::map_chr(seq_len(nrow(parent_to_children)), function(i) {
      parent <- parent_to_children$parent_variable[i]
      children <- parent_to_children$children[[i]]
      paste0("    ", global_varname, " <- ", global_varname, " %>% create_variable_category('",
             parent, "', children = c('", paste(children, collapse = "', '"), "'))")
    })

    message <- paste(
      paste0("Missing parent_variable references: ", paste0("'", missing_parents, "'", collapse = ", ")),
      "",
      "The following variables reference parent_variable values that do not exist:",
      paste(detail_lines, collapse = "\n"),
      "",
      "YAML FIX: Add the missing categories to the 'categories' section of your entity YAML file:",
      "",
      "categories:",
      paste(yaml_suggestions, collapse = "\n"),
      "",
      "R FIX OPTION 1: Create the missing categories programmatically:",
      paste(create_commands, collapse = "\n"),
      "",
      "R FIX OPTION 2: Remove the invalid parent_variable references:",
      paste(remove_commands, collapse = "\n"),
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