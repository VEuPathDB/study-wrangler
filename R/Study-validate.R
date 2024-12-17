#' Validate a Study Object
#'
#' Performs validation checks on constituent entities
#' and performs a row-wise check for all parent IDs
#'
#' @param entity A Study object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Study", function(object) {
  study <- object
  root_entity <- study@root_entity
  quiet <- study@quiet

  # name of caller's object for code suggestions after violations
  caller_name = find_global_varname(study, fallback = 'study')
  
  tools <- create_feedback_tools(quiet = quiet)
  # the following can be made nicer with library(zeallot)
  add_feedback <- tools$add_feedback
  give_feedback <- tools$give_feedback
  get_is_valid <- tools$get_is_valid

  # Validate all entities  
  entities <- get_entities(study)
  
  entities %>% map(
    function(entity) {
      entity_caller_name = find_global_varname(entity, 'entity')
      is_valid <- entity %>% quiet() %>% validate()
      if (!is_valid) {
        add_feedback(
          glue(
            "The entity named '{get_entity_name(entity)}' is not valid.\nPlease run `{entity_caller_name} %>% validate()` for more details."
          )
        )
      }
    }
  )
  # early termination if any of the entities are invalid
  if (!get_is_valid()) {
    give_feedback(fatal_message = "Error: one or more entities is invalid.")
    return(invisible(FALSE))
  }
  
  # Metadata slot validation...
  if (is.na(study@name)) {
    add_feedback(
      glue("Study name is missing. Add it with `{caller_name} <- {caller_name} %>% set_study_name('a name')`.")
    )
  }
  
  # Foreign key/parent ID check
  if (length(entities) > 0) {
    
    # Recursive function to check parent-child relationships
    check <- function(entity) {
      # Initialize an empty tibble to collect problematic pairs
      problematic_pairs <- tibble(parent = character(), child = character())
      
      for (child in entity@children) {
        result <- check_parent_child_join(entity, child)
        
        # If invalid, create a row for the parent-child pair
        if (!result$is_valid) {
          problematic_pair <- tibble(
            parent = get_entity_name(entity),
            child = get_entity_name(child)
          )
          problematic_pairs <- bind_rows(problematic_pairs, problematic_pair)
        }
        
        # Recurse into the child's children and bind the results
        child_results <- check(child)
        problematic_pairs <- bind_rows(problematic_pairs, child_results)
      }
      
      return(problematic_pairs) # Return the combined tibble for this branch
    }
    
    # Start the check from the root entity
    problematic_pairs <- check(root_entity)
    
    # If there are any problematic pairs, report them
    if (nrow(problematic_pairs) > 0) {
      add_feedback(to_lines(
        "Parent-child entity relationships are problematic in the following pairs:",
        indented(
          kable(problematic_pairs)
        ),
        "~~~~",
        "Use the following code to get row-wise details:",
        indented(
          problematic_pairs %>%
            transmute(glue("{caller_name} <- {caller_name} %>% check_parent_child_row_linkage('{parent}', '{child}')")) %>% pull()
        )
      ))
    }
  }
  
  give_feedback()
  return(invisible(get_is_valid()))  
})
