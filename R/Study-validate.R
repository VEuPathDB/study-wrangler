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
  
  tools <- create_feedback_tools(quiet = quiet)
  # the following can be made nicer with library(zeallot)
  add_feedback <- tools$add_feedback
  give_feedback <- tools$give_feedback
  get_is_valid <- tools$get_is_valid

  # Validate all entities  
  entities <- get_entities(study)
  
  entities %>% map(
    function(entity) {
      is_valid <- entity %>% set_quiet() %>% validate()
      if (!is_valid) {
        add_feedback(
          glue(
            "The entity named '{get_entity_name(entity)}' is not valid. Please run `validate()` on it directly for more details."
          )
        )
      }
    }
  )
  # early termination if any of the entities are invalid
  if (!get_is_valid()) {
    give_feedback(fatal_message = "Error: one or more entities is invalid.")
    return(FALSE)
  }
  
  # Metadata slot validation...
  if (is.na(study@name)) {
    add_feedback(
      "Study name is missing. Add it with `set_study_name()`."
    )
  }
  
  # Foreign key/parent ID check
  if (length(entities) > 0) {
    
    # Recursive function to check parent-child relationships
    check <- function(entity) {
      problematic_pairs <- list() # Initialize list to collect problematic pairs
      
      for (child in entity@children) {
        result <- check_parent_child_join(entity, child)
        
        # If invalid, collect the parent-child pair
        if (!result$is_valid) {
          problematic_pairs <- c(
            problematic_pairs,
            paste(get_entity_name(entity), "->", get_entity_name(child))
          )
        }
        
        # Recurse into the child's children
        child_results <- check(child)
        problematic_pairs <- c(problematic_pairs, child_results)
      }
      
      return(problematic_pairs) # Return all problematic pairs for this branch
    }
    
    # Start the check from the root entity
    problematic_pairs <- check(root_entity)
    
    # If there are any problematic pairs, report them
    if (length(problematic_pairs) > 0) {
      add_feedback(to_lines(
        "Parent-child entity relationships are problematic in the following pairs:",
        paste(problematic_pairs, collapse = "; "),
        "Use check_parent_child_join(parent, child) for row-wise details."

        # TO DO <<<<<<<<<<<<<<<<<<<<<<<<<
        # should probably have a study method that takes the names:
        # study %>% check_parent_child_join('household', 'participant')
        # and also get_entity_by_name(study, name)
      ))
    }
  }
  
  give_feedback()
  
  return(get_is_valid())  
})
