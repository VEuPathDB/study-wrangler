#' Study Validators
#'
#' Validation functions for Study objects.
#'
#' @name study-validators
NULL

#' Validator: Check study has a name
#' @keywords internal
validate_study_has_name <- function(study) {
  if (is.na(study@name)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = "Study name is missing."
    ))
  }
  list(valid = TRUE)
}


#' Validator: Check parent-child entity relationships
#' @keywords internal
validate_study_entity_relationships <- function(study) {
  entities <- get_entities(study)
  
  if (length(entities) == 0) {
    return(list(valid = TRUE))
  }
  
  root_entity <- study@root_entity
  
  # Recursive function to check parent-child relationships
  check <- function(entity) {
    problematic_pairs <- tibble(parent = character(), child = character())
    
    for (child in entity@children) {
      result <- check_parent_child_join(entity, child)
      
      if (!result$is_valid) {
        problematic_pair <- tibble(
          parent = get_entity_name(entity),
          child = get_entity_name(child)
        )
        problematic_pairs <- bind_rows(problematic_pairs, problematic_pair)
      }
      
      # Recurse into child's children
      child_results <- check(child)
      problematic_pairs <- bind_rows(problematic_pairs, child_results)
    }
    
    return(problematic_pairs)
  }
  
  problematic_pairs <- check(root_entity)
  
  if (nrow(problematic_pairs) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "Parent-child entity relationships are problematic in the following pairs:\n",
        paste(capture.output(kable(problematic_pairs)), collapse = "\n")
      )
    ))
  }
  
  list(valid = TRUE)
}