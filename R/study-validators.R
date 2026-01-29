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


#' Validator: Check for unique entity stable_ids across study
#' @keywords internal
validate_study_unique_entity_stable_ids <- function(study) {
  entities <- get_entities(study)

  if (length(entities) == 0) {
    return(list(valid = TRUE))
  }

  # Only one entity cannot have duplicates
  if (length(entities) == 1) {
    return(list(valid = TRUE))
  }

  # Collect stable_ids from all entities (suppress auto-generation messages)
  entity_stable_ids <- entities %>%
    map_dfr(~ {
      tibble(
        entity_name = get_entity_name(.x),
        stable_id = suppressMessages(get_stable_id(.x))
      )
    })

  # Check for duplicates
  duplicate_ids <- entity_stable_ids %>%
    group_by(stable_id) %>%
    filter(n() > 1) %>%
    arrange(stable_id) %>%
    ungroup()

  if (nrow(duplicate_ids) > 0) {
    global_varname <- find_global_varname(study, fallback = 'study')

    # Create summary showing which entities share each duplicate ID
    duplicate_summary <- duplicate_ids %>%
      group_by(stable_id) %>%
      summarise(
        entities = paste(entity_name, collapse = ", "),
        .groups = "drop"
      )

    message <- paste(
      "Duplicate stable_ids detected in entity metadata.",
      "This can occur when entity names hash to the same ID or have identical names.",
      "",
      "Affected entities:",
      paste(capture.output(kable(duplicate_summary)), collapse = "\n"),
      "",
      "To resolve this, you must manually set unique stable_ids for affected entities.",
      "",
      "For a single entity (before adding to a study):",
      glue("    entity <- entity %>% set_stable_id('ENT_unique_id')"),
      "",
      "For multiple entities:",
      glue("    {global_varname} <- {global_varname} %>% set_entity_stable_ids(c('entity1', 'entity2', ...), c('ENT_id1', 'ENT_id2', ...))"),
      "",
      "Or to use entity names as stable_ids:",
      glue("    {global_varname} <- {global_varname} %>% set_entity_stable_ids(c('entity1', 'entity2', ...))"),
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