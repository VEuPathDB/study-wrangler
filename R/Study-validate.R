#' Validate a Study Object
#'
#' Performs validation checks on constituent entities using the configured
#' validation profile system and validates study-level constraints.
#'
#' @param object A Study object to validate.
#' @param profile Character vector of validation profiles to use. If NULL, uses global config.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Study", function(object, profiles = NULL) {
  study <- object
  quiet <- study@quiet
  
  # Get validation profiles to use
  profiles <- get_validation_profiles(profiles)
  
  # Get validators for this profile and object type
  validators <- get_validators_for_profiles(profiles, "study")
  
  entities <- get_entities(study)
  
  # name of caller's object for code suggestions after violations
  global_varname = find_global_varname(study, fallback = 'study')
  
  tools <- create_feedback_tools(
    quiet = quiet,
    success_message = glue("Study and its {length(entities)} {ifelse(length(entities) == 1, 'entity', 'entities')} are valid.")
  )
  add_feedback <- tools$add_feedback
  give_feedback <- tools$give_feedback
  get_is_valid <- tools$get_is_valid

  # Validate all entities  
  entities %>% map(
    function(entity) {
      is_valid <- entity %>% quiet() %>% validate(profiles = profiles)
      if (!is_valid) {
        entity_name <- get_entity_name(entity)
        add_feedback(
          glue(
            "The entity named '{entity_name}' is not valid.\nPlease run `{global_varname} %>% get_entity('{entity_name}') %>% verbose() %>% validate()` for more details."
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
  
  # Run all study validators for this profile
  for (validator_meta in validators) {
    result <- validator_meta$func(study)
    
    if (!result$valid) {
      if (result$fatal %||% FALSE) {
        # Fatal error - stop validation immediately
        give_feedback(fatal_message = result$message)
        return(invisible(FALSE))
      } else if (validator_meta$stop_on_error %||% FALSE) {
        # Stop on error validator failed - stop validation immediately
        give_feedback(fatal_message = result$message)
        return(invisible(FALSE))
      } else {
        # Non-fatal warning - continue validation
        add_feedback(result$message)
      }
    } else if (is_truthy(result$message)) {
      # Advisory message - validation passed but there's informational feedback
      add_feedback(result$message)
    }
  }
  
  give_feedback()
  return(invisible(get_is_valid()))  
})
