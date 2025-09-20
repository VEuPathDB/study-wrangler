#' Validate a Study Object
#'
#' Performs validation checks on constituent entities using the configured
#' validation profile system and validates study-level constraints.
#'
#' @param object A Study object to validate.
#' @param profile Character vector of validation profiles to use. If NULL, uses global config.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Study", function(object, profile = NULL) {
  study <- object
  quiet <- study@quiet
  
  # Get validation profiles to use
  profiles <- get_validation_profiles(profile)
  
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

  # Run all study validators for this profile
  for (validator in validators) {
    result <- validator(study)
    
    if (!result$valid) {
      if (result$fatal %||% FALSE) {
        # Fatal error - stop validation immediately
        give_feedback(fatal_message = result$message)
        return(invisible(FALSE))
      } else {
        # Non-fatal warning - continue validation
        add_feedback(result$message)
      }
    }
  }
  
  # TODO: Add remaining validation logic that hasn't been converted to validators yet
  # This includes study-specific validation checks that will be migrated in phases
  
  give_feedback()
  return(invisible(get_is_valid()))  
})
