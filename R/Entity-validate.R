#' Validate an Entity Object
#'
#' Performs various checks on data completeness and consistency using
#' the configured validation profile system.
#' 
#'
#' @param entity An Entity object to validate.
#' @param profile Character vector of validation profiles to use. If NULL, uses global config.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Entity", function(object, profile = NULL) {
  entity <- object
  quiet <- entity@quiet
  
  # Get validation profiles to use
  profiles <- get_validation_profiles(profile)
  
  # Get validators for this profile and object type
  validators <- get_validators_for_profiles(profiles, "entity")
  
  tools <- create_feedback_tools(quiet = quiet, success_message = "Entity is valid.")
  add_feedback <- tools$add_feedback
  give_feedback <- tools$give_feedback
  get_is_valid <- tools$get_is_valid

  # name of variable to use in fix-it command suggestions  
  global_varname = find_global_varname(entity, 'entity')
  
  # Run all validators for this profile
  for (validator_meta in validators) {
    result <- validator_meta$func(entity)
    
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
    }
  }
  
  # TODO: Add remaining validation logic that hasn't been converted to validators yet
  # This includes the more complex validation checks that will be migrated in phases
  
  # Output feedback to the user
  give_feedback()  
  
  # Return overall validation status
  is_valid <- get_is_valid()
  if (quiet) {
    return(is_valid) # don't be *too* quiet
  } else {
    return(invisible(is_valid))
  }
})
