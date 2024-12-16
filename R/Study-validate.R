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
      if (is_valid) {
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
  
  # more validation...
  if (is.na())
  
  
  return(get_is_valid())  
})
