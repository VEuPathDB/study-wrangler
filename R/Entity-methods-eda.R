#' File: Entity EDA-specific methods
#'
#' Methods specifically for EDA platform integration
#'

#' @export
setGeneric("infer_geo_variables_for_eda", function(entity) standardGeneric("infer_geo_variables_for_eda"))


#' infer_geo_variables_for_eda
#'
#' Infers and sets metadata for geographic coordinate variables (latitude and longitude).
#'
#' This method searches for variables with names matching common latitude and longitude
#' patterns (e.g., "latitude", "lat", "longitude", "long", "lng") and sets the appropriate
#' metadata required for EDA validation:
#' - Latitude: stable_id = 'OBI_0001620', data_type = 'number'
#' - Longitude: stable_id = 'OBI_0001621', data_type = 'longitude'
#'
#' Metadata is only set if exactly one latitude variable and one longitude variable are found.
#' If multiple matches or mismatched pairs are detected, a warning is issued and no changes are made.
#'
#' @param entity an Entity object
#' @returns Modified entity with geographic variable metadata set (if exactly one lat and one lng found)
#' @export
setMethod("infer_geo_variables_for_eda", "Entity",
function(entity) {
  variables <- entity@variables

  # Helper function to check if a variable name matches a pattern
  is_variable_like <- function(var_name, patterns) {
    any(sapply(patterns, function(pattern) {
      grepl(paste0("\\b", pattern, "\\b"), var_name, ignore.case = TRUE)
    }))
  }

  # Define patterns for latitude and longitude
  lat_patterns <- c("latitude", "lat")
  lng_patterns <- c("longitude", "long", "lng")

  # Find latitude-like and longitude-like variables
  lat_vars <- variables %>%
    filter(sapply(variable, function(v) is_variable_like(v, lat_patterns))) %>%
    pull(variable)

  lng_vars <- variables %>%
    filter(sapply(variable, function(v) is_variable_like(v, lng_patterns))) %>%
    pull(variable)

  total_geocoord_vars <- length(lat_vars) + length(lng_vars)

  # Case 1: No geocoordinate variables found - nothing to do
  if (total_geocoord_vars == 0) {
    if (!entity@quiet) {
      message("No geographic coordinate variables detected")
    }
    return(entity)
  }

  # Case 2: Not exactly one lat and one lng - warn and return unchanged
  if (length(lat_vars) != 1 || length(lng_vars) != 1) {
    all_vars <- c(lat_vars, lng_vars)
    warning(glue(
      "Found {total_geocoord_vars} geographic coordinate variable(s) but not exactly one latitude and one longitude: ",
      "{paste(all_vars, collapse = ', ')}. ",
      "Expected exactly one of each type. No metadata changes made."
    ))
    return(entity)
  }

  # Case 3: Exactly one lat and one lng - set the metadata
  lat_var <- lat_vars[1]
  lng_var <- lng_vars[1]

  entity <- entity %>%
    set_variable_metadata(lat_var, stable_id = get_config()$export$eda$stable_ids$latitude, data_type = factor('number')) %>%
    set_variable_metadata(lng_var, stable_id = get_config()$export$eda$stable_ids$longitude, data_type = factor('longitude'))

  if (!entity@quiet) {
    message(glue("Set geographic metadata for latitude variable '{lat_var}' and longitude variable '{lng_var}'"))
  }

  return(entity)
})
