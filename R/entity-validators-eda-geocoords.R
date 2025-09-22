#' EDA Entity Geocoordinate Validators
#'
#' Validation functions for geocoordinate variables in EDA profile.
#'
#' @name entity-validators-eda-geocoords
NULL

#' Validator: Check geocoordinate variables
#' @keywords internal
validate_geocoordinate_variables <- function(entity) {
  variables <- entity@variables
  
  # Helper function to check if a variable name or provider_label matches a pattern
  is_variable_like <- function(var_row, patterns) {
    variable_name <- var_row$variable
    provider_labels <- var_row$provider_label[[1]]
    
    # Check variable name
    name_match <- any(sapply(patterns, function(pattern) {
      grepl(paste0("\\b", pattern, "\\b"), variable_name, ignore.case = TRUE)
    }))
    
    # Check provider_labels if they exist
    if (length(provider_labels) > 0 && !is.null(provider_labels) && !all(is.na(provider_labels))) {
      label_match <- any(sapply(provider_labels, function(label) {
        if (is.na(label) || is.null(label)) return(FALSE)
        any(sapply(patterns, function(pattern) {
          grepl(paste0("\\b", pattern, "\\b"), label, ignore.case = TRUE)
        }))
      }))
    } else {
      label_match <- FALSE
    }
    
    name_match || label_match
  }
  
  # Define patterns for latitude and longitude
  lat_patterns <- c("latitude", "lat")
  lng_patterns <- c("longitude", "long", "lng")
  
  # Find latitude-like and longitude-like variables
  lat_vars <- variables %>%
    filter(sapply(seq_len(nrow(.)), function(i) is_variable_like(.[i, ], lat_patterns))) %>%
    pull(variable)
  
  lng_vars <- variables %>%
    filter(sapply(seq_len(nrow(.)), function(i) is_variable_like(.[i, ], lng_patterns))) %>%
    pull(variable)
  
  total_geocoord_vars <- length(lat_vars) + length(lng_vars)
  
  # Case 1: No geocoordinate variables found
  if (total_geocoord_vars == 0) {
    return(list(valid = TRUE))
  }
  
  # Case 2: Only one geocoordinate variable (orphan)
  if (total_geocoord_vars == 1) {
    orphan_var <- c(lat_vars, lng_vars)[1]
    return(list(
      valid = TRUE,
      message = paste0("Advisory: Found single geocoordinate variable '", orphan_var, 
                      "' without its pair. Please check that geocoordinate variables have not been missed due to mislabelling.")
    ))
  }
  
  # Case 3: More than 2 geocoordinate variables
  if (total_geocoord_vars > 2) {
    all_vars <- c(lat_vars, lng_vars)
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0("Found more than 2 geocoordinate variables: ", 
                      paste(all_vars, collapse = ", "), 
                      ". Expected exactly one latitude and one longitude variable.")
    ))
  }
  
  # Case 4: Exactly 2 variables - validate they are one lat and one lng
  if (length(lat_vars) != 1 || length(lng_vars) != 1) {
    all_vars <- c(lat_vars, lng_vars)
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0("Found 2 geocoordinate variables but not exactly one latitude and one longitude: ", 
                      paste(all_vars, collapse = ", "), 
                      ". Expected exactly one of each type.")
    ))
  }
  
  # Validate the latitude variable
  lat_var <- lat_vars[1]
  lat_metadata <- variables %>% filter(variable == lat_var)
  
  lat_issues <- c()
  if (is.na(lat_metadata$stable_id) || lat_metadata$stable_id != 'OBI_0001620') {
    lat_issues <- c(lat_issues, paste0("Latitude variable '", lat_var, "' must have stable_id = 'OBI_0001620'"))
  }
  if (is.na(lat_metadata$data_type) || lat_metadata$data_type != 'number') {
    lat_issues <- c(lat_issues, paste0("Latitude variable '", lat_var, "' must have data_type = 'number'"))
  }
  
  # Validate the longitude variable
  lng_var <- lng_vars[1]
  lng_metadata <- variables %>% filter(variable == lng_var)
  
  lng_issues <- c()
  if (is.na(lng_metadata$stable_id) || lng_metadata$stable_id != 'OBI_0001621') {
    lng_issues <- c(lng_issues, paste0("Longitude variable '", lng_var, "' must have stable_id = 'OBI_0001621'"))
  }
  if (is.na(lng_metadata$data_type) || lng_metadata$data_type != 'longitude') {
    lng_issues <- c(lng_issues, paste0("Longitude variable '", lng_var, "' must have data_type = 'longitude'"))
  }
  
  # Combine all issues
  all_issues <- c(lat_issues, lng_issues)
  
  if (length(all_issues) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste(all_issues, collapse = "\n")
    ))
  }
  
  # All validations passed
  list(valid = TRUE)
}

