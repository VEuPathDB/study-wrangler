#' Validation Plugin System for Study Wrangler
#'
#' This module provides a plugin-based validation system that allows for
#' different validation profiles (baseline, eda, etc.)
#'
#' @name validators
NULL

#' Registry for validation functions
#' @keywords internal
.validator_registry <- new.env(parent = emptyenv())

#' Register a validator function
#'
#' @param name Character string name for the validator
#' @param validator_func Function that takes an Entity/Study object and returns validation result
#' @param object_type Character vector specifying what objects this validator works on: "entity", "study", or "both"
#' @param profiles Character vector of profiles this validator applies to
#' @param description Optional description of what the validator checks
#' @keywords internal
register_validator <- function(name, validator_func, object_type = "entity", profiles = "baseline", description = NULL) {
  .validator_registry[[name]] <- list(
    func = validator_func,
    object_type = object_type,
    profiles = profiles,
    description = description
  )
}

#' Get validators for specified profiles and object type
#'
#' @param profiles Character vector of validation profiles
#' @param object_type Character string: "entity" or "study"
#' @return List of validator functions
#' @keywords internal
get_validators_for_profiles <- function(profiles, object_type) {
  all_validators <- as.list(.validator_registry)
  
  # Filter validators that match profiles and object type
  matching_validators <- all_validators[
    sapply(all_validators, function(v) {
      profile_match <- any(v$profiles %in% profiles)
      type_match <- object_type %in% v$object_type || "both" %in% v$object_type
      profile_match && type_match
    })
  ]
  
  # Return just the functions
  lapply(matching_validators, function(v) v$func)
}

#' List all registered validators
#'
#' @return Tibble with validator information
#' @export
list_validators <- function() {
  all_validators <- as.list(.validator_registry)
  
  if (length(all_validators) == 0) {
    return(tibble(
      name = character(0),
      object_type = character(0),
      profiles = character(0),
      description = character(0)
    ))
  }
  
  tibble(
    name = names(all_validators),
    object_type = sapply(all_validators, function(v) paste(v$object_type, collapse = ", ")),
    profiles = sapply(all_validators, function(v) paste(v$profiles, collapse = ", ")),
    description = sapply(all_validators, function(v) v$description %||% "")
  )
}

# ============================================================================
# ENTITY VALIDATORS
# ============================================================================

#' Validator: Check metadata is not empty
#' @keywords internal
validate_entity_metadata_not_empty <- function(entity) {
  variables <- entity@variables
  if (nrow(variables) == 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = "Variables' metadata is empty. Ensure metadata is correctly populated."
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check data has columns
#' @keywords internal
validate_entity_data_has_columns <- function(entity) {
  data <- entity@data
  if (ncol(data) == 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = "Data contains no columns. Ensure data is correctly delimited and reload."
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check column alignment between data and metadata
#' @keywords internal
validate_entity_column_alignment <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  true_variables <- variables %>% filter(has_values) %>% pull(variable)
  missing_variables <- setdiff(colnames(data), true_variables)
  extra_variables <- setdiff(true_variables, colnames(data))
  
  issues <- c()
  fatal <- FALSE
  
  if (length(missing_variables) > 0) {
    issues <- c(issues, paste("Variable metadata is missing for these data columns:", 
                             paste(missing_variables, collapse = ", ")))
    fatal <- TRUE
  }
  
  if (length(extra_variables) > 0) {
    issues <- c(issues, paste("These variables have metadata but no data columns:",
                             paste(extra_variables, collapse = ", ")))
    fatal <- TRUE
  }
  
  if (length(issues) > 0) {
    return(list(
      valid = FALSE,
      fatal = fatal,
      message = paste(issues, collapse = "\n")
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check for required metadata columns
#' @keywords internal
validate_entity_required_metadata <- function(entity) {
  variables <- entity@variables
  required_metadata_cols <- c('data_shape')
  
  variables_with_critical_NAs <- variables %>%
    filter(!data_type %in% c('id', 'category')) %>%
    select('variable', all_of(required_metadata_cols)) %>%
    filter(if_any(all_of(required_metadata_cols), is.na))
  
  if (nrow(variables_with_critical_NAs) > 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = "NAs found in critical variable metadata columns"
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check entity has a name
#' @keywords internal
validate_entity_has_name <- function(entity) {
  if (is.na(entity@name) || entity@name == '') {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = "Entity is missing required 'name' metadata"
    ))
  }
  list(valid = TRUE)
}

#' Validator: Check for NA values in ID columns
#' @keywords internal
validate_entity_id_columns_no_na <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  id_columns <- variables %>% filter(data_type == "id") %>% pull(variable)
  
  if (length(id_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  na_in_ids <- data %>%
    select(all_of(id_columns)) %>%
    summarise(across(everything(), ~ any(is.na(.)))) %>%
    unlist() %>% as.logical()
  
  if (any(na_in_ids)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste("ID columns contain NA values:", 
                     paste(id_columns[na_in_ids], collapse = ", "))
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check for duplicate values in ID columns
#' @keywords internal
validate_entity_id_columns_no_duplicates <- function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  my_id_columns <- variables %>% 
    filter(data_type == "id") %>% 
    filter(entity_level == 0) %>% 
    pull(variable)
  
  if (length(my_id_columns) == 0) {
    return(list(valid = TRUE))
  }
  
  dupes_in_ids <- data %>%
    select(all_of(my_id_columns)) %>%
    summarise(across(everything(), anyDuplicated)) %>%
    unlist() %>% as.logical()
  
  if (any(dupes_in_ids)) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste("ID columns contain duplicates:", 
                     paste(my_id_columns[dupes_in_ids], collapse = ", "))
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check entity has required ID column
#' @keywords internal
validate_entity_has_id_column <- function(entity) {
  variables <- entity@variables
  
  my_id_variable <- variables %>%
    filter(data_type == 'id') %>%
    filter(entity_level == 0)
  
  if (nrow(my_id_variable) == 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = "This entity appears to have no ID column. It must have a column with a unique value in each row."
    ))
  }
  
  list(valid = TRUE)
}

# ============================================================================
# STUDY VALIDATORS  
# ============================================================================

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

#' Validator: Check all entities in study are valid
#' @keywords internal
validate_study_entities_valid <- function(study) {
  entities <- get_entities(study)
  
  invalid_entities <- c()
  
  for (entity in entities) {
    # Get the validation profiles to use for entity validation
    profiles <- get_validation_profiles()
    entity_validators <- get_validators_for_profiles(profiles, "entity")
    
    # Run entity validators
    for (validator in entity_validators) {
      result <- validator(entity)
      if (!result$valid && (result$fatal %||% FALSE)) {
        invalid_entities <- c(invalid_entities, get_entity_name(entity))
        break  # Stop at first fatal error for this entity
      }
    }
  }
  
  if (length(invalid_entities) > 0) {
    return(list(
      valid = FALSE,
      fatal = TRUE,
      message = paste("Invalid entities found:", paste(invalid_entities, collapse = ", "))
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
      message = "Parent-child entity relationships are problematic"
    ))
  }
  
  list(valid = TRUE)
}

# ============================================================================
# VALIDATOR INITIALIZATION
# ============================================================================

#' Initialize baseline validators
#' @keywords internal
.init_baseline_validators <- function() {
  # Entity validators
  register_validator("entity_metadata_not_empty", validate_entity_metadata_not_empty, 
                    "entity", "baseline", "Check that entity metadata is not empty")
  
  register_validator("entity_data_has_columns", validate_entity_data_has_columns,
                    "entity", "baseline", "Check that entity data has columns")
  
  register_validator("entity_column_alignment", validate_entity_column_alignment,
                    "entity", "baseline", "Check column alignment between data and metadata")
  
  register_validator("entity_required_metadata", validate_entity_required_metadata,
                    "entity", "baseline", "Check for required metadata columns")
  
  register_validator("entity_has_name", validate_entity_has_name,
                    "entity", "baseline", "Check entity has a name")
  
  register_validator("entity_id_no_na", validate_entity_id_columns_no_na,
                    "entity", "baseline", "Check for NA values in ID columns")
  
  register_validator("entity_id_no_duplicates", validate_entity_id_columns_no_duplicates,
                    "entity", "baseline", "Check for duplicate values in ID columns")
  
  register_validator("entity_has_id_column", validate_entity_has_id_column,
                    "entity", "baseline", "Check entity has required ID column")
  
  # Study validators
  register_validator("study_has_name", validate_study_has_name,
                    "study", "baseline", "Check study has a name")
  
  register_validator("study_entities_valid", validate_study_entities_valid,
                    "study", "baseline", "Check all entities in study are valid")
  
  register_validator("study_entity_relationships", validate_study_entity_relationships,
                    "study", "baseline", "Check parent-child entity relationships")
}

# Initialize validators when the package loads
.init_baseline_validators()