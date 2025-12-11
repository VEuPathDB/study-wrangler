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

#' Registration order counter
#' @keywords internal
.registration_counter <- 0

#' Register a validator function
#'
#' @param name Character string name for the validator
#' @param validator_func Function that takes an Entity/Study object and returns validation result
#' @param object_type Character vector specifying what objects this validator works on: "entity", "study", or "both"
#' @param profiles Character vector of profiles this validator applies to
#' @param description Optional description of what the validator checks
#' @param stop_on_error Logical, whether to stop validation on first error from this validator
#' @keywords internal
register_validator <- function(name, validator_func, object_type = "entity", profiles = "baseline", description = NULL, stop_on_error = FALSE) {
  # Increment registration counter
  .registration_counter <<- .registration_counter + 1
  
  .validator_registry[[name]] <- list(
    func = validator_func,
    object_type = object_type,
    profiles = profiles,
    description = description,
    stop_on_error = stop_on_error,
    registration_order = .registration_counter
  )
}

#' Get validators for specified profiles and object type
#'
#' @param profiles Character vector of validation profiles
#' @param object_type Character string: "entity" or "study"
#' @return List of validator functions with metadata
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
  
  # Sort by stop_on_error (TRUE first), then by registration order
  sorted_validators <- matching_validators[
    order(
      sapply(matching_validators, function(v) !v$stop_on_error),  # stop_on_error TRUE first
      sapply(matching_validators, function(v) v$registration_order)  # then by registration order
    )
  ]
  
  # Return validators with metadata
  sorted_validators
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
      description = character(0),
      stop_on_error = logical(0),
      registration_order = integer(0)
    ))
  }
  
  tibble(
    name = names(all_validators),
    object_type = sapply(all_validators, function(v) paste(v$object_type, collapse = ", ")),
    profiles = sapply(all_validators, function(v) paste(v$profiles, collapse = ", ")),
    description = sapply(all_validators, function(v) v$description %||% ""),
    stop_on_error = sapply(all_validators, function(v) v$stop_on_error %||% FALSE),
    registration_order = sapply(all_validators, function(v) v$registration_order %||% 0L)
  )
}

# ============================================================================
# ENTITY AND STUDY VALIDATORS ARE NOW IN SEPARATE FILES
# ============================================================================

# ============================================================================
# VALIDATOR INITIALIZATION
# ============================================================================

#' Initialize baseline validators
#' @keywords internal
.init_baseline_validators <- function() {
  # Basic Entity validators (fatal - stop on error)
  register_validator("entity_metadata_not_empty", validate_entity_metadata_not_empty, 
                    "entity", "baseline", "Check that entity metadata is not empty", stop_on_error = TRUE)
  
  register_validator("entity_data_has_columns", validate_entity_data_has_columns,
                    "entity", "baseline", "Check that entity data has columns", stop_on_error = TRUE)
  
  register_validator("entity_column_alignment", validate_entity_column_alignment,
                    "entity", "baseline", "Check column alignment between data and metadata", stop_on_error = TRUE)
  
  register_validator("entity_required_metadata", validate_entity_required_metadata,
                    "entity", "baseline", "Check for required metadata columns", stop_on_error = TRUE)
  
  register_validator("entity_metadata_types", validate_entity_metadata_types,
                    "entity", "baseline", "Check metadata values respect types and factor levels", stop_on_error = TRUE)
  
  register_validator("entity_data_type_not_na", validate_entity_data_type_not_na,
                    "entity", "baseline", "Check data_type is not NA for any variable", stop_on_error = TRUE)
  
  register_validator("entity_has_name", validate_entity_has_name,
                    "entity", "baseline", "Check entity has a name")
  
  register_validator("entity_units_on_numeric_only", validate_entity_units_on_numeric_only,
                    "entity", "baseline", "Check units only on numeric variables")
  
  # ID validators
  register_validator("entity_id_no_na", validate_entity_id_columns_no_na,
                    "entity", "baseline", "Check for NA values in ID columns")
  
  register_validator("entity_id_no_duplicates", validate_entity_id_columns_no_duplicates,
                    "entity", "baseline", "Check for duplicate values in ID columns")
  
  register_validator("entity_has_id_column", validate_entity_has_id_column,
                    "entity", "baseline", "Check entity has required ID column")
  
  register_validator("entity_one_id_per_level", validate_entity_one_id_per_level,
                    "entity", "baseline", "Check only one ID column per entity level")
  
  register_validator("entity_id_entity_name_match", validate_entity_id_entity_name_match,
                    "entity", "baseline", "Check ID column entity_name matches entity name")
  
  # Data type validators
  register_validator("entity_integer_data_types", validate_entity_integer_data_types,
                    "entity", "baseline", "Check integer variables contain integers")

  register_validator("entity_number_data_types", validate_entity_number_data_types,
                    "entity", "baseline", "Check number variables contain numeric values")

  register_validator("entity_string_data_types", validate_entity_string_data_types,
                    "entity", "baseline", "Check string variables contain character values")

  register_validator("entity_string_data_shapes", validate_entity_string_data_shapes,
                    "entity", "baseline", "Check string variables have appropriate data_shape")

  register_validator("entity_date_data_types", validate_entity_date_data_types,
                    "entity", "baseline", "Check date variables are R date type")
  
  register_validator("entity_multi_valued_character", validate_entity_multi_valued_character,
                    "entity", "baseline", "Check multi-valued variables are character type")
  
  register_validator("entity_ordinal_factors", validate_entity_ordinal_factors,
                    "entity", "baseline", "Check ordinal variables are factors")
  
  register_validator("entity_ordinal_levels", validate_entity_ordinal_levels,
                    "entity", "baseline", "Check ordinal levels consistency")

  register_validator("entity_multivalued_ordinal_levels", validate_entity_multivalued_ordinal_levels,
                    "entity", "baseline", "Check multi-valued ordinal expanded values are in ordinal_levels")

  # Relationship validators
  register_validator("entity_parent_variable_exists", validate_entity_parent_variable_exists,
                    "entity", "baseline", "Check parent_variable references exist")
  
  register_validator("entity_no_circular_graph", validate_entity_no_circular_graph,
                    "entity", "baseline", "Check for circular paths in variable tree")
  
  # Collection validators
  register_validator("entity_collections_have_categories", validate_entity_collections_have_categories,
                    "entity", "baseline", "Check collections have corresponding variable categories")
  
  register_validator("entity_collections_required_metadata", validate_entity_collections_required_metadata,
                    "entity", "baseline", "Check collections have required metadata")
  
  register_validator("entity_collections_homogeneous", validate_entity_collections_homogeneous,
                    "entity", "baseline", "Check collection child variables have consistent metadata")
  
  # Study validators
  register_validator("study_has_name", validate_study_has_name,
                    "study", "baseline", "Check study has a name")
  
  register_validator("study_entity_relationships", validate_study_entity_relationships,
                    "study", "baseline", "Check parent-child entity relationships")
}

#' Initialize EDA validators
#' @keywords internal
.init_eda_validators <- function() {
  # EDA geocoordinate validators
  register_validator("geocoordinate_variables", validate_geocoordinate_variables,
                    "entity", "eda", "Validate geocoordinate variables have correct metadata")

  # EDA display_name validators
   register_validator("eda_variable_display_name_not_null", validate_eda_variable_display_name_not_null,
                     "entity", "eda", "Validate display_name is not null for any variable")
}
