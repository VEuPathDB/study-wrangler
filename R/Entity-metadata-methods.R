#' File: Entity metadata management methods
#'
#' Generics (attempting to remove redundancy from documentation)
#' 
#' @export
setGeneric("infer_missing_data_types", function(entity) standardGeneric("infer_missing_data_types"))
#' @export
setGeneric("infer_missing_data_shapes", function(entity) standardGeneric("infer_missing_data_shapes"))
#' @export
setGeneric("set_entity_metadata", function(entity, ...) standardGeneric("set_entity_metadata"))



#' infer_missing_data_types
#' 
#' Infers `data_type` metadata for columns where this is currently `NA`.
#' 
#' @param entity an Entity object
#' @returns modified entity
#' @export
setMethod("infer_missing_data_types", "Entity", function(entity) {

  variables <- entity@variables
  data <- entity@data

  # use infer_column_data_type(data_column) to fill in NAs in variables$data_type column
  variables <- variables %>%
    rowwise() %>% # performance is not critical here
    mutate(data_type = if_else(is.na(data_type), infer_data_type(data, variable), data_type)) %>%
    ungroup() # remove special rowwise grouping

  # clone and modify original entity argument
  entity <- initialize(entity, variables=variables)
  return(entity)
})

#' infer_missing_data_shapes
#' 
#' Infers `data_shape` metadata for columns where this is currently `NA`
#' Also converts non-continuous columns to factors and sets their `vocabulary`
#' 
#' @param entity an Entity object
#' @returns modified entity
#' @export
setMethod("infer_missing_data_shapes", "Entity", function(entity) {
  data <- entity@data
  variables <- entity@variables
  
  # only infer for data_shape == NA and non-ID cols
  cols_to_infer <- variables %>%
    filter(is.na(data_shape)) %>%
    filter(!is.na(data_type)) %>%
    filter(data_type != 'id') %>%
    pull(variable)
  
  # infer data_shape as continuous or categorical
  # (further refinement to 'ordinal' will require user-input)
  variables <- variables %>% mutate(
    data_shape = if_else(
      variable %in% cols_to_infer,
      case_when(
        data_type %in% c('number', 'integer', 'date') ~ 'continuous',
        .default = 'categorical'
      ),
      data_shape
    )
  )

  # mutate non-continuous columns into factors only for
  # columns that we previously inferred (`cols_to_infer`)
  factor_vars <- variables %>%
    filter(variable %in% cols_to_infer) %>%
    filter(data_shape != "continuous") %>% pull(variable)

  data <- data %>%
    mutate(across(all_of(factor_vars), as.factor))
  
  # Set the vocabulary metadata for the factor variables
  variables <- variables %>%
    rowwise() %>% # for simplicity, not speed
    mutate(vocabulary = list(
      if (variable %in% factor_vars) {
        levels(data[[variable]])
      } else {
        vocabulary
      }
    )) %>%
    ungroup()
  
  # clone and modify original entity argument
  entity <- initialize(entity, data=data, variables=variables)
  return(entity)
})



#' set_entity_metadata
#' 
#' Sets metadata such as `name`, `display_name`, etc (see "Entity-class.R")
#' 
#' @param entity an Entity object
#' @param ... key = value pairs for setting metadata
#' @returns modified entity
#' @export
setMethod("set_entity_metadata", "Entity", function(entity, ...) {
  metadata <- list(...)
  
  # Validate metadata keys
  validate_entity_metadata_names(metadata)
  
  # Merge new metadata with the existing entity slots
  current_metadata <- as_list(entity)
  updated_metadata <- modifyList(current_metadata, metadata)
  
  # Apply defaults
  updated_metadata <- apply_entity_metadata_defaults(updated_metadata)
  
  # Re-initialize the entity
  do.call(initialize, c(entity, updated_metadata))
})
