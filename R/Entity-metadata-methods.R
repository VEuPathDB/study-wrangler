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
#' @export
setGeneric("sync_variable_metadata", function(entity, ...) standardGeneric("sync_variable_metadata"))



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
  return(entity %>% initialize(variables=variables))
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
  return(entity %>% initialize(data=data, variables=variables))
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
  updated_metadata <- apply_entity_metadata_defaults(updated_metadata, verbose = TRUE)
  
  # Clone and modify the entity
  return(do.call(initialize, c(entity, updated_metadata)))
})

#' sync_variable_metadata
#' 
#' Fixes `data` and `variables` metadata if they aren't aligned with each other.
#' 
#' 1. If there are data column(s) with no metadata, add metadata rows with default values
#' 2. If there are metadata row(s) with no data, remove those rows
#' 
#' `data` is not touched!
#' 
#' 
#' @param entity an Entity object
#' @param ... additional args TBD
#' @returns modified entity
#' @export
setMethod("sync_variable_metadata", "Entity", function(entity, ...) {
  data <- entity@data
  variables <- entity@variables
  
  missing_variables <- setdiff(colnames(data), variables$variable)
  extra_variables <- setdiff(variables$variable, colnames(data))

  # Early return if no mismatches
  if (length(missing_variables) == 0 && length(extra_variables) == 0) {
    message("No metadata synchronization needed.")
    return(entity)
  }
  
  if (length(missing_variables)) {
    # create new row(s) in variables for these newly appeared data columns
    # we will assume that the column names are "clean"
    missing_metadata <-
      tibble(
        variable=missing_variables,
      ) %>%
      expand_grid(variable_metadata_defaults) %>%
      # and we'll set the provider_label to the same name because that's all we have
      mutate(
        provider_label = missing_variables
      )

    variables <- bind_rows(variables, missing_metadata)
    message(paste(
      "Synced variables metadata by adding defaults for:",
      paste(missing_variables, collapse = ", ")
    ))
  }
    
  if (length(extra_variables)) {
    variables <- variables %>%
      filter(!variable %in% extra_variables)
    message(paste(
      "Synced metadata by removing these variables with no data:",
      paste(extra_variables, collapse = ", ")
    ))
  }
  
  return(
    entity %>%
      initialize(variables=variables) %>%
      infer_missing_data_types() %>%
      infer_missing_data_shapes()
  )
})

  
