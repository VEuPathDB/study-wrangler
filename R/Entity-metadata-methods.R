#' File: Entity metadata management methods
#'
#' Generics (attempting to remove redundancy from documentation)
#' 
#' @export
setGeneric("infer_missing_data_types", function(object) standardGeneric("infer_missing_data_types"))
#' @export
setGeneric("infer_missing_data_shapes", function(object) standardGeneric("infer_missing_data_shapes"))
#' @export
setGeneric("infer_missing_data_shapes", function(object) standardGeneric("infer_missing_data_shapes"))



#' infer_missing_data_types
#' 
#' Infers `data_type` metadata for columns where this is currently `NA`.
#' 
#' @param object an Entity object
#' @returns modified entity
#' @export
setMethod("infer_missing_data_types", "Entity", function(object) {

  metadata <- object@metadata
  data <- object@data

  # use infer_column_data_type(data_column) to fill in NAs in metadata$data_type column
  metadata <- metadata %>%
    rowwise() %>% # performance is not critical here
    mutate(data_type = if_else(is.na(data_type), infer_data_type(data, variable), data_type)) %>%
    ungroup() # remove special rowwise grouping

  # clone and modify original entity argument
  entity <- initialize(object, metadata=metadata)
  return(entity)
})

#' infer_missing_data_shapes
#' 
#' Infers `data_shape` metadata for columns where this is currently `NA`
#' Also converts 
#' 
#' @param object an Entity object
#' @returns modified entity
#' @export
setMethod("infer_missing_data_shapes", "Entity", function(object) {
  data <- object@data
  metadata <- object@metadata
  
  cols_to_infer <- metadata %>% filter(is.na(data_shape)) %>% pull(variable)
  
  # infer data_shape as continuous or categorical
  # (further refinement to 'ordinal' will require user-input)
  metadata <- metadata %>% mutate(
    data_shape = if_else(
      is.na(data_shape) & !is.na(data_type),
      case_when(
        data_type %in% c('number', 'integer', 'date') ~ 'continuous',
        .default = 'categorical'
      ),
      data_shape
    )
  )

  # mutate categorical columns into factors only for `cols_to_infer`
  categorical_vars <- metadata %>% filter(data_shape == "categorical") %>% pull(variable)
  data <- data %>%
    mutate(across(all_of(categorical_vars), as.factor))
  
  # clone and modify original entity argument
  entity <- initialize(object, data=data, metadata=metadata)
  return(entity)
})

