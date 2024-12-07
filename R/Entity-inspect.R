#' Inspect Generic
#'
#' Defines the S4 generic for the inspect function.
#' 
#' @param entity The object to inspect.
#' @export
setGeneric("inspect", function(entity) standardGeneric("inspect"))

#' Inspect an Entity Object
#'
#' Provides a summary view of the entity's
#' 1. metadata
#' 2. ID columns
#' 3. variables' metadata
#' 4. variable data
#'
#' @param entity An Entity object to inspect.
#' @export
setMethod("inspect", "Entity", function(entity) {
  # Extract data and variables
  data <- entity@data
  variables <- entity@variables
  
  # Ensure variables has `data_type` and `data_shape`
  if (!all(c("data_type", "data_shape") %in% colnames(variables))) {
    stop("Variables metadata must contain `data_type` and `data_shape` columns.")
  }
  
  ids_metadata <- variables %>% filter(data_type == 'id')
  variables_metadata <- variables %>%
    filter(data_type != 'id') %>%
    select(-starts_with('entity_')) %>%
    arrange(display_order)
  
  cat("TBC: entity-level metadata summarised here.\n")
  
  cat("\nID columns:\n")
  print(ids_metadata %>% select(variable, entity_name, entity_level))
  
  cat("\nVariable columns:\n")
  print(variables_metadata)
  
  cat("\nskim() summary of variable data:\n")
  
  skim_data <- data %>% select(-all_of(ids_metadata$variable)) %>% skim()
  print(skim_data, include_summary = FALSE)
})
