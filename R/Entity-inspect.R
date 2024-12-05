#' Inspect Generic
#'
#' Defines the S4 generic for the inspect function.
#' 
#' @param object The object to inspect.
#' @export
setGeneric("inspect", function(object) standardGeneric("inspect"))

#' Inspect an Entity Object
#'
#' Provides a summary view of the entity's
#' 1. metadata
#' 2. ID columns
#' 3. variable columns and metadata
#' 4. variable data
#'
#' @param object An Entity object to inspect.
#' @export
setMethod("inspect", "Entity", function(object) {
  # Extract data and metadata
  data <- object@data
  metadata <- object@metadata
  
  # Ensure metadata has `data_type` and `data_shape`
  if (!all(c("data_type", "data_shape") %in% colnames(metadata))) {
    stop("Metadata must contain `data_type` and `data_shape` columns.")
  }
  
  ids_metadata <- metadata %>% filter(data_type == 'id')
  variables_metadata <- metadata %>% filter(data_type != 'id')
  
  cat("TBC: entity-level metadata summarised here.\n")
  
  cat("\nID columns:\n")
  print(ids_metadata %>% select(variable))
  
  cat("\nVariable columns:\n")
  print(variables_metadata)
  
  cat("\nskim() summary of variable data:\n")
  
  data %>% select(-all_of(ids_metadata$variable)) %>% skim()
})
