library(glue)

#' Inspect Variable Generic
#'
#' Defines the S4 generic for inspecting a specific variable in an Entity.
#' 
#' @param entity The object to inspect.
#' @param variable_name The name of the variable to inspect.
#' @export
setGeneric("inspect_variable", function(entity, variable_name) standardGeneric("inspect_variable"))


#' Inspect a Specific Variable in an Entity
#'
#' Provides detailed metadata and a summary for a specified variable in an Entity object.
#'
#' @param entity An Entity object containing the variable.
#' @param variable_name The name of the variable to inspect, as a character string.
#' @export
setMethod("inspect_variable", "Entity", function(entity, variable_name) {
  # Validate input
  if (!variable_name %in% entity@variables$variable) {
    stop("Variable name not found in Entity variables' metadata.")
  }
  
  # Extract metadata for the specified variable
  variable_metadata <- entity@variables %>% filter(variable == variable_name)
  
  # Extract data for the specified variable
  variable_data <- entity@data[[variable_name]]
  
  # Print detailed metadata
  cat(glue("Metadata for {variable_name}\n", .trim = FALSE))
  print(n=50, variable_metadata %>% 
          select(-starts_with("entity_")) %>%            # Exclude columns that start with "entity_"
          mutate(across(where(is.list), ~ map_chr(.x, ~ paste(.x, collapse = ", ")))) %>% # Format list columns as comma-separated strings
          mutate(across(everything(), as.character)) %>% # Convert all columns to character
          pivot_longer(cols = everything(), names_to = "Field", values_to = "Value"))
  
  # Print summary of data
  cat(glue("\nSummary of data for {variable_name}:\n", .trim = FALSE))

  skim_summary <- skim(variable_data) %>%
    as_tibble() %>%
    mutate(across(everything(), as.character)) %>% # Convert all columns to character
    pivot_longer(
      cols = -c(skim_type, skim_variable),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    select(Metric, Value) # Keep only relevant columns
  
  print(skim_summary)
})
