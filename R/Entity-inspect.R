library(knitr)

#' Inspect Generic
#'
#' Defines the S4 generic for the inspect function.
#' 
#' @param entity The object to inspect.
#' @param ... Additional arguments for customization.
#' @export
setGeneric("inspect", function(entity, ...) standardGeneric("inspect"))

#' Inspect an Entity Object
#'
#' Provides a summary view of the entity's variables and data or redirects
#' to inspect a specific variable if a variable name is provided.
#'
#' @param entity An Entity object to inspect.
#' @param variable_name An optional character string specifying a variable to inspect.
#' @export
setMethod("inspect", "Entity", function(entity, variable_name = NULL) {
  if (!is.null(variable_name)) {
    # Delegate to inspect_variable and return early
    inspect_variable(entity, variable_name)
    return()
  }

  # Extract data and variables
  data <- entity@data
  variables <- entity@variables
  
  # Ensure variables has `data_type` and `data_shape`
  if (!all(c("data_type", "data_shape") %in% colnames(variables))) {
    stop("Error: variables metadata must contain `data_type` and `data_shape` columns.")
  }
  
  ids_metadata <- variables %>% filter(data_type == 'id')
  variables_metadata <- variables %>%
    filter(data_type != 'id') %>%
    select(-starts_with('entity_')) %>%
    arrange(display_order)
  
  cat("Entity-level metadata:")
  slots_list <- as_list(entity)
  character_slots <- slots_list[lapply(slots_list, class) == "character"]
  print(kable(tibble(
    Field = names(character_slots),
    Value = unlist(character_slots)
  )))
  
  # some row count stats
  cat("\nRow counts:")
  print(kable(tibble(
    Type = c(
      "Total",
      "No missing values"
    ),
    Count = c(
      nrow(data),
      data %>% filter(if_all(everything(), ~ !is.na(.))) %>% nrow()
    )
  )))

  cat("\n\nID columns:")
  print(kable(ids_metadata %>% select(variable, entity_name, entity_level)))
  
  cat("\nKey variable metadata:\n(use `inspect(entity, 'variable.name')` for more detail)")
  print(kable(variables_metadata %>%
    select(variable, provider_label, data_type, data_shape, display_name, stable_id)
  ))
  
  cat("\nVariable annotation summary:")
  print(kable(tibble(
    Type = c(
      "Total number of variables",
      "display_name provided*",
      "definition provided"
    ),
    Count = c(
      nrow(variables_metadata),
      variables_metadata %>% filter(!is.na(display_name)) %>% nrow(),
      variables_metadata %>% filter(!is.na(definition)) %>% nrow()
    )
  )))
  cat("* use `set_variable_display_names_from_provider_labels()` to use original column headings as-is.\n")
  
  cat("\nSummary of variable values and distributions:\n")
  
  skim_data <- data %>% select(-all_of(ids_metadata$variable)) %>% skim()
  print(skim_data, include_summary = FALSE)
})
