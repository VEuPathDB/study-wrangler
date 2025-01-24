library(glue)
library(knitr)

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
  # get the non-id column metadata (aka variables)
  regular_metadata <- entity %>% get_variable_and_category_metadata()
  # get the same plus auto-generated fields like range_min, range_max, vocabulary...
  hydrated_metadata <- entity %>% get_hydrated_variable_and_category_metadata()
  
  # Validate input
  if (!variable_name %in% regular_metadata$variable) {
    if (variable_name %in% entity@variables$variable) {
      # it must be an ID column if it's not a variable or category
      stop(glue("Error: '{variable_name}' is an ID column, not a variable or category column."))
    }
    stop(glue("Error: variable name '{variable_name}' not found in Entity variables' metadata."))
  }
  
  # Extract metadata for the specified variable
  variable_metadata_orig <- regular_metadata %>% filter(variable == variable_name)
  variable_metadata <- hydrated_metadata %>% filter(variable == variable_name)

  # get the attributes that were auto-generated/hydrated
  read_only_attributes <- setdiff(names(hydrated_metadata), names(regular_metadata))
  original_attributes <- names(regular_metadata)
  
  # Print detailed metadata
  cat(
    to_lines(
      heading(glue("Metadata for '{variable_name}'")),
      kable(
        variable_metadata %>% 
          # select(-starts_with("entity_")) %>%            # Exclude columns that start with "entity_"
          mutate(across(where(is.list), ~ map_chr(.x, ~ paste(.x, collapse = ", ")))) %>% # Format list columns as comma-separated strings
          mutate(across(everything(), as.character)) %>% # Convert all columns to character
          pivot_longer(cols = everything(), names_to = "Field", values_to = "Value") %>%
          rowwise() %>%
          mutate(Field = case_when(
            Field %in% original_attributes &&
              is.na(variable_metadata_orig[, Field]) &&
              !is.na(variable_metadata[, Field]) ~ paste0(Field, " (+)"),
            Field %in% read_only_attributes ~ paste0(Field, " (*)"),
            TRUE ~ Field
          )) %>%
          ungroup()
      ),
      "~~~~",
      "Fields marked with an plus (+) have derived default values but may be overridden.",
      "Fields marked with an asterisk (*) are derived and read-only."
    )
  )
      
  # if this is an actual variable with data:
  if (all(variable_metadata$has_values)) {
    # Extract data for the specified variable
    variable_data <- entity@data[[variable_name]]
    
    cat(
      to_lines(
        # Print summary of data
        heading(glue("Summary of data for '{variable_name}'")),
        
        kable(
          skim(variable_data) %>%
            as_tibble() %>%
            mutate(across(everything(), as.character)) %>% # Convert all columns to character
            pivot_longer(
              cols = -c(skim_type, skim_variable),
              names_to = "Metric",
              values_to = "Value"
            ) %>%
            select(Metric, Value) # Keep only relevant columns
        )
      )
    )
    
    # If the variable is a factor and has more than 5 levels, show all levels with counts
    if (is.factor(variable_data) && nlevels(variable_data) > 5) {
      cat(
        to_lines(
          heading(glue("Full factor levels (aka vocabulary) with counts for {variable_name}")),
          kable(
            tibble(
              Value = levels(variable_data),
              Count = as.numeric(table(variable_data))
            )
          )
        )
      )
    }
  } else {
    # it's a category "variable"
    # we can list its children instead
    cat(to_lines(
      heading(glue("Children of category '{variable_name}'")),
      kable(
        hydrated_metadata %>%
          filter(parent_variable == variable_name) %>%
          select(variable, data_type, display_name, stable_id) %>%
          mutate(
            type = if_else(data_type == 'category', 'category', 'variable')
          ) %>%
          select(-data_type) %>%
          relocate(type, .after = variable)
      )
    ))
  }
})
