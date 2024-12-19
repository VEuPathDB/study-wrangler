library(knitr)

#' Inspect an Entity Object
#'
#' Provides a summary view of the entity's variables and data or redirects
#' to inspect a specific variable if a variable name is provided.
#'
#' @param entity An Entity object to inspect.
#' @param variable_name An optional character string specifying a variable to inspect.
#' @export
setMethod("inspect", "Entity", function(object, variable_name = NULL) {
  entity <- object

  if (!is.null(variable_name)) {
    # Delegate to inspect_variable and return early
    return(inspect_variable(entity, variable_name))
  }

  # Extract data and variables
  data <- entity@data
  variables <- entity@variables
  
  # Ensure variables has `data_type` and `data_shape`
  if (!all(c("data_type", "data_shape") %in% colnames(variables))) {
    stop("Error: variables metadata must contain `data_type` and `data_shape` columns.")
  }
  
  ids_metadata <- get_id_column_metadata(entity)
  variables_metadata <- get_variable_metadata(entity)

  # entity level metadata  
  slots_list <- as_list(entity)
  character_slots <- slots_list[lapply(slots_list, class) == "character"]
  cat(
    to_lines(
      heading("Entity-level metadata"),
      kable(
        tibble(
          Field = names(character_slots),
          Value = unlist(character_slots)
        )
      )
    )
  )
  
  # some row count stats
  cat(
    to_lines(
      heading("Row counts"),
      kable(
        tibble(
          Type = c(
            "Total",
            "Complete data (no missing values)"
          ),
          Count = c(
            nrow(data),
            data %>% filter(if_all(everything(), ~ !is.na(.))) %>% nrow()
          )
        )
      ),

      heading("ID columns"),
      kable(ids_metadata %>% select(variable, entity_name, entity_level)),
      glue("
~~~~
If you see variables in the table above that should not be handled as IDs
then you can redo the automatic column type detection with:
`redetect_columns(c('col_name1', 'col_name_2`))`
~~~~
If there are ID columns missing above, you may need to use:
`set_parents(names=c('parent_name', 'grandparent_name'), columns=c('parent.id', 'grandparent.id'))`
"),
      
      heading("Key variable metadata"),
      kable(variables_metadata %>%
        select(variable, provider_label, data_type, data_shape, display_name, stable_id)),
      "~~~~",
      "Use `inspect(entity, 'variable.name')` for more detail on individual variables",
      
      heading("Variable annotation summary"),
      kable(
        tibble(
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
        )
      ),
      "~~~~",
      "* use `set_variable_display_names_from_provider_labels()` to use original column headings as-is."
    )
  )

  if (nrow(variables_metadata)) {
    skim_data <- data %>% select(-all_of(ids_metadata$variable)) %>% skim()
    cat(
      to_lines(
        heading("Summary of variable values and distributions"),
        capture_skim(skim_data, include_summary = FALSE)
      )
    )
  }
})
