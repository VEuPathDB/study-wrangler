library(knitr)
library(glue)

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

  global_varname <- find_global_varname(object, 'entity')
  
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

  entity_name <- entity %>% get_entity_name()
  
  variables_metadata <- if (is_truthy(entity_name)) {
    get_hydrated_variable_and_category_metadata(entity)
  } else {
    message(to_lines(c(
      "Warning: because this entity has no `name`, default `stable_id` attributes cannot be generated.",
      glue("Run `validate({global_varname})` for more details.")
    )))
    get_variable_and_category_metadata(entity)
  }
  
  category_metadata <- get_category_metadata(entity)
  
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
      kable(
        ids_metadata %>%
          select(variable, entity_name, entity_level) %>%
          rename(`ID column` = variable)
      ),
      glue("
~~~~
If you see variables in the table above that should not be handled as IDs
then you can redo the automatic column type detection with:
  {global_varname} <- {global_varname} %>% redetect_columns_as_variables(c('col_name.1', 'col_name.2'))
~~~~
If there are ID columns missing above, you may need to use:
  {global_varname} <- {global_varname} %>% set_parents(names=c('parent_name', 'grandparent_name'), id_columns=c('parent.id', 'grandparent.id'))
"),
      
      heading("Summary of important metadata for all variables and categories"),
      kable(variables_metadata %>%
        select(variable, provider_label, data_type, data_shape, display_name, stable_id, is_multi_valued) %>%
        rename(`var. or cat. name` = variable)
      ),
      "~~~~",
      glue("Use `{global_varname} %>% inspect('variable.name')` for full details on individual variables"),
      "If numeric or date variables are shown as string/categorical they may be delimited multi-value columns.",
      "You can set them to multi-valued as follows:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variables_multivalued('variable.1.name' = 'delimiter.1', 'variable.2.name' = 'delimiter.2')")),

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
      "* to use original column headings for display names, use the following command:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variable_display_names_from_provider_labels()"))
    )
  )

  multivalued_metadata <- variables_metadata %>%
    filter(has_values & is_multi_valued)
  
  ### values ###
  if (nrow(variables_metadata)) {
    univalued_variables <- variables_metadata %>%
      filter(has_values & !is_multi_valued) %>%
      pull(variable)
    
    string_variables <- variables_metadata %>%
      filter(has_values & data_type == 'string') %>%
      pull(variable)
    
    skim_data <- data %>%
      select(all_of(univalued_variables)) %>%
      mutate(across(any_of(string_variables), as.factor)) %>%
      skim()

    cat(
      to_lines(
        heading("Summary of variable values and distributions"),
        "Note: the following summaries are are made directly from the data table of this entity",
        "with the skimr package.",
        if (nrow(multivalued_metadata) > 0)
          "Multiple-valued variables, if present, are not included. See the dedicated section below."
        else
          "",
        "Variables with data_type = 'string' are presented as R factors for improved readability.\n",
        capture_skim(skim_data, include_summary = FALSE)
      )
    )
  }
  
  ### multi-valued variable summary ###
  #   glue("of the expanded multiple values, use `{global_varname} %>% inspect('variable.name')`"),
  if (nrow(multivalued_metadata) > 0) {
    safe_entity_name <- if (is_truthy(entity_name)) entity_name else 'entity'

    cat(
      to_lines(
        heading("Multi-valued variables summary"),
        kable(
          multivalued_metadata %>%
            rowwise() %>%
            mutate(
              value_counts = list(data %>% pull(variable) %>% str_count(multi_value_delimiter) %>% + 1),
              missing = data %>% pull(variable) %>% is.na() %>% sum(),
              total = value_counts %>% sum(na.rm = TRUE),
              min_values = value_counts %>% min(na.rm = TRUE),
              median_values = value_counts %>% median(na.rm = TRUE),
              max_values = value_counts %>% max(na.rm = TRUE),
              "values per row (min/med/max)" = glue("{min_values} / {median_values} / {max_values}"),
              value_counts = NULL,
              min_values = NULL,
              median_values = NULL,
              max_values = NULL,
              median = NULL, # this column wasn't "used" but appears anyway - possible bug!
              .keep = "used"
            ) %>%
            ungroup() %>%
            rename(
              delimiter = multi_value_delimiter,
              "NA rows" = missing,
              "total values" = total
            )
        )
      )
    )
  }
  
  ### units ###
  cat(
    to_lines(
      heading("Summary of units for numeric variables"),
      units_summary(entity)
    )
  )
  
  
  ### variable tree ###
  cat(
    to_lines(
      heading("Variable categories/organisation"),
      variable_ascii_tree(entity)
    )
  )
})


#'
#' If there are no non-NA values for parent_variable this will return
#' a message saying there's no variable tree/hierarchy
#' @param entity an Entity object
#' @return printable string
#'
variable_ascii_tree <- function(entity) {
  metadata <- entity %>% get_variable_and_category_metadata()
  global_varname <- find_global_varname(entity, 'entity')
  
  if (metadata %>% pull(parent_variable) %>% is.na() %>% all()) {
    return(to_lines(
      "This entity currently has no variable categorization.",
      "You may optionally use a command similar to the below to organise your variables:",
      indented(glue("{global_varname} <- {global_varname} %>% create_variable_category('category_name', children=c('var.1', 'var.2'))"))
    ))
  }
  
  # add parent_variable where it is NA
  root_name <- entity %>% get_entity_name()
  
  # check root_name isn't already in metadata$variable
  # if it is, then use '_root_' instead
  if (root_name %in% metadata$variable) {
    root_name <- '_root_'
  }
  
  metadata <- metadata %>%
    mutate(
      parent_variable = if_else(
        is.na(parent_variable),
        root_name,
        parent_variable
      )
    ) %>%  # and select only the salient columns
    select(
      variable,
      parent_variable,
      display_name,
      display_order
    )
  
  # and then add a row for '_root_'
  metadata <- metadata %>%
    bind_rows(
      tibble(
        variable = root_name,
        parent_variable = NA,
        display_name = entity %>% get_display_name(),
        display_order = NA
      )
    )
  
  # some functions for creating an object we can use in the ascii tree
  # function
  
  # a simple object that contains a tibble and a row identifier
  # a kind of row-pointer (assumes tibble contains a column with the row name)
  create_row_ptr <- function(tibble, row_name) {
    list(
      tibble = tibble,
      row_name = row_name
    )
  }
  
  get_label_fn <- function(row_ptr) {
    row <- row_ptr$tibble %>% filter(variable == row_ptr$row_name)
    return(
      glue("{row$variable}{if (is_truthy(row$display_name)) glue(' ({row$display_name})') else ''}")
    )
  }
  
  get_children_fn <- function(row_ptr) {
    all_rows <- row_ptr$tibble
    parent_name <- row_ptr$row_name
    # Define how to find children based on the parent row
    return(
      all_rows %>%
        filter(parent_variable == parent_name) %>%
        arrange(display_order) %>%
        pull(variable) %>%
        map(
          function(var) {
            return(create_row_ptr(all_rows, var))
          }
        )
    )
  }
  
  root <- create_row_ptr(metadata, root_name)
  
  return(
    c(
      recursive_ascii_tree(
        root,
        prefix = "",
        is_last = TRUE,
        is_root = TRUE,
        get_label_fn = get_label_fn,
        get_children_fn = get_children_fn
      ),
      "~~~~",
      "The entity/variable/category display_name is shown in parentheses, where available."
    )
  )
}


units_summary <- function(entity) {
  global_varname <- find_global_varname(entity, 'entity')
  
  numeric_vars_metadata <- entity %>%
    get_variable_metadata() %>%
    filter(data_type %in% c("integer", "number"))
  
  if (nrow(numeric_vars_metadata) == 0)
    return("This entity has no numeric variables, so no units to summarise.")
 
  advice <- if (numeric_vars_metadata %>% pull(unit) %>% is.na() %>% all()) {
    list(
      "~~~~",
      "None of the variables above have units. They are optional but if you want to add them, use:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variable_metadata('variable.name', unit = 'kg')"))
    )
  } else {
    ""
  }
  
  return(c(
    kable(
      numeric_vars_metadata %>% select(
        variable,
        display_name,
        unit
      )
    ),
    advice
  ))
   
}

