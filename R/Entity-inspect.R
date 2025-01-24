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

  variables_metadata <- if (entity %>% get_entity_name() %>% is_truthy()) {
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
      kable(ids_metadata %>% select(variable, entity_name, entity_level)),
      glue("
~~~~
If you see variables in the table above that should not be handled as IDs
then you can redo the automatic column type detection with:
`redetect_columns_as_variables(c('col_name1', 'col_name_2`))`
~~~~
If there are ID columns missing above, you may need to use:
`set_parents(names=c('parent_name', 'grandparent_name'), columns=c('parent.id', 'grandparent.id'))`
"),
      
      heading("Summary of important metadata for all variables and categories"),
      kable(variables_metadata %>%
        select(variable, provider_label, data_type, data_shape, display_name, stable_id)),
      "~~~~",
      glue("Use `{global_varname} %>% inspect('variable.name')` for a lot more detail on individual variables"),
      
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
    skim_data <- data %>%
      select(-all_of(ids_metadata$variable)) %>%
      skim()
    cat(
      to_lines(
        heading("Summary of variable values and distributions"),
        capture_skim(skim_data, include_summary = FALSE)
      )
    )
  }
  
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
  metadata <- entity %>% get_hydrated_variable_and_category_metadata()
  
  if (metadata %>% pull(parent_variable) %>% is.na() %>% all()) {
    return(to_lines(
      "This entity currently has no variable categorization.",
      "You may optionally use `create_variable_category()` to organise your variables."
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
      display_name
    )
  
  # and then add a row for '_root_'
  metadata <- metadata %>%
    bind_rows(
      tibble(
        variable = root_name,
        parent_variable = NA,
        display_name = entity %>% get_display_name()
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
