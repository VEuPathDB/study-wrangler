#' Validate an Entity Object
#'
#' Performs various checks on data completeness and consistency 
#' 
#'
#' @param entity An Entity object to validate.
#' @returns a Boolean indicating success or failure
#' @export
setMethod("validate", "Entity", function(object) {
  entity <- object
  # Extract data and variables
  data <- entity@data
  variables <- entity@variables
  quiet <- entity@quiet
  
  tools <- create_feedback_tools(quiet = quiet, success_message = "Entity is valid.")
  # the following can be made nicer with library(zeallot)
  add_feedback <- tools$add_feedback
  give_feedback <- tools$give_feedback
  get_is_valid <- tools$get_is_valid

  # name of variable to use in fix-it command suggestions  
  global_varname = find_global_varname(entity, 'entity')
  
  # Fatal Validation: Check if metadata is empty
  if (nrow(variables) == 0) {
    give_feedback(
      fatal_message =
        to_lines(c(
          "Variables' metadata is empty. Ensure metadata is correctly populated.",
          "To reset the metadata to defaults, use the following command:",
          indented(
            glue("{global_varname} <- {global_varname} %>% sync_variable_metadata()")
          )
        ))
    )
    return(invisible(FALSE))
  }
  
  # Fatal Validation: Check if data has no columns
  if (ncol(data) == 0) {
    give_feedback(fatal_message = "Data contains no columns. Ensure data is correctly delimited and reload.")
    return(invisible(FALSE))
  }
  
  # Validation: Check column alignment
  true_variables <- variables %>% filter(has_values) %>% pull(variable)
  missing_variables <- setdiff(colnames(data), true_variables)
  extra_variables <- setdiff(true_variables, colnames(data))
  
  if (length(missing_variables) > 0) {
    give_feedback(fatal_message=to_lines(
      "Variable metadata is missing for these data columns:",
       indented(paste(missing_variables, collapse = ", ")),
      glue("add default metadata with `{global_varname} <- {global_varname} %>% sync_variable_metadata()`")
    ))
    return(invisible(FALSE))
  }
  
  if (length(extra_variables) > 0) {
    give_feedback(fatal_message=to_lines(
      "These variables have metadata but no data columns:",
      indented(paste(extra_variables, collapse = ", ")),
      glue("remove the metadata with `{global_varname} <- {global_varname} %>% sync_variable_metadata()`")
    ))
    return(invisible(FALSE))
  }
  
  # Validation: check that there are no NAs in required columns
  # for variables metadata
  required_metadata_cols = c('data_shape')
  variables_with_critical_NAs <- variables %>%
    filter(!data_type %in% c('id', 'category')) %>%
    select('variable', all_of(required_metadata_cols)) %>%
    filter(if_any(all_of(required_metadata_cols), is.na))
  if (nrow(variables_with_critical_NAs) > 0) {
    give_feedback(fatal_message=to_lines(
      c(
        "Error: NAs found in critical variable metadata columns:",
        kable(variables_with_critical_NAs),
        "~~~~",
        "This error is not expected to occur. Please contact the developers."
      )
    ))
    return(invisible(FALSE))
  }
  
  ### End of fatal checks ###

  # Validation: Check that all metadata values respect the types and factor
  # levels in `variable_metadata_defaults`. We do not expect the factor columns
  # to be actual factors, but the values in them should respect the factors in
  # the defaults.
  
  validate_column <- function(col_name, col_values, default_val) {
    if (is.integer(default_val)) {
      bad <- col_values[!is.na(col_values) & !is.integer(col_values)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are integers", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.character(default_val)) {
      bad <- col_values[!is.na(col_values) & !is.character(col_values)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are character/string", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.logical(default_val)) {
      bad <- col_values[!is.na(col_values) & !(col_values %in% c(TRUE, FALSE))]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Not all values are logical", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.factor(default_val)) {
      lvls <- levels(default_val)
      bad <- col_values[!is.na(col_values) & !(col_values %in% lvls)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Contains values outside factor levels", Values = paste(unique(bad), collapse = ", ")))
    } else if (is.list(default_val) && length(default_val) == 1 && is.factor(default_val[[1]])) {
      # list of factor values
      lvls <- levels(default_val[[1]])
      all_values <- col_values %>% unlist() %>% keep(~ !is.na(.x))
      bad <- all_values[!is.na(all_values) & !(all_values %in% lvls)]
      if (length(bad) > 0) return(tibble(Column = col_name, Issue = "Contains values outside allowed multi-factor levels", Values = paste(unique(bad), collapse = ", ")))
    }
    NULL
  }
  
  issues <- names(variable_metadata_defaults) %>%
    map(~ validate_column(.x, variables[[.x]], variable_metadata_defaults[[.x]])) %>%
    compact() %>%
    bind_rows()

  if (nrow(issues) > 0) {
    give_feedback(
      fatal_message = paste0(
        "Variable metadata contains illegal values:\n",
        paste(kable(issues), collapse = "\n")
      )
    )
    return(invisible(FALSE))
  }
  
  # Validation: is.na(data_type) not allowed for any variable
  missing_data_type <- variables %>%
    filter(is.na(data_type)) %>% pull(variable)
  if (length(missing_data_type) > 0) {
    give_feedback(
      fatal_message = paste0(
        "Metadata data_shape must not be NA for these variables:\n",
        paste(missing_data_type, collapse = ", ")
      )
    )
    return(invisible(FALSE))
  }
  
  # Validation: Check for NA values in 'id' columns
  id_columns <- variables %>% filter(data_type == "id") %>% pull(variable)
  
  na_in_ids <- data %>%
    select(all_of(id_columns)) %>%
    summarise(across(everything(), ~ any(is.na(.)))) %>%
    unlist() %>% as.logical()
  
  if (any(na_in_ids)) {
    add_feedback(paste(
      "ID columns contain NA values:", 
      paste(id_columns[na_in_ids], collapse = ", ")
    ))
  }
  
  # Validation: Check for duplicated values in this entity's id column(s)
  # check for duplicate cols will be done later
  my_id_columns <- variables %>% 
    filter(data_type == "id") %>% 
    filter(entity_level == 0) %>% 
    pull(variable)
  
  dupes_in_ids <- data %>%
    select(all_of(my_id_columns)) %>%
    summarise(across(everything(), anyDuplicated)) %>%
    unlist() %>% as.logical()
  
  if (any(dupes_in_ids)) {
    add_feedback(paste(
      "ID columns contain duplicates:", 
      paste(my_id_columns[dupes_in_ids], collapse = ", ")
    ))
  }
  
  # Validation: Check entity@name is defined and non-empty
  if (is.na(entity@name) || entity@name == '') {
    add_feedback(paste0(
      c(
        "Entity is missing required 'name' metadata",
        "[You can fix this with `set_entity_name('...')` or `entity_from_file(file, name='...')]"
      ),
      collapse="\n"
    ))
  }
  
  # Validation: there should only be one ID column per entity_level
  id_col_contraventions <- variables %>%
    filter(data_type == "id") %>%
    group_by(entity_level) %>%
    filter(n() > 1) %>%
    summarise(id_columns = paste0(variable, collapse = ", "), .groups = "drop")
  
  if (nrow(id_col_contraventions)) {
    add_feedback(paste0(
      c(
        "There are multiple ID columns per entity level:",
        kable(id_col_contraventions),
        "~~~~",
        "Entity level 0 is this entity. Level -1 is the parent entity, -2 is the grandparent, etc.",
        "It is likely that one or more variable columns have been incorrectly detected as ID columns.",
        "To fix this, redo the column type detection as follows:",
        glue("{global_varname} <- {global_varname} %>% redetect_columns_as_variables(columns = c('variable1', 'variable2'))")
      ),
      collapse="\n"
    ))
  }
  
  my_id_variable <- variables %>%
    filter(data_type == 'id') %>%
    filter(entity_level == 0)
  
  # Validation: there must be an ID column for this entity
  if (nrow(my_id_variable) == 0) {
    add_feedback(to_lines(
      c(
        "This entity appears to have no ID column.",
        "It must have a column with a unique value in each row.",
        "You can create a simple numeric ID as follows:",
        indented(c(
          glue("{global_varname} <- {global_varname} %>%"),
          indented(c(
            "modify_data(mutate(ID = row_number())) %>%",
            "sync_variable_metadata() %>%",
            "redetect_column_as_id('ID')"
          ))
        )),
        glue("Then `validate({global_varname})` again")
      )
    ))
  }
  
  
  # Validation: If there's an entity@name, and only one ID column at entity_level == 0,
  # check the latter's entity_name is correct
  if (!is.na(entity@name) &&
      entity@name != "" &&
      all(id_col_contraventions$entity_level != 0) &&
      nrow(my_id_variable) > 0) {
    if (my_id_variable %>% pull(entity_name) %>% coalesce("") != entity@name) {
      add_feedback(glue(to_lines(
        c(
          "ID column '{my_id_variable %>% pull(variable)}' has incorrect `entity_name`.",
          "It is '{my_id_variable %>% pull(entity_name)}' and should be '{entity@name}'",
          "You can fix this with:",
          indented(
            "{global_varname} <- {global_varname} %>% set_variable_metadata('{my_id_variable %>% pull(variable)}', entity_name='{entity@name}')"
          )
        )
      )))
    }
  }
  
  # Validation: check that integer variables' data columns contain integers
  integer_columns <- variables %>%
    filter(data_type == "integer" & !is_multi_valued) %>% pull(variable)
  
  not_integers <- data %>%
    select(all_of(integer_columns)) %>%
    summarise(
      across(
        everything(),
        # we allow factors as long as they are all-integer
        ~ !(is.integer(.) | (is.factor(.) & all(as.integer(.) == ., na.rm = TRUE)))
      )
    ) %>%
    unlist() %>% as.logical()
  
  if (any(not_integers)) {
    for (col_name in integer_columns[not_integers]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as 'integer' but contains non-integer values."),
          "To fix this, either change the column's data type to 'number' to allow non-integers:",
          indented(glue("{global_varname} <- {global_varname} %>% set_variable_metadata('{col_name}', data_type = 'number')")),
          "Or, if appropriate, you can modify the column's data to only contain integers, for example:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.integer({col_name})))")),
          "However, be aware of data loss. E.g. `as.integer('12 cm')` is `NA`",
          "Fix any issues before using `as.integer()`"
        )
      ))
    }
  }
  
  # Validation: check that number variables' data columns contain numeric values
  number_columns <- variables %>%
    filter(data_type == "number" & !is_multi_valued) %>% pull(variable)
  
  not_numbers <- data %>%
    select(all_of(number_columns)) %>%
    summarise(across(everything(), ~ !is.numeric(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_numbers)) {
    for (col_name in number_columns[not_numbers]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as 'number' but contains non-numeric values."),
          "To fix this, you can modify the column's data to only contain numeric values, for example:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.numeric({col_name})))")),
          "However, be aware of data loss. E.g. `as.numeric('12 cm')` is `NA`",
          "Fix any issues before using `as.numeric()`"
        )
      ))
    }
  }
  
  # Validation: check that date variables' data columns are R date type
  date_columns <- variables %>%
    filter(data_type == "date" & !is_multi_valued) %>% pull(variable)

  not_dates <- data %>%
    select(all_of(date_columns)) %>%
    summarise(across(everything(), ~ !is.Date(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_dates)) {
    for (col_name in date_columns[not_dates]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as 'date' but R does not currently recognise it as a date."),
          "To fix this, you can modify the column's data by converting to dates, for example:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.Date({col_name})))")),
          "However, this may fail or create NAs if there are badly formatted, incorrect or ambiguous dates.",
          "You should ensure there is no unintentional data loss when manipulating dates."
        )
      ))
    }
  }

  # Validation: check that multi-valued variables' data columns are R character type
  multi_valued_columns <- variables %>%
    filter(is_multi_valued) %>% pull(variable)
  
  not_character_cols <- data %>%
    select(all_of(multi_valued_columns)) %>%
    summarise(across(everything(), ~ !is.character(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_character_cols)) {
    for (col_name in multi_valued_columns[not_character_cols]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as multi-valued but its R data type is not 'character'."),
          "To fix this, you can modify the column's data by converting to character, for example:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.character({col_name})))")),
          "You should ensure there is no unintentional data loss (e.g. precision) when manipulating data columns."
        )
      ))
    }
  }
  
    
  # Validation: check that categorical columns are factors
  factor_columns <- variables %>%
    filter(data_shape == "ordinal") %>% pull(variable)
  
  not_factors <- data %>%
    select(all_of(factor_columns)) %>%
    summarise(across(everything(), ~ !is.factor(.))) %>%
    unlist() %>% as.logical()

  if (any(not_factors)) {
    for (col_name in factor_columns[not_factors]) {
      add_feedback(to_lines(
        c(
          glue("The variable '{col_name}' has data_shape 'ordinal', and therefore"),
          glue("its data column must be an R factor (it is currently {data %>% pull(col_name) %>% class()})."),
          "The best way to define an ordinal variable is as follows:",
          indented(glue("{global_varname} <- {global_varname} %>% set_variable_ordinal_levels(variable_name, levels)")),
          "This will take care of the data column reformatting for you."
        )
      ))
    }
  }
  
  # Validation: check that data_shape == 'ordinal' columns have
  # non-empty ordinal_levels, and non-ordinals have empty ordinal_levels
  ordinal_issues <- variables %>%
    mutate(
      num_levels = map_int(ordinal_levels, length),
      issue = case_when(
        data_shape == 'ordinal' & !data_type %in% c("integer", "string") ~ glue("Variable '{variable}' data_shape 'ordinal' is not compatible with data_type '{data_type}'"), 
        data_shape == 'ordinal' & num_levels == 0 ~ glue("Ordinal variable '{variable}' has no ordinal_levels defined but requires them."),
        data_shape != 'ordinal' & num_levels > 0 ~ glue("Non-ordinal variable '{variable}' has {num_levels} ordinal_levels but should have none."),
        TRUE ~ "OK"
      )
    ) %>%
    filter(issue != 'OK') %>%
    pull(issue)
  
  if (length(ordinal_issues) > 0) {
    add_feedback(to_lines(
      ordinal_issues,
      "To create an ordinal variable without these issues, use:",
      indented(glue("{global_varname} <- {global_varname} %>% set_variable_ordinal_levels(variable_name, levels)"))
    ))
    
  }
  
  # Validation: check that parent_variable column in metadata
  # always points to a real variable or category when it is not NA
  
  variable_relationships <- variables %>% select(variable, parent_variable)
  
  bad_parents <- variable_relationships %>%
    filter(!is.na(parent_variable)) %>%
    left_join(
      variable_relationships,
      join_by(parent_variable == variable),
      keep = TRUE, # Retain both left and right versions of overlapping column names (e.g., variable.left, variable.right)
      suffix = c(".left", ".right")
    ) %>%
    filter(is.na(variable.right)) %>%
    pull(variable.left) 
  
  if (length(bad_parents) > 0) {
    add_feedback(to_lines(
      "These variables or categories have 'parent_variable' values that do not exist:",
      indented(paste0(bad_parents, collapse=", ")),
      "The values for 'parent_variable' should be variable names.",
      "To avoid issues like this, create categories with `create_variable_category()`"
    ))
  }
  
  # also check for circular paths in the variable tree
  
  graph <- igraph::graph_from_data_frame(
    variable_relationships %>% replace_na(list(parent_variable = '_root_'))
  )
  if (!igraph::is_acyclic(graph)) {
    add_feedback(
      "Illegal circular path detected in the parent_variable -> variable graph."
    )
  }

  # Validation: check that units are not given for non-numeric vars
  non_numeric_vars_with_units <- entity %>%
    get_variable_metadata() %>%
    filter(
      !data_type %in% c("integer", "number"),
      !is.na(unit)
    ) %>%
    pull(variable)
  
  if (length(non_numeric_vars_with_units) > 0) {
    add_feedback(to_lines(
      glue("These non-numeric variables should not have units: {paste0(non_numeric_vars_with_units, collapse=', ')}"),
      "You can remove them with:",
      # the next line is intended to be repeated for multiple vars
      indented(glue("{global_varname} <- {global_varname} %>% set_variable_metadata('{non_numeric_vars_with_units}', unit = NA)"))
    ))
  }
  
  # Validation: check that all collections have a corresponding variable category
  orphan_collections <- entity@collections %>%
    anti_join(get_category_metadata(entity), join_by(category == variable)) %>%
    pull(category)
    
  if (length(orphan_collections) > 0) {
    add_feedback(to_lines(
      glue("These variable collections have no corresponding variable category: {paste0(orphan_collections, collapse=', ')}"),
      "You should remove them and add new collections for valid variable categories, e.g. as follows:",
      # the next line is intended to be repeated for multiple vars
      indented(glue("{global_varname} <- {global_varname} %>% delete_variable_collection('{orphan_collections}')"))
    ))
  }

  # Output feedback to the user
  give_feedback()  
  
  # Return overall validation status
  is_valid <- get_is_valid()
  if (quiet) {
    return(is_valid) # don't be *too* quiet
  } else {
    return(invisible(is_valid))
  }
})
