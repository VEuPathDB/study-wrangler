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
  
  tools <- create_feedback_tools(quiet = quiet)
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
  missing_variables <- setdiff(colnames(data), variables$variable)
  extra_variables <- setdiff(variables$variable, colnames(data))
  
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
    filter(data_type != 'id') %>%
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
  issues <- names(variable_metadata_defaults) %>%
    map(function(col_name) {
      col_values <- variables[[col_name]]
      default_val <- variable_metadata_defaults[[col_name]]
      
      if (is.integer(default_val)) {
        # Integer validation
        invalid_values <- col_values[!is.na(col_values) & !is.integer(col_values)]
        if (length(invalid_values) > 0) {
          tibble(Column = col_name, Issue = "Not all values are integers", Values = paste(unique(invalid_values), collapse = ", "))
        } else {
          NULL
        }
      } else if (is.character(default_val)) {
        # Character validation
        invalid_values <- col_values[!is.na(col_values) & !is.character(col_values)]
        if (length(invalid_values) > 0) {
          tibble(Column = col_name, Issue = "Not all values are character/string type", Values = paste(unique(invalid_values), collapse = ", "))
        } else {
          NULL
        }
      } else if (is.logical(default_val)) {
        # Logical validation
        invalid_values <- col_values[!is.na(col_values) & !(col_values %in% c(TRUE, FALSE))]
        if (length(invalid_values) > 0) {
          tibble(Column = col_name, Issue = "Not all values are logical (TRUE/FALSE)", Values = paste(unique(invalid_values), collapse = ", "))
        } else {
          NULL
        }
      } else if (is.factor(default_val)) {
        # Factor validation
        default_levels <- levels(default_val)
        invalid_values <- col_values[!is.na(col_values) & !(col_values %in% default_levels)]
        if (length(invalid_values) > 0) {
          tibble(Column = col_name, Issue = "Contains values outside factor levels", Values = paste(unique(invalid_values), collapse = ", "))
        } else {
          NULL
        }
      } else {
        NULL
      }
    }) %>%
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
        glue("{global_varname} <- {global_varname} %>% redetect_columns_as_variable(columns = c('variable1', 'variable2'))")
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
      add_feedback(glue(paste0(
        c(
          "ID column '{my_id_variable %>% pull(variable)}' has incorrect `entity_name`.",
          "It is '{my_id_variable %>% pull(entity_name)}' and should be '{entity@name}'",
          "[You can fix this with `set_variable_metadata('{my_id_variable %>% pull(variable)}', entity_name='{entity@name}')`]"
        ),
        collapse="\n"
      )))
    }
  }
  
  # Validation: check that integer columns contain integers
  integer_columns <- variables %>%
    filter(data_type == "integer") %>% pull(variable)
  
  not_integers <- data %>%
    select(all_of(integer_columns)) %>%
    summarise(across(everything(), ~ !is.integer(.))) %>%
    unlist() %>% as.logical()
  
  if (any(not_integers)) {
    for (col_name in integer_columns[not_integers]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as 'integer' but contains non-integer values."),
          "To fix this, either change the column's data type to 'number' to allow non-integers:",
          indented(glue("{global_varname} <- {global_varname} %>% set_variable_metadata('{col_name}', data_type = 'number')")),
          "Or, if appropriate, you can modify the column's data to only contain integers, for example:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.integer({col_name})))"))
        )
      ))
    }
  }
  
  # Validation: check that number columns contain numeric values
  number_columns <- variables %>%
    filter(data_type == "number") %>% pull(variable)
  
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
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = as.numeric({col_name})))"))
        )
      ))
    }
  }
  
  # Validation: check that number columns contain numeric values
  date_columns <- variables %>%
    filter(data_type == "date") %>% pull(variable)

  stop("TO DO date validation")  
  
  # Validation: check that categorical columns are factors
  factor_columns <- variables %>%
    filter(data_shape != "continuous") %>% pull(variable)
  
  not_factors <- data %>%
    select(all_of(factor_columns)) %>%
    summarise(across(everything(), ~ !is.factor(.))) %>%
    unlist() %>% as.logical()

  if (any(not_factors)) {
    for (col_name in factor_columns[not_factors]) {
      add_feedback(to_lines(
        c(
          glue("The column '{col_name}' is declared as 'factor' but is not an R factor."),
          "To fix this, either mutate the data column back into a factor:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = factor({col_name})))")),
          "Or you can manipulate factors using the `forcats` library:",
          indented(glue("{global_varname} <- {global_varname} %>% modify_data(mutate({col_name} = fct_recode({col_name}, 'newVal' = 'oldVal')))"))
        )
      ))
    }
  }
  
  # Output feedback to the user
  give_feedback()  
  
  # Return overall validation status
  return(invisible(get_is_valid()))
})
