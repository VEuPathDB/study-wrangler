#' export_entity_to_stf
#'
#' Export an Entity as STF files to a specified output directory.
#'
#' @param entity An Entity object.
#' @param output_directory Output directory for STF files.
#' @return The Entity object (invisibly).
#' @export
#' @importFrom glue glue
#' @importFrom purrr map imap discard compact keep set_names list_assign
#' @importFrom dplyr relocate
#' @importFrom tibble as_tibble
#' @importFrom readr write_tsv
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr %>%

export_entity_to_stf <- function(entity, output_directory) {
  entity_name <- entity %>% get_entity_name()

  # Check if the directory exists, and create it if necessary
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)  # Use recursive = TRUE for nested directories
  }
  
  # write the metadata to YAML
  entity_metadata.path <- file.path(output_directory, glue("entity-{entity_name}.yaml"))

  # turn metadata tibble into a sparse R object (nested list) targeted for YAML
  # exclude any default values to keep the YAML concise
  postprocess_metadata <- function(metadata) {
    defaults <- as.list(variable_metadata_defaults)
    metadata %>%
      tibble_to_sparse_object() %>%
      map(function(record) {
        record %>%
          imap(~ if (!identical(.x, defaults[[.y]])) .x) %>%
          compact()
      })
  }

  ids_metadata <- entity %>% get_id_column_metadata() %>% rename(id_column = variable) %>% postprocess_metadata()
  variables_metadata <- entity %>% get_variable_metadata() %>% postprocess_metadata()
  categories_metadata <- entity %>% get_category_metadata() %>% rename(category = variable) %>% postprocess_metadata()
  collections_metadata <- entity@collections %>% postprocess_metadata()
  
  entity_metadata <- entity %>%
    slotNames() %>%
    keep(~ is.character(slot(entity, .x))) %>%
    set_names() %>%
    map(~ slot(entity, .x)) %>%
    discard(is.na) %>%
    list_assign(
      id_columns = ids_metadata,
      variables = variables_metadata,
      categories = categories_metadata,
      collections = collections_metadata
    )

  write_pretty_yaml(entity_metadata, entity_metadata.path)

  # write the data to STF-style TSV
  write_stf_data(entity, output_directory)

  invisible(entity)
}



write_stf_data <- function(entity, output_directory) {
  entity_name <- entity %>% get_entity_name()
  data.path <- file.path(output_directory, glue("entity-{entity_name}.tsv"))
  
  # Make sure the first columns are ID columns in entity_level order
  ids_metadata <- entity %>% get_id_column_metadata()  # Already in entity_level order
  id_column_names <- ids_metadata %>% pull(variable)
  data <- entity %>% get_data() %>% relocate(all_of(id_column_names))
  
  # Determine if we need tall format
  headers <- names(data)
  ncols <- length(headers)
  is_tall <- ncols > nrow(data) && ncols >= 200
  
  # Replace the last ID column headers with the special
  # STF ID column header which tells us which orientation the file is in
  # and tells us when the entity names stop and variable names begin
  this_entity_column_name <- id_column_names[length(id_column_names)] 
  headers[length(id_column_names)] <- if (is_tall) {
    glue("Descriptors \\\\ {this_entity_column_name}")
  } else {
    glue("{this_entity_column_name} \\\\ Descriptors")
  }
  
  # Convert everything to character and add headers as row 1
  data <- bind_rows(
    set_names(headers, names(data)),  # Apply corrected headers
    data %>% mutate(across(everything(), as.character))
  )
  
  # Transpose if needed
  if (is_tall) {
    data <- data %>% t() %>% as_tibble()
  }
  
  data %>% write_tsv(data.path, col_names = FALSE, progress = FALSE)
}

