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
#' @importFrom dplyr tibble_to_sparse_object

export_entity_to_stf <- function(entity, output_directory) {
  entity_name <- entity %>% get_entity_name()

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

  entity_metadata <- entity %>%
    slotNames() %>%
    keep(~ is.character(slot(entity, .x))) %>%
    set_names() %>%
    map(~ slot(entity, .x)) %>%
    discard(is.na) %>%
    list_assign(
      id_columns = ids_metadata,
      variables = variables_metadata,
      categories = categories_metadata
    )

  write_pretty_yaml(entity_metadata, entity_metadata.path)

  # write the data to STF-style TSV
  write_stf_data(entity, output_directory)

  invisible(entity)
}
