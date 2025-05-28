#' entity_from_file
#' 
#' @description
#' Reads a tabular data file (e.g., TSV) and converts it into an Entity object.
#' This function infers variables' metadata, including data types and shapes,
#' and allows for pre-processing of the raw data before type inference.
#'
#' @param file_path A string specifying the path to the input file.
#' @param preprocess_fn An optional function to modify the raw data
#'   before type inference. This can be used for tasks such as correcting
#'   invalid dates or other data cleanup. The function should accept a tibble
#'   and return a modified tibble. All columns in the input tibble will be
#'   character type (aka strings). Default is `NULL`.
#' @param ... Additional named parameters to set entity metadata (see Entity class),
#'   e.g. name="household", display_name="Household"   
#' @return An Entity object with two main components:
#' \itemize{
#'   \item `data`: A tibble containing the processed tabular data.
#'   \item `variables`: A tibble describing the columns, including:
#'     \itemize{
#'       \item `variable`: Unique, R-friendly column names.
#'       \item `provider_label`: Original column names from the input file.
#'       \item `data_type`: Inferred type of each column (e.g., `id`, `number`, `date`).
#'       \item `data_shape`: Shape of the data (e.g., `continuous` or `categorical`).
#'       \item and many many more (MOST IMPORTANT ONES TO BE DOCUMENTED)
#'     }
#' }
#'
#' @examples
#' # Load an entity from a file
#' households <- entity_from_file('households.tsv', name='household')
#'
#' # Inspect the entity
#' inspect(households)
#' # Dive deeper into a specific variable (using the R-friendly tibble column name)
#' inspect_variable(households, 'Number.of.animals')
#'
#' # Make fixes and validate
#' validate(households)
#'
#' # Create another entity and combine into a study
#' participants <- entity_from_file('participants.tsv')
#' study <- study_from_entities(households, participants)
#'
#' @export
entity_from_file <- function(file_path, preprocess_fn = NULL, ...) {
  if (!file.exists(file_path)) {
    stop("Error: file does not exist: ", file_path)
  }
  ext <- tolower(tools::file_ext(file_path))
  if (ext == "csv") {
    return(entity_from_csv(file_path, preprocess_fn, ...))
  } else if (ext == "tsv" || ext == "txt") {
    return(entity_from_tsv(file_path, preprocess_fn, ...))
  } else {
    # Try to autodetect by reading first line
    first_line <- readLines(file_path, n = 1)
    if (grepl(",", first_line) && !grepl("\t", first_line)) {
      return(entity_from_csv(file_path, preprocess_fn, ...))
    } else {
      return(entity_from_tsv(file_path, preprocess_fn, ...))
    }
  }
}

#' entity_from_tibble
#'
#' @description
#' Creates an Entity object from a raw character-only tibble. Optionally applies a preprocessing function.
#' @param data A tibble with all columns as character.
#' @param preprocess_fn Optional function to preprocess the tibble before type inference.
#' @param ... Additional named parameters to set entity metadata (see Entity class).
#' @return An Entity object.
#' @export
entity_from_tibble <- function(data, preprocess_fn = NULL, ...) {
  metadata = list(...)
  validate_object_metadata_names('Entity', metadata)

  # Apply the pre-processing function, if provided.
  if (!is.null(preprocess_fn)) {
    data <- preprocess_fn(data)
  }

  provider_labels <- colnames(data) %>% map(list)
  clean_names <- make.names(colnames(data), unique = TRUE)
  colnames(data) <- clean_names

  if (anyDuplicated(provider_labels)) {
    duplicates <- provider_labels[duplicated(provider_labels) | duplicated(provider_labels, fromLast = TRUE)]
    renamed <- tibble(
      original = provider_labels,
      renamed = clean_names
    ) %>% 
      filter(original %in% duplicates)
    warning(
      paste(
        "Duplicate column names detected in input. Renamed as follows:\n",
        paste(renamed$original, "->", renamed$renamed, collapse = "\n"),
        sep=""
      )
    )
  }

  data <- type_convert_quietly(data)
  variables <- tibble(variable=clean_names) %>% expand_grid(variable_metadata_defaults)
  variables <- variables %>% mutate(provider_label = provider_labels)
  if (!is.null(metadata$name)) {
    variables <- variables %>% mutate(entity_name = metadata$name)
  }
  constructor_args <- c(list(data = data, variables = variables), metadata)
  entity <- do.call(entity, constructor_args)
  entity <- entity %>% infer_missing_data_types() %>% infer_missing_data_shapes()
  return(entity)
}

#' entity_from_tsv
#' @description Convenience function to create an Entity from a TSV file.
#' @export
entity_from_tsv <- function(file_path, preprocess_fn = NULL, ...) {
  data <- suppressWarnings(
    readr::read_tsv(
      file_path,
      name_repair = 'minimal',
      col_types = readr::cols(.default = "c"),
      progress = FALSE
    )
  )
  problems <- readr::problems(data)
  if (nrow(problems) > 0) {
    stop(paste0(
      c(
        "Error: Issues were encountered while parsing the file:",
        knitr::kable(problems)
      ),
      collapse="\n"
    ))
  }
  entity_from_tibble(data, preprocess_fn, ...)
}

#' entity_from_csv
#' @description Convenience function to create an Entity from a CSV file.
#' @export
entity_from_csv <- function(file_path, preprocess_fn = NULL, ...) {
  data <- suppressWarnings(
    readr::read_csv(
      file_path,
      name_repair = 'minimal',
      col_types = readr::cols(.default = "c"),
      progress = FALSE
    )
  )
  problems <- readr::problems(data)
  if (nrow(problems) > 0) {
    stop(paste0(
      c(
        "Error: Issues were encountered while parsing the file:",
        knitr::kable(problems)
      ),
      collapse="\n"
    ))
  }
  entity_from_tibble(data, preprocess_fn, ...)
}
