library(tidyverse)

#' Create an Entity Object from a Tabular Data File
#'
#' @description
#' Reads a tabular data file (e.g., TSV) and converts it into an Entity object.
#' This function infers column metadata, including data types and shapes,
#' and allows for pre-processing of the raw data before type inference.
#'
#' @param file_path A string specifying the path to the input file.
#' @param preprocess_fn An optional function to modify the raw data
#'   before type inference. This can be used for tasks such as correcting
#'   invalid dates or other data cleanup. The function should accept a tibble
#'   and return a modified tibble. All columns in the input tibble will be
#'   character type (aka strings). Default is `NULL`.
#'
#' @return An Entity object with two main components:
#' \itemize{
#'   \item `data`: A tibble containing the processed tabular data.
#'   \item `metadata`: A tibble describing the columns, including:
#'     \itemize{
#'       \item `variable`: Unique, R-friendly column names.
#'       \item `provider_label`: Original column names from the input file.
#'       \item `data_type`: Inferred type of each column (e.g., `id`, `number`, `date`).
#'       \item `data_shape`: Shape of the data (e.g., `continuous` or `categorical`).
#'     }
#' }
#'
#' @examples
#' # Load an entity from a file
#' households <- entity_from_file('households.tsv')
#'
#' # Inspect the entity
#' glimpse(households)
#'
#' # Make fixes and validate
#' validate(households)
#'
#' # Create another entity and combine into a study
#' participants <- entity_from_file('participants.tsv')
#' study <- study_from_entities(households, participants)
#'
#' @export
entity_from_file <- function(file_path, preprocess_fn = NULL) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read the data with minimal column name repair and no type detection
  data <- readr::read_tsv(file_path,
                          name_repair = 'minimal',
                          col_types = readr::cols(.default = "c")
                          )

  # Apply the pre-processing function, if provided.
  # Mainly useful for fixing dates before they are auto-detected and validated.
  # Other fixes can be made on the return value of this function.
  if (!is.null(preprocess_fn)) {
    data <- preprocess_fn(data)
  }

  # Original column names
  provider_labels <- colnames(data)
  
  # Generate unique, R-friendly names
  clean_names <- make.names(provider_labels, unique = TRUE)
  colnames(data) <- clean_names
  
  # Warn if duplicates exist in provider labels
  if (anyDuplicated(provider_labels)) {
    duplicates <- provider_labels[duplicated(provider_labels) | duplicated(provider_labels, fromLast = TRUE)]
    renamed <- tibble(
      original = provider_labels,
      renamed = clean_names
    ) %>% 
      filter(original %in% duplicates)
    
    warning(
      paste(
        "Duplicate column names detected in input file. Renamed as follows:\n",
        paste(renamed$original, "->", renamed$renamed, collapse = "\n"),
        sep=""
      )
    )
  }
  
  # Detect column types (initially all read in as `chr`)
  # Suppress messages but intercept warnings about dates
  data <- withCallingHandlers(
    suppressMessages(readr::type_convert(data, guess_integer = TRUE)),
    warning = function(w) {
      # Check if it's a type_convert warning and embellish it
      if (grepl("expected valid date", conditionMessage(w))) {
        warning(paste(
          conditionMessage(w),
          "This date was converted to NA.\nPlease consider using `entity_from_file(filename, preprocess_fn=function)` to clean up dates.",
          sep="\n"
        ), call. = FALSE)
        invokeRestart("muffleWarning")
      } else {
        # If not a type_convert warning, pass it through
      }
    }
  )

  # Convert R column types to EDA annotations
  detect_column_type <- function(column) {
    if (inherits(column, "Date") || inherits(column, "POSIXct")) {
      return("date")
    } else if (n_distinct(column) == length(column)) {
      return("id") # guess ID type only works for primary keys  
    } else if (is.integer(column)) { 
      return("integer")
    } else if (is.numeric(column)) {
      return("number")
    } else {
      return("string")
    }
  }
  
  metadata <- tibble(
    variable = clean_names,
    provider_label = provider_labels,
    data_type = unname(map_chr(data, detect_column_type))
  )
  
  # infer data_shape as continuous or categorical
  # (further refinement to 'ordinal' will require user-input)
  metadata <- metadata %>% mutate(
    data_shape = case_when(
      data_type %in% c('number', 'integer', 'date') ~ 'continuous',
      .default = 'categorical'
    )
  )
  
  # Mutate categorical columns into factors
  categorical_vars <- metadata$variable[metadata$data_shape == "categorical"]
  data <- data %>%
    mutate(across(all_of(categorical_vars), as.factor))
  
  # Return an Entity object
  entity(data = data, metadata = metadata)
}
