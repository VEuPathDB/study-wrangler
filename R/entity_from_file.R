library(tidyverse)

# R/entity_from_file.R
entity_from_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read the data with minimal column name repair and no type detection
  data <- readr::read_tsv(file_path,
                          name_repair = 'minimal',
                          col_types = readr::cols(.default = "c")
                          )
  
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
  data <- suppressMessages(readr::type_convert(data, guess_integer = TRUE))

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
  
  # Return an Entity object
  entity(data = data, metadata = metadata)
}
