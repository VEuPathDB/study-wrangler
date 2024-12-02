library(tidyverse)

# R/entity_from_file.R
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
  
  # Return an Entity object
  entity(data = data, metadata = metadata)
}
