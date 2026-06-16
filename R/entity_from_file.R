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
#'   character type (aka strings). Default is `NULL`. Note: if the input file
#'   contains duplicate column names, they will be deduplicated before
#'   `preprocess_fn` is called (e.g. two `notes` columns become `notes` and
#'   `notes.1`), so any column references inside the function must use the
#'   deduplicated names.
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
#' @param data A tibble with all columns as character (unless skip_type_convert is TRUE)
#' @param preprocess_fn Optional function to preprocess the tibble before type inference.
#'   If the input has duplicate column names, they are deduplicated (e.g. `notes`, `notes.1`)
#'   before this function is called, so column references must use the deduplicated names.
#' @param skip_type_convert Optional boolean to skip R column type detection and use existing types. Default is FALSE. 
#' @param ... Additional named parameters to set entity metadata (see Entity class).
#' @return An Entity object.
#' @export
entity_from_tibble <- function(data, preprocess_fn = NULL, skip_type_convert = FALSE, ...) {
  metadata = list(...)
  validate_object_metadata_names('Entity', metadata)

  # Silently remove ghost columns first so they don't trigger a spurious
  # duplicate-name warning below (e.g. multiple trailing empty-header columns).
  ghost_col_indices <- which(
    trimws(colnames(data)) == "" &
    sapply(data, function(col) all(is.na(col)))
  )
  if (length(ghost_col_indices) > 0) {
    data <- data[, -ghost_col_indices, drop = FALSE]
  }

  # Warn about and rename duplicate column names before preprocess_fn runs, so
  # that callers can catch the warning and preprocess_fn receives unique names.
  # Capture original names first so provider_label can reflect the input file.
  original_names <- colnames(data)
  deduped_names <- make.names(original_names, unique = TRUE)
  if (anyDuplicated(original_names)) {
    renamed <- tibble(original = original_names, renamed = deduped_names) %>%
      filter(original != renamed)
    warning(
      paste(
        "Duplicate column names detected in input. Renamed as follows:\n",
        paste(renamed$original, "->", renamed$renamed, collapse = "\n"),
        sep = ""
      )
    )
    colnames(data) <- deduped_names
  }

  # Apply the pre-processing function, if provided.
  if (!is.null(preprocess_fn)) {
    data <- preprocess_fn(data)
  }

  # Drop named columns where every value is NA or blank, and inform the user
  empty_data_indices <- which(
    sapply(data, function(col) all(is.na(col) | trimws(as.character(col)) == ""))
  )
  if (length(empty_data_indices) > 0) {
    empty_col_names <- colnames(data)[empty_data_indices]
    if (!isTRUE(metadata$quiet)) {
      message(
        "Dropped ", length(empty_data_indices),
        " empty column(s) (all values missing or blank): ",
        paste(empty_col_names, collapse = ", ")
      )
    }
    data <- data[, -empty_data_indices, drop = FALSE]
  }

  # Build a map from deduplicated name → original name so provider_label
  # reflects what the user's input file actually contained.
  name_map <- setNames(original_names, deduped_names)
  provider_labels <- colnames(data) %>% map(function(col) {
    list(if (col %in% names(name_map)) name_map[[col]] else col)
  })

  clean_names <- make.names(colnames(data), unique = TRUE)
  colnames(data) <- clean_names

  if (!skip_type_convert) {
    data <- type_convert_quietly(data)
  }
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

# Detect file encoding. UTF-16LE/BE, UTF-8, Windows-1252, and ISO-8859-1 cover
# the vast majority of tabular data files users upload in the wild (UTF-8 from
# modern tools; Windows-1252 and ISO-8859-1 from legacy Excel/Access exports in
# Western locales; UTF-16 from some Windows applications). We roll our own
# rather than using readr::guess_encoding() because it uses inconsistent casing
# ("windows-1252"), gives ~0.4 confidence for ISO-8859-1/Windows-1252, and its
# sampling behaviour is unspecified across readr versions — too unreliable for a
# deterministic pick. Instead: check for UTF-16 BOM or alternating-NUL pattern
# on a small raw sample first, then probe UTF-8 via readLines warning, then use
# a binary byte-range scan to distinguish the two single-byte encodings (bytes
# 0x80-0x9F are printable only in Windows-1252).
detect_file_encoding <- function(path) {
  # ~100 lines worth of bytes is more than enough to detect UTF-16 patterns
  sample_bytes <- readBin(path, what = "raw", n = 4000L)

  if (length(sample_bytes) >= 2) {
    if (sample_bytes[1] == as.raw(0xFF) && sample_bytes[2] == as.raw(0xFE))
      return("UTF-16LE")
    if (sample_bytes[1] == as.raw(0xFE) && sample_bytes[2] == as.raw(0xFF))
      return("UTF-16BE")
  }

  # No BOM: look for alternating NUL bytes that indicate UTF-16 encoded ASCII.
  # In UTF-16LE the high byte (positions 2,4,6,...) is 0x00 for BMP ASCII chars;
  # in UTF-16BE the high byte is at positions 1,3,5,...
  if (length(sample_bytes) >= 4) {
    n <- length(sample_bytes)
    pos1_nuls <- sum(sample_bytes[seq(1L, n, 2L)] == as.raw(0x00))
    pos2_nuls <- sum(sample_bytes[seq(2L, n, 2L)] == as.raw(0x00))
    threshold <- 0.4 * (n %/% 2L)
    if (pos2_nuls >= threshold && pos1_nuls < threshold) return("UTF-16LE")
    if (pos1_nuls >= threshold && pos2_nuls < threshold) return("UTF-16BE")
  }

  had_invalid_input <- FALSE
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  withCallingHandlers(
    readLines(con, warn = TRUE),
    warning = function(w) {
      if (grepl("invalid input", conditionMessage(w))) had_invalid_input <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  if (!had_invalid_input) return("UTF-8")

  has_windows_range <- local({
    raw_bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    any(raw_bytes >= as.raw(0x80) & raw_bytes <= as.raw(0x9F))
  })
  if (has_windows_range) "Windows-1252" else "ISO-8859-1"
}

#' entity_from_tsv
#' @description Convenience function to create an Entity from a TSV file.
#' @export
entity_from_tsv <- function(file_path, preprocess_fn = NULL, ...) {
  enc <- detect_file_encoding(file_path)
  data <- suppressWarnings(
    readr::read_tsv(
      file_path,
      name_repair = 'minimal',
      col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = enc),
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
  enc <- detect_file_encoding(file_path)
  data <- suppressWarnings(
    readr::read_csv(
      file_path,
      name_repair = 'minimal',
      col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = enc),
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
