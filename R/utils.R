library(glue)

# skimr shortens all factor names to three characters by default.
# Create a custom skimmer that doesn't do this and trims top counts to 5.
skim <- skimr::skim_with(
  factor = skimr::sfl(
    top_counts = function(x) {
      tab <- table(x)
      top <- sort(tab, decreasing = TRUE)
      # Select the top 5 levels
      top5 <- head(top, 5)
      # Summarize the rest
      if (length(top) > 5) {
        others <- sum(top[-(1:5)])
        top5 <- c(top5, `<Others>` = others)
      }
      # Format output
      paste0(names(top5), ": ", top5, collapse = ", ")
    }
  )
)

# Convert R column types to `data_type` metadata annotation
infer_data_type <- function(data, column_name, .no_id_check = FALSE) {
  column <- data %>% pull(column_name)
  
  if (inherits(column, "Date") || inherits(column, "POSIXct")) {
    return("date")
  } 
  
  if (!.no_id_check && n_distinct(column) == length(column)) {
    return("id") # Guess ID type only works for primary keys  
  } 
  
  if (is.integer(column)) { 
    return("integer")
  } 
  
  if (is.numeric(column)) {
    return("number")
  } 
  
  return("string")
}

#'
#' void context function that will stop() if a metadata name isn't an Entity slot
#' 
#' usage:
#' 
#' myfunction <- function(arg1, arg2, ...) {
#'   metadata = list(...)
#'   validate_entity_metadata_names(metadata) # will bail if there's a problem
#'   # continue doing things with metadata
#' }
#'
validate_entity_metadata_names <- function(metadata) {
  disallowed_keys <- c("data", "variables")
  valid_keys <- setdiff(slotNames("Entity"), disallowed_keys)
  invalid_keys <- setdiff(names(metadata), valid_keys)
  if (length(invalid_keys) > 0) {
    stop("Error: these entity_from_file() args are not valid Entity metadata names: ", toString(invalid_keys))
  }
}


#'
#' set `display_name` and `display_name_plural` using name
#'
apply_entity_metadata_defaults <- function(metadata, verbose = FALSE) {
  if (!is.na(metadata$name) && is.na(metadata$display_name)) {
    metadata$display_name <- metadata$name
    if (verbose) message(glue("Note: added default display_name, '{metadata$display_name}', for entity"))
  }
  if (!is.na(metadata$display_name) && is.na(metadata$display_name_plural)) {
    metadata$display_name_plural <- paste0(metadata$display_name, "s")
    if (verbose) message(glue("Note: added default display_name_plural, '{metadata$display_name_plural}', for entity"))
  }
  metadata
}

#'
#' provide an as_list method for all S4 objects - not sure why this isn't
#' in base R!
#'
# Define a generic function for as_list
setGeneric("as_list", function(object) standardGeneric("as_list"))

# Define the method for S4 objects
setMethod("as_list", "ANY", function(object) {
  if (!isS4(object)) {
    stop("Error: the input is not an S4 object.")
  }
  slots <- slotNames(object)
  setNames(lapply(slots, function(slot_name) slot(object, slot_name)), slots)
})

#'
#' validate_entity_name
#' 
#' returns TRUE/FALSE
#'
validate_entity_name <- function(name) {
  is.character(name) && 
    length(name) > 0 && 
    grepl("^[a-zA-Z0-9]+$", name)
}

