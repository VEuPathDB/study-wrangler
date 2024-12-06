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
infer_data_type <- function(data, column_name) {
  column <- data %>% pull(column_name)
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
