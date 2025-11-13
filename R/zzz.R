#' skimr shortens all factor names to three characters by default.
#' Create a custom skimmer that doesn't do this and trims top counts to 5.
#' @export
skim <- NULL  # Initialized in .onLoad

.onLoad <- function(libname, pkgname) {
  # Initialize validators when the package loads
  .init_baseline_validators()
  .init_eda_validators()

  # Create custom skimmer that doesn't shorten factor names and trims top counts to 5
  skim <<- skim_with(
    factor = sfl(
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
}

.onAttach <- function(libname, pkgname) {
  if (is.null(getOption("knitr.table.format"))) {
    options(knitr.table.format = "simple")
    packageStartupMessage("Setting knitr.table.format to 'simple'.")
  }
}
