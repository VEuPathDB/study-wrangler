.onLoad <- function(libname, pkgname) {
  # Initialize validators when the package loads
  .init_baseline_validators()
  .init_eda_validators()
}

.onAttach <- function(libname, pkgname) {
  if (is.null(getOption("knitr.table.format"))) {
    options(knitr.table.format = "simple")
    packageStartupMessage("Setting knitr.table.format to 'simple'.")
  }
}
