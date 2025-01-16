.onAttach <- function(libname, pkgname) {
  if (is.null(getOption("knitr.table.format"))) {
    options(knitr.table.format = "simple")
    packageStartupMessage("Setting knitr.table.format to 'simple'.")
  }
}
