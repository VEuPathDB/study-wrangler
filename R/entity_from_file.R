library(tidyverse)

# R/entity_from_file.R
entity_from_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  data <- readr::read_tsv(file_path, col_types = readr::cols(.default = "c"))
  metadata <- tibble(variable = colnames(data), provider_label = colnames(data))
  entity(data = data, metadata = metadata)
}
