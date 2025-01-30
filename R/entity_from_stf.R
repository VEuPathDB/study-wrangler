entity_from_stf <- function(file_path) {
  # Read in the file as all-character with no headers
  data <- read_tsv(file_path, col_names = FALSE, col_types = cols(.default = "c"))
  
  # Determine if it's wide or tall format based on the first row
  is_wide <- any(str_detect(data[1, ], ".+\\\\\\\\ Descriptors"))
  
  # If tall, transpose to wide format
  if (!is_wide) {
    data <- data %>% t() %>% as_tibble(.name_repair = "unique")
  }
  
  # Extract the header row in place
  headers <- data %>% slice_head(n = 1) %>% unlist(use.names = FALSE)
  data <- data %>% slice_tail(n = nrow(data) - 1)  # Drop header row in place
  
  # Assign correct headers
  colnames(data) <- headers
  
  # TO DO: Handle variables (metadata about ID columns and variable columns)
  variables <- NULL  # Placeholder
  
  # TO DO: Handle metadata if needed
  metadata <- list()  # Placeholder
  
  # Construct entity
  constructor_args <- c(list(data = data, variables = variables), metadata)
  entity <- do.call(entity, constructor_args)
  
  return(entity)
}
