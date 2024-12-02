library(tibble)

# R/Entity-class.R
setClass(
  "Entity",
  slots = list(
    data = "tbl_df", # The tibble containing the entity data
    metadata = "tbl_df", # Metadata for variables
    name = "character", # Internal entity name
    parent_name = "character", # Parent entity's name
    id_column = "character", # Column for entity IDs
    parent_id_column = "character" # Column for parent entity IDs
  )
)

# Constructor function
entity <- function(data, metadata = tibble(), name = "", parent_name = "", id_column = "", parent_id_column = "") {
  new("Entity", 
      data = data,
      metadata = metadata,
      name = name,
      parent_name = parent_name,
      id_column = id_column,
      parent_id_column = parent_id_column)
}
