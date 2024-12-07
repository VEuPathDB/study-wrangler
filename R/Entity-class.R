library(tibble)

# R/Entity-class.R
setClass(
  "Entity",
  slots = list(
    data = "tbl_df", # The tibble containing the entity data
    variables = "tbl_df", # Metadata for variables (and ID columns) in the data tibble

    # entity metadata
    name = "character", # Internal entity name, e.g. "household"
    description = "character", # Description of the entity
    display_name = "character", # External display name, e.g. "Household" 
    display_name_plural = "character", # External plural, e.g. "Households"
    
    # if this is not the root entity:
    parent_name = "character" # Parent entity's internal name, e.g. "community"
  )
)

# Constructor function
entity <- function(data,
                   variables = tibble(),
                   name = "",
                   description = "",
                   display_name = "",
                   display_name_plural = "",
                   parent_name = ""
                   ) {
  new("Entity", 
      data = data,
      variables = variables,
      name = name,
      description = description,
      display_name = display_name,
      display_name_plural = display_name_plural,
      parent_name = parent_name
  )
}
