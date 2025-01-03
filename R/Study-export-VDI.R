#' @export
setGeneric("export_to_vdi", function(study, output_directory) standardGeneric("export_to_vdi"))


#' export_to_vdi
#'
#' Export a `Study` object to a specified output directory, creating the directory if it doesn't exist.
#'
#' @param study A `Study` object.
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (which will be created if it doesn't exist).
#' @return The `Study` object (invisibly).
#' @export
setMethod("export_to_vdi", "Study", function(study, output_directory) {
  if (missing(output_directory)) {
    stop("Required argument output_directory not provided.")
  }
  
  # Check if the directory exists, and create it if necessary
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)  # Use recursive = TRUE for nested directories
  }
  
  # do the actual export here, including recursing into the entity tree, writing 
  # tab-delimited data files into the output_directory and finally writing
  # `install.json` which describes the table structures and indexes.
  
  install_json <- list()

  # study table schema
  install_json <- append(install_json, vdi_study_table_def) 
  # study table data
  study_cache <- tibble(
    user_dataset_id = "@USER_DATASET_ID@",
    stable_id = study %>% get_study_id(),
    internal_abbrev = study %>% get_study_abbreviation(),
    modification_date = "@MODIFICATION_DATE@"
  )
  write_tsv(study_cache, file = file.path(output_directory, "study.cache"), col_names = FALSE)

  # now we need to recursively populate the `entitytypegraph` table
  # first add the schema
  install_json <- append(install_json, vdi_entitytypegraph_table_def)
  # now do the dirty work
  root_entity <- study %>% get_root_entity()
  
  # install_json <- export_to_vdi(list(root_entity), output_directory, install_json)
    
  # Convert to JSON and pretty-print
  json_content <- jsonlite::toJSON(install_json, pretty = TRUE, auto_unbox = TRUE)
  
  # Write to file
  json_file <- file.path(output_directory, "install.json")
  write(json_content, file = json_file)
    
  # Return the study object invisibly
  return(invisible(study))
})
