#'
#' for Study and EntityPath objects
#'
setGeneric("export_to_vdi", function(object, output_directory) standardGeneric("export_to_vdi"))


#' export_to_vdi
#'
#' Export a `Study` object to a specified output directory, creating the directory if it doesn't exist.
#'
#' @param object A `Study` object.
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (which will be created if it doesn't exist).
#' @return The `Study` object (invisibly).
#' @export
setMethod("export_to_vdi", "Study", function(object, output_directory) {
  study <- object
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
  install_json <- append(install_json, list(vdi_study_table_def)) 
  # study table data
  study_cache <- tibble(
    user_dataset_id = "@USER_DATASET_ID@",
    stable_id = study %>% get_study_id(),
    internal_abbrev = study %>% get_study_abbreviation(),
    modification_date = "@MODIFICATION_DATE@"
  )
  write_tsv(study_cache, file = file.path(output_directory, "study.cache"), col_names = FALSE)

  # now we need to recursively populate the `entitytypegraph` table
  # first add the schema (schema defs and templates can be found in VDI-schema.R)
  install_json <- append(install_json, list(vdi_entitytypegraph_table_def))
  # and initialise the a list (of tibbles we will merge with bind_rows() later)
  entitytypegraph_cache <- list()

  # now do the dirty work
  root_entity <- study %>% get_root_entity()
  
  export_data <- export_entity_to_vdi_recursively(
    EntityPath(list(root_entity)),
    output_directory,
    install_json,
    entitytypegraph_cache,
    study
  )
  install_json <- export_data$install_json
  entitytypegraph_cache <- export_data$entitytypegraph_cache %>% bind_rows()

  write_tsv(entitytypegraph_cache, file = file.path(output_directory, "entitytypegraph.cache"), col_names = FALSE)
  
  # Convert to JSON and pretty-print
  json_content <- jsonlite::toJSON(install_json, pretty = TRUE, auto_unbox = TRUE)
  
  # Write to file
  json_file <- file.path(output_directory, "install.json")
  write(json_content, file = json_file)
    
  # Return the study object invisibly
  return(invisible(study))
})


export_entity_to_vdi_recursively <- function(
  object,
  output_directory,
  install_json,
  entitytypegraph_cache,
  study
) {
  entities <- object
  current_entity <- entities[[length(entities)]]
  parent_entity <- if (length(entities) > 1) entities[[length(entities) - 1]] else NULL

  # Add current entity to `entitytypegraph_cache`
  entity_entry <- tibble(
    stable_id = current_entity %>% get_stable_id(),
    study_stable_id = study %>% get_study_id(),
    parent_stable_id = if (is.null(parent_entity)) NA else parent_entity %>% get_stable_id(),
    internal_abbrev = study %>% get_entity_abbreviation(current_entity %>% get_entity_name()),
    description = current_entity %>% get_description(),
    display_name = current_entity %>% get_display_name(),
    display_name_plural = current_entity %>% get_display_name_plural(),
    has_attribute_collections = NA, # Placeholder for now
    is_many_to_one_with_parent = NA, # Placeholder for now
    cardinality = NA # Placeholder for now
  )
  entitytypegraph_cache <- append(entitytypegraph_cache, list(entity_entry))
  
  install_json <- export_ancestors_to_vdi(entities, output_directory, install_json, study)
  install_json <- export_attributegraph_to_vdi(entities, output_directory, install_json, study)
  
  
  # Recurse into child entities
  child_entities <- current_entity %>% get_children()
  for (child in child_entities) {
    updated_data <- export_entity_to_vdi_recursively(
      EntityPath(c(entities, list(child))),
      output_directory,
      install_json,
      entitytypegraph_cache,
      study
    )
    install_json <- updated_data$install_json
    entitytypegraph_cache <- updated_data$entitytypegraph_cache
  }
  
  # Return updated `install_json` and `entitytypegraph_cache`
  list(
    install_json = install_json,
    entitytypegraph_cache = entitytypegraph_cache
  )
}

#'
#' dump the ancestors_{study_abbrev}_{entity_abbrev}.cache file and append
#' install_json with the table info
#'
#'
export_ancestors_to_vdi <- function(entities, output_directory, install_json, study) {
  current_entity <- entities[[length(entities)]]
  
  # Map the list of entities to a list of tibbles `ids`
  ids <- map(entities, function(entity) {
    entity_data <- entity %>% get_data()
    
    if (is.null(entity %>% get_parent_id_column())) {
      # If the entity has no parent, output a single-column tibble
      entity_data %>% select(entity %>% get_entity_id_column())
    } else {
      # If the entity has a parent, output a two-column tibble
      entity_data %>% 
        select(
          get_parent_id_column(entity),
          get_entity_id_column(entity)
        )
    }
  })
  
  # Reduce the list of tibbles to a single tibble of ancestors by joining
  ancestors <- reduce(ids, ~ right_join(.x, .y, by = intersect(names(.x), names(.y))))
  
  # Output the data
  tablename <- glue("ancestors_{study %>% get_study_abbreviation()}_{study %>% get_entity_abbreviation(current_entity %>% get_entity_name())}")
  filename <- glue("{tablename}.cache")
  write_tsv(ancestors, file.path(output_directory, filename), col_names = FALSE)
  
  # Create the table definition JSON
  
  # Map the list of entities to a list of field definitions
  field_defs <- map2(entities, seq_along(entities) - 1, function(entity, index_minus_one) {
    field_def <- ancestors_table_field_def
    
    # Set the field name to {entity_abbreviation}_stable_id
    field_def$name <- glue("{study %>% get_entity_abbreviation(entity %>% get_entity_name())}_stable_id")
    
    # Set the cacheFileIndex to the entity's index (starting at zero)
    field_def$cacheFileIndex <- index_minus_one
    
    return(field_def)
  })
  
  ancestors_table_def <- list(
    name = tablename,
    fields = field_defs,
    type = "table"
  )

  install_json <- append(install_json, list(ancestors_table_def))

  # TO DO: indexes for this table
  
  return(install_json)
}


#'
#' dump the entity variable metadata into the
#' attributegraph_{study_abbrev}_{entity_abbrev}.cache file and append
#' install_json with the table info
#'
#'
export_attributegraph_to_vdi <- function(entities, output_directory, install_json, study) {
  current_entity <- entities[[length(entities)]]
  
  # `metadata` has a column `variable` containing the internal variable name
  # and about 30 other columns with metadata about display names, min/max ranges etc
  metadata <- current_entity %>% get_variable_metadata()
  
  # data has column names that correspond to the `variable` names in `metadata`
  data <- current_entity %>% get_data()

  # For the rows (variables) in `metadata` where `data_shape != 'continuous'`
  # the corresponding column in `data` should contain factor values.
  # We need to add a vocabulary column to `metadata` that contains a JSON string
  # of the values of the factor for those variables, and NA otherwise.
  
  # For the rows (variables) in `metadata` where `data_shape != 'continuous'`,
  # the corresponding column in `data` should contain factor values.
  # We need to add a vocabulary column to `metadata` that contains a JSON string
  # of the factor levels for those variables, and NA otherwise.
  
  metadata <- metadata %>%
    rowwise() %>% # Row-wise operation since we process individual rows
    mutate(
      vocabulary = if_else(
        # For non-continuous variables, encode levels as JSON
        data_shape != 'continuous',
        as.character(
          jsonlite::toJSON(
            levels(data %>% pull(variable)), 
            auto_unbox = FALSE
          )
        ),
        # Otherwise, set vocabulary to NA
        NA_character_
      )
    ) %>%
    ungroup()

  # get the column names in order (sort by cacheFileIndex)
  column_names <- attributegraph_table_fields %>%
    map(~ list(name = .x$name, cacheFileIndex = .x$cacheFileIndex)) %>%
    bind_rows() %>%
    arrange(cacheFileIndex) %>%
    pull(name)
  
  # get the metadata in the correct column order ready for dumping to .cache file
  metadata <- metadata %>% select(all_of(column_names))
  
  # Output the data
  tablename <- glue("attributegraph_{study %>% get_study_abbreviation()}_{study %>% get_entity_abbreviation(current_entity %>% get_entity_name())}")
  filename <- glue("{tablename}.cache")
  write_tsv(metadata, file.path(output_directory, filename), col_names = FALSE)

  # TO DO: need to set `maxLength` to max from data for all type="SQL_VARCHAR"
  # in `attributegraph_table_fields` before adding to `install_json`
  
    
  return(install_json)   
}
