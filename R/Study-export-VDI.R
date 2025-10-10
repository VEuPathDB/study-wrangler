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
  # reset the warning message deduplicator
  message_without_dupes$reset()
  
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

  write_tsv(
    entitytypegraph_cache,
    file = file.path(output_directory, "entitytypegraph.cache"),
    col_names = FALSE,
    na = '',
    escape = 'none' # just in case there are double-quotes in the display names
  )
  
  # Convert to JSON and pretty-print
  json_content <- jsonlite::toJSON(install_json, pretty = TRUE, auto_unbox = TRUE)
  
  # Write to file
  json_file <- file.path(output_directory, "install.json")
  write(json_content, file = json_file)
    
  # Return the study object invisibly
  return(invisible(study))
})


#'
#' recursive because each entity needs all its ancestors in order so it can
#' output the ancestors table
#'
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
  entity_abbreviation <- study %>% get_entity_abbreviation(current_entity %>% get_entity_name())
  
  # Add current entity to `entitytypegraph_cache`
  entity_entry <- tibble(
    stable_id = current_entity %>% get_stable_id(),
    study_stable_id = study %>% get_study_id(),
    parent_stable_id = if (is.null(parent_entity)) NA else parent_entity %>% get_stable_id(),
    internal_abbrev = entity_abbreviation,
    description = current_entity %>% get_description(),
    display_name = current_entity %>% get_display_name(),
    display_name_plural = current_entity %>% get_display_name_plural(),
    has_attribute_collections = as.integer(nrow(current_entity@collections) > 0),
    is_many_to_one_with_parent = as.integer(is_many_to_one_with_ancestor(current_entity)),
    cardinality = current_entity %>% get_data() %>% nrow()
  )
  entitytypegraph_cache <- append(entitytypegraph_cache, list(entity_entry))
  
  install_json <- export_ancestors_to_vdi(entities, output_directory, install_json, study)
  install_json <- export_attributes_to_vdi(entities, output_directory, install_json, study)
  install_json <- export_collections_to_vdi(entities, output_directory, install_json, study)
  
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

#' is_many_to_one_with_ancestor
#' 
#' Determines whether the entity has a many-to-one relationship with its parent.
#' 
#' @param entity An entity object
#' 
#' @returns Logical. TRUE if the entity has a many-to-one relationship with its parent, FALSE otherwise.
#' @export
is_many_to_one_with_ancestor <- function(entity) {
  parent_id_column <- get_parent_id_column(entity)
  
  if (is.null(parent_id_column)) {
    return(FALSE)
  }
  
  parent_ids <- entity %>% get_data() %>% pull(parent_id_column)
  
  return(any(duplicated(parent_ids)))
}


#'
#' dump the ancestors_{study_abbrev}_{entity_abbrev}.cache file and append
#' install_json with the table info
#'
#'
export_ancestors_to_vdi <- function(entities, output_directory, install_json, study) {
  current_entity <- entities[[length(entities)]]
  entity_abbreviation <- study %>% get_entity_abbreviation(current_entity %>% get_entity_name())
  
  # Map the list of entities to a list of tibbles `ids`
  ids <- map(entities, function(entity) {
    entity_data <- entity %>% get_data()
    
    if (is.null(get_parent_id_column(entity))) {
      # If the entity has no parent, output a single-column tibble
      entity_data %>% select(all_of(entity %>% get_entity_id_column()))
    } else {
      # If the entity has a parent, output a two-column tibble
      entity_data %>% 
        select(all_of(c(
          get_parent_id_column(entity),
          get_entity_id_column(entity)
        )))
    }
  })
  
  # Reduce the list of tibbles to a single tibble of ancestors by joining
  ancestors <- reduce(ids, ~ right_join(.x, .y, by = intersect(names(.x), names(.y))))
  
  # Output the data
  tablename <- glue("ancestors_{study %>% get_study_abbreviation()}_{entity_abbreviation}")
  filename <- glue("{tablename}.cache")
  write_tsv(ancestors, file.path(output_directory, filename), col_names = FALSE)
  
  # Create the table definition JSON
  
  # Map the list of entities to a list of field definitions
  field_defs <- map2(entities, seq_along(entities) - 1, function(entity, index_minus_one) {
    e_abbrev <- study %>% get_entity_abbreviation(entity %>% get_entity_name())
    field_def <- ancestors_table_field_def
    
    # Set the field name to {e_abbrev}_stable_id
    field_def$name <- glue("{e_abbrev}_stable_id")
    
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

  # add index
  index_def <- ancestors_pkey_def
  index_def$tableName <- tablename
  index_def$name <- gsub('####', tablename, index_def$name, fixed = TRUE)
  index_def$orderedColumns <- index_def$orderedColumns %>%
    map(~ gsub('####', entity_abbreviation, .x, fixed = TRUE))
  install_json <- append(install_json, list(index_def))
  
  return(install_json)
}

#'
#' function to set the max_length or prec (precision) value
#' of a VDI table field to the maximum length (or precision)
#' observed in the metadata table
#'
set_vdi_field_maxima <- function(metadata, field_def) {
  column_values <- metadata %>% pull(field_def$name) %>% as.character()
  max_length <- column_values %>%
    nchar() %>% replace(is.na(.), 1) %>% max(default=1, na.rm = TRUE)
  if (field_def$type == "SQL_VARCHAR") {
    field_def$maxLength <- max_length
  } else if (field_def$type == "SQL_NUMBER") {
    field_def$prec <- max_length
  }
  return(field_def)
}

#'
#' dump the entity variable metadata into the
#' attributegraph_{study_abbrev}_{entity_abbrev}.cache file and append
#' install_json with the table info
#'
#' AND...
#' 
#' dump the entity variables' values to attributevalue_{study_abbrev}_{entity_abbrev}.cache
#' and append install_json
#'
export_attributes_to_vdi <- function(entities, output_directory, install_json, study) {
  current_entity <- entities[[length(entities)]]
  entity_abbreviation <- study %>% get_entity_abbreviation(current_entity %>% get_entity_name())
  
  # `metadata` has a column `variable` containing the internal variable name
  # and about 30 other columns with metadata about display names, min/max ranges etc
  # hydrated means that default IDs are generated
  metadata <- current_entity %>% get_hydrated_variable_and_category_metadata()

  # data has column names that correspond to the `variable` names in `metadata`
  data <- current_entity %>% get_data()

  ### metadata to attributegraph.cache ###

  # JSONify some array fields of metadata  
  metadata <- metadata %>%
    mutate(across(where(is.logical), as.integer)) %>% # TRUE/FALSE to 0/1
    rowwise() %>% # Row-wise operation since we process individual rows
    mutate(
      # need to catch NAs specifically otherwise they end up as "{}" in the output
      vocabulary = if_else(all(is.na(vocabulary)), '', as.character(jsonlite::toJSON(vocabulary))),
      # without the `unlist()` the export is too nested: "[['label1', 'label2']]"
      provider_label = jsonify_list_column(provider_label),
      hidden = jsonify_list_column(hidden)
    ) %>%
    ungroup()

  # get the column names in order (sort by cacheFileIndex)
  column_names <- attributegraph_table_fields %>%
    map(~ list(name = .x$name, cacheFileIndex = .x$cacheFileIndex)) %>%
    bind_rows() %>%
    arrange(cacheFileIndex) %>%
    pull(name)
  
  # get the metadata in the correct column order ready for dumping to .cache file
  metadata_only <- metadata %>% select(all_of(column_names))
  
  # Output the data
  tablename <- glue("attributegraph_{study %>% get_study_abbreviation()}_{entity_abbreviation}")
  filename <- glue("{tablename}.cache")
  # `escape = 'none'` prevents doubling of double-quotes
  write_tsv(
    metadata_only,
    file.path(output_directory, filename),
    col_names = FALSE,
    na = '',
    escape = 'none'
  )

  # set `maxLength` to max from data for all type="SQL_VARCHAR"
  # in `attributegraph_table_fields` before adding to `install_json`
  # also similar treatment for `prec` field for "SQL_NUMBER" fields
  
  field_defs <- attributegraph_table_fields %>%
    map(partial(set_vdi_field_maxima, metadata))
  
  attributegraph_table_def <- list(
    name = tablename,
    type = "table",
    fields = field_defs
  )
  install_json <- append(install_json, list(attributegraph_table_def))

  # take care of the index
  index_def <- attributegraph_pkey_def
  index_def$tableName <- tablename
  index_def$name <- gsub('####', tablename, index_def$name, fixed = TRUE)
  install_json <- append(install_json, list(index_def))
  
  ### actual values to attributevalue.cache ###

  # make a variable name to stable_id lookup before we start messing with `metadata`
  stable_ids <- metadata %>% select(variable, stable_id) %>% deframe()

  # the different value columns need to be handled separately because
  # pivot_longer doesn't seem to be able to handle this complex case:
  # string, number, then date (order is important!)
  
  # the ID column needs to be renamed to the following:
  id_col <- current_entity %>% get_entity_id_column()
  id_col_vdi <- glue("{entity_abbreviation}_stable_id")
  
  # Handle string variables
  string_variables <- metadata %>% filter(data_type == 'string') %>% pull(variable)
  string_data <- if (length(string_variables) > 0) {
    data %>%
      # rename the ID column to `id_col_vdi`
      select("{id_col_vdi}" := {{id_col}}, all_of(string_variables)) %>%
      # convert to character in case it was a numeric ID column
      mutate("{id_col_vdi}" := as.character(!!sym(id_col_vdi))) %>%
      pivot_longer(
        all_of(string_variables),
        names_transform = function(name) stable_ids[name],
        names_to = "attribute_stable_id",
        values_to = "string_value"
      )
  } else {
    tibble("{id_col_vdi}" := character(), attribute_stable_id = character(), string_value = character())
  }
  
  # Handle number variables
  number_variables <- metadata %>% filter(data_type %in% c("integer", "number", "longitude")) %>% pull(variable)
  number_data <- if (length(number_variables) > 0) {
    data %>%
      select("{id_col_vdi}" := {{id_col}}, all_of(number_variables)) %>%
      # convert to character in case it was a numeric ID column
      mutate("{id_col_vdi}" := as.character(!!sym(id_col_vdi))) %>%
      pivot_longer(
        all_of(number_variables),
        names_transform = function(name) stable_ids[name],
        names_to = "attribute_stable_id",
        values_to = "number_value"
      )
  } else {
    tibble("{id_col_vdi}" := character(), attribute_stable_id = character(), number_value = numeric())
  }

  # Handle date variables
  date_variables <- metadata %>% filter(data_type == "date") %>% pull(variable)
  date_data <- if (length(date_variables) > 0) {
    data %>%
      select("{id_col_vdi}" := {{id_col}}, all_of(date_variables)) %>%
      # convert to character in case it was a numeric ID column
      mutate("{id_col_vdi}" := as.character(!!sym(id_col_vdi))) %>%
      pivot_longer(
        all_of(date_variables),
        names_transform = function(name) stable_ids[name],
        names_to = "attribute_stable_id",
        values_to = "date_value"
      )
  } else {
    tibble("{id_col_vdi}" := character(), attribute_stable_id = character(), date_value = as.Date(character()))
  }

  # Combine all data types
  attribute_values_data <- bind_rows(string_data, number_data, date_data) %>%
    arrange(!!sym(id_col_vdi))
  # TO DO: find out if the sort (arrange) is necessary/desirable.
  
  # Output the data
  tablename <- glue("attributevalue_{study %>% get_study_abbreviation()}_{entity_abbreviation}")
  filename <- glue("{tablename}.cache")
  # `escape = 'none'` prevents doubling of double-quotes
  write_tsv(
    attribute_values_data,
    file.path(output_directory, filename),
    col_names = FALSE,
    na = '',
    escape = 'none'
  )
  
  
  # write tsv and append install_json (making sure `fields[1]$name` is changed to `id_col_vdi`)
  field_defs <- attributevalue_table_fields
  
  # set the first column name to "hshl_stable_id" or similar
  field_defs[[1]]$name <- id_col_vdi
  
  # set the max length of string values
  field_defs[[3]]$maxLength <- attribute_values_data %>%
    pull(string_value) %>%
    as.character() %>%
    nchar() %>%
    replace(is.na(.), 1) %>%
    max()
  
  attributevalues_table_def <- list(
    name = tablename,
    type = 'table',
    fields = field_defs
  )
  install_json <- append(install_json, list(attributevalues_table_def))
  
  # and the indexes
  index_defs <- map(
    attributevalue_index_defs,
    function(index_def) {
      index_def$tableName <- tablename
      index_def$name <- gsub('####', tablename, index_def$name, fixed = TRUE)
      index_def$orderedColumns <- index_def$orderedColumns %>%
        map(~ gsub('####', entity_abbreviation, .x, fixed = TRUE))
      return(index_def)
    }
  )
  install_json <- append(install_json, index_defs)
  
  return(install_json)   
}


#'
#' dump the collections_{study_abbrev}_{entity_abbrev}.cache file and append
#' install_json with the table info
#'
#'
export_collections_to_vdi <- function(entities, output_directory, install_json, study) {
  current_entity <- entities[[length(entities)]]
  entity_abbreviation <- study %>% get_entity_abbreviation(current_entity %>% get_entity_name())
  
  metadata <- current_entity %>% get_hydrated_collection_metadata()
  
  if (nrow(metadata) == 0) {
    return(install_json)
  }
  ### metadata to collection_blah_blah.cache ###
  
  # JSONify some array fields of metadata  
  metadata <- metadata %>%
    mutate(across(where(is.logical), as.integer)) # TRUE/FALSE to 0/1

  # get the column names in order (sort by cacheFileIndex)
  column_names <- collections_table_fields %>%
    map(~ list(name = .x$name, cacheFileIndex = .x$cacheFileIndex)) %>%
    bind_rows() %>%
    arrange(cacheFileIndex) %>%
    pull(name)
  
  # get the metadata in the correct column order ready for dumping to .cache file
  metadata_only <- metadata %>% select(all_of(column_names))
  
  # Output the data
  tablename <- glue("collection_{study %>% get_study_abbreviation()}_{entity_abbreviation}")
  filename <- glue("{tablename}.cache")
  # `escape = 'none'` prevents doubling of double-quotes
  write_tsv(
    metadata_only,
    file.path(output_directory, filename),
    col_names = FALSE,
    na = '',
    escape = 'none'
  )
  
  # set `maxLength` to max from data for all type="SQL_VARCHAR"
  # in `collections_table_fields` before adding to `install_json`
  # also similar treatment for `prec` field for "SQL_NUMBER" fields
  field_defs <- collections_table_fields %>%
    map(partial(set_vdi_field_maxima, metadata_only))
  
  collections_table_def <- list(
    name = tablename,
    type = "table",
    fields = field_defs
  )
  install_json <- append(install_json, list(collections_table_def))
  
  # add index
  index_def <- collections_pkey_def
  index_def$tableName <- tablename
  index_def$name <- gsub('####', tablename, index_def$name, fixed = TRUE)
  install_json <- append(install_json, list(index_def))
  
  
  ### now the collectionattribute_* table (links collection to variables)

  # table names, etc
  collectionattributes_table_name <- glue::glue(
    "collectionattribute_{study %>% get_study_abbreviation()}_{entity_abbreviation}"
  )
  collectionattributes_filename <- glue("{collectionattributes_table_name}.cache")

  # do the join to get the data
  collectionattributes <- metadata %>%
    left_join(
      current_entity %>% get_hydrated_variable_and_category_metadata(),
      join_by(category == parent_variable),
    )  %>%
    # select and rename these in the correct order (see VDI-schema.R)
    select(collection_stable_id = stable_id.x, attribute_stable_id = stable_id.y)
  
  # write the data
  write_tsv(
    collectionattributes,
    file.path(output_directory, collectionattributes_filename),
    col_names = FALSE,
    na = '',
    escape = 'none'
  )

  field_defs <- collectionattributes_table_fields %>%
    map(partial(set_vdi_field_maxima, collectionattributes))
  
  # update schema JSON
  collectionattributes_table_def <- list(
    name = collectionattributes_table_name,
    type = "table",
    fields = field_defs
  )
  install_json <- append(install_json, list(collectionattributes_table_def))

  # add index
  index_def <- collectionattributes_index_def
  index_def$tableName <- collectionattributes_table_name
  index_def$name <- gsub('####', collectionattributes_table_name, index_def$name, fixed = TRUE)
  install_json <- append(install_json, list(index_def))
  
  return(install_json)  
}


jsonify_list_column <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    ""
  } else {
    as.character(jsonlite::toJSON(unlist(x)))
  }
}

