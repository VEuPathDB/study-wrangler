#' export_to_vdi
#'
#' Export an `Entity` object as part of the `Study` VDI export process
#'
#' @param object An `EntityPath` object - a list Entities from the Study's root entity to the current entity
#' @param output_directory A character string containing the relative or
#'        absolute path of the output directory (it is assumed to exist by now).
#' @param install_json A JSON-like list object that contains table schema and index definitions
#' @return appended `install_json` JSON-like object
#' not exported - or at least it doesn't need to be
setMethod("export_to_vdi", "EntityPath", function(object, output_directory, install_json, entitytypegraph_cache) {
  entities <- object
  current_entity <- entities[[length(entities)]]
  
  # Add current entity to `entitytypegraph_cache`
  entity_entry <- tibble(
    stable_id = current_entity %>% get_entity_id(),
    study_stable_id = entities[[1]] %>% get_entity_id(), # Root entity's stable ID
    parent_stable_id = if (length(entities) > 1) entities[[length(entities) - 1]] %>% get_entity_id() else NA,
    internal_abbrev = current_entity %>% get_entity_abbreviation(),
    description = current_entity %>% get_entity_description(),
    display_name = current_entity %>% get_entity_name(),
    display_name_plural = current_entity %>% get_entity_plural_name(),
    has_attribute_collections = NA, # Placeholder for now
    is_many_to_one_with_parent = NA, # Placeholder for now
    cardinality = NA # Placeholder for now
  )
  entitytypegraph_cache <- append(entitytypegraph_cache, list(entity_entry))
  
  # Recurse into child entities
  child_entities <- current_entity %>% get_entity_children()
  for (child in child_entities) {
    updated_data <- export_to_vdi(
      EntityPath(c(entities, list(child))),
      output_directory,
      install_json,
      entitytypegraph_cache
    )
    install_json <- updated_data$install_json
    entitytypegraph_cache <- updated_data$entitytypegraph_cache
  }
  
  # Return updated `install_json` and `entitytypegraph_cache`
  list(
    install_json = install_json,
    entitytypegraph_cache = entitytypegraph_cache
  )
})
