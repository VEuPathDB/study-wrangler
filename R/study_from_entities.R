#' study_from_entities
#'
#' @description
#' Creates a study object from the provided entities and study metadata
#'
#' @param entities a `list` of Entity objects
#' @param metadata_source Optional Study object to copy metadata from (all slots except root_entity)
#' @param ... Additional named parameters to set study metadata (see Study class),
#'   e.g. `name="my beautiful study"`. These override metadata from `metadata_source` if provided.
#' @return A Study object
#'
#' @export
study_from_entities <- function(entities, metadata_source = NULL, ...) {
  # Start with metadata from the metadata_source parameter if provided
  if (!is.null(metadata_source)) {
    if (!inherits(metadata_source, "Study")) {
      stop("The 'metadata_source' parameter must be a Study object")
    }
    # Extract all slots except root_entity
    slot_names <- slotNames(metadata_source)
    metadata_slot_names <- slot_names[slot_names != "root_entity"]
    metadata <- map(metadata_slot_names, ~ slot(metadata_source, .x)) %>% set_names(metadata_slot_names)
  } else {
    metadata <- list()
  }

  # Merge with explicit ... args (which take precedence)
  explicit_metadata <- list(...)
  metadata <- list_modify(metadata, !!!explicit_metadata)

  # check extra args are valid slots
  validate_object_metadata_names('Entity', metadata)

  # Validate input
  if (!is.list(entities)) stop("Entities must be provided as a list.")
  if (length(entities) == 0) stop("At least one entity must be provided.")

  # Make sure all entities are named uniquely
  entity_names <- entities %>% map_chr(get_entity_name)
  
  # Identify names that occur more than once
  duplicate_names <- entity_names[duplicated(entity_names) | duplicated(entity_names, fromLast=TRUE)]
  
  # Annotate duplicates with an asterisk
  annotated_names <- ifelse(entity_names %in% duplicate_names,
                            paste0(entity_names, "*"),
                            entity_names)
  
  if (length(duplicate_names) > 0) {
    stop(to_lines(
      "Entities must all have unique `entity_name` but there are duplicates:",
      annotated_names
    ))
  }

  # Strip all entities of their @children (if they have them)
  # before assembling them together
  entities <- entities %>% map(remove_children)

  # Step 1: Find the root entity (the only parentless entity)
  root_candidates <- Filter(function(entity) is.null(get_parent_name(entity)), entities)
  
  if (length(root_candidates) == 1) {
    root_entity <- root_candidates[[1]]
  } else if (length(root_candidates) > 1) {
    stop("Multiple root entities found. Invalid tree.")
  } else {
    stop("No root entity found. Invalid tree.")
  }

  # Step 2: Recursive function to grow the tree
  grow_tree <- function(parent, entities) {
    parent_name <- get_entity_name(parent)
    
    # Find children of the current parent
    children <- Filter(function(entity) isTRUE(get_parent_name(entity) == parent_name), entities)
    
    # Recursively grow the tree and attach children
    for (child in children) {
      child <- grow_tree(child, entities)
      parent@children <- c(parent@children, list(child))
    }
    
    return(parent)
  }
  
  # Step 3: Grow the tree from the root after
  root_entity <- grow_tree(root_entity, entities)
  
  # Step 4: Sanity check
  tree_entities <- flatten_entities(root_entity)
  if (length(tree_entities) != length(entities)) {
    stop("Tree does not include all provided entities. Check for disconnected or orphaned nodes.")
  }
  
  # Step 5: Create and return the Study object, with merged metadata from ... args
  constructor_args <- c(list(root_entity = root_entity), metadata)
  study <- do.call(study, constructor_args)
  return(study)
}
