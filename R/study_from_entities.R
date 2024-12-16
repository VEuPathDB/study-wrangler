study_from_entities <- function(entities, ...) {
  # check extra args are valid slots
  metadata = list(...)
  validate_object_metadata_names('Entity', metadata)

  # Validate input
  if (!is.list(entities)) stop("Entities must be provided as a list.")
  if (length(entities) == 0) stop("At least one entity must be provided.")
  
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
  # stripping all entities of their @children
  entities <- lapply(entities, function(entity) {
    entity@children <- list()
    return(entity)
  })
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
