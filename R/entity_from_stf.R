#' entity_from_stf
#' 
#' @description
#' Creates an entity object from a TSV file and optional YAML metadata file
#'
#' @param tsv_path path to one STF-format TSV file
#' @param yaml_path path to one YAML file containing STF metadata (optional)
#'        if not provided, the function will look for a .yaml or .yml file with the same
#'        file stem as `tsv_path` and if it exists, use it.
#'        If no `yaml_path` is provided or discovered, the function will go ahead
#'        and load the data without additional metadata.
#' @return An entity object
#'
#' @export
entity_from_stf <- function(tsv_path, yaml_path = NULL) {
  # handle automatic yaml_path discovery
  if (is.null(yaml_path)) {
    # strip off the final extension, then re-attach .yaml and .yml
    candidates <- c("yaml", "yml") %>% 
      map_chr(~ fs::path_ext_set(tsv_path, .x))
    
    # keep only the ones that exist on disk, take the first (or stay NULL)
    existing <- keep(candidates, fs::file_exists)
    if (length(existing)) {
      yaml_path <- existing[[1]]
    }
  }
  
  # Read in the file as all-character with no headers
  data <- read_tsv(tsv_path, col_names = FALSE, col_types = cols(.default = "c"), progress = FALSE)

  # Determine if it's wide or tall format based on the first row
  is_wide <- any(str_detect(data[1, ], ".+\\\\\\\\ Descriptors|Descriptors \\\\\\\\"))
  
  # If tall, transpose to wide format
  if (!is_wide) {
    data <- data %>% t() %>% as_tibble(.name_repair = "unique")
  }
  
  # Extract the header row
  headers <- data %>% slice_head(n = 1) %>% unlist(use.names = FALSE)
  data <- data %>% slice_tail(n = nrow(data) - 1)  # Drop header row
  
  # Locate the "Descriptors" marker column**
  descriptor_col_index <- which(str_detect(headers, "\\\\+\\s*Descriptors|Descriptors\\s*\\\\+"))

  # clean headers without "Descriptors"
  clean_headers <- str_remove(headers, "\\s*\\\\+\\s*Descriptors|Descriptors\\s*\\\\+\\s*")
  
  # set the data column names
  colnames(data) <- clean_headers
  
  # do basic R type detection on columns
  data <- data %>% type_convert_quietly()
  
  # Initialize placeholders
  variables <- NULL
  entity_metadata <- NULL
  collections <- empty_collections
  
  # Function to process metadata lists safely into a tibble
  process_metadata_list <- function(metadata_list, metadata_defaults) {
    if (length(metadata_list) == 0) {
      return(NULL)  # Return NULL so `bind_rows()` later skips empty sections
    }
    
    metadata_list %>%
      map(function(row_object) {
        # Merge defaults with the row's metadata
        merged <- list_modify(as.list(metadata_defaults), !!!row_object)

        if (length(merged) != ncol(metadata_defaults) + 1)
          stop("Error: unused fields in YAML are not allowed")
        # TO DO: handle this more cleanly. Validate YAML and report issues before attempting to load?
        
        # Convert each element based on its default type
        merged <- map2(
          merged,
          c(as.list(metadata_defaults), list(variable = NA_character_)),
          function(value, default) {
            # handle list(factor) e.g. 'hidden' field
            if (is.list(default) && is.factor(unlist(default))) {
              lvls <- levels(unlist(default))
              return(as.list(value) %>% map(~ factor(.x, levels=lvls)))
            }
            # TO DO: explain why list-of-empty-list is handled differently
            if (is.list(default) && !identical(value, list(list()))) {
              return(list(as.list(value)))
            }
            if (is.factor(default)) {
              # If default is factor, convert value to factor using levels from default.
              # This works even if `value` is already a factor.
              return(factor(value, levels = levels(default)))
            }
            if (is.integer(default)) {
              return(as.integer(value))
            }
            if (is.numeric(default)) {
              return(as.numeric(value))
            }
            if (inherits(default, "Date")) {
              return(as.Date(value))
            }
            # For any other type (including character), leave the value as is.
            return(value)
          }
        )
        
        tibble(
          !!!merged
        )
      }) %>%
      reduce(bind_rows) %>%
      relocate(variable)
  }
  
  if (is.null(yaml_path) || !file.exists(yaml_path)) {
    if (length(descriptor_col_index) != 1) {
      stop("Error: Could not determine a single 'Descriptors' marker column.")
    }
    
    # Extract ID column names** (everything before the `Descriptors` column)
    id_column_names <- clean_headers[seq_len(descriptor_col_index)]
    
    # The right-most ID column before Descriptors is the entity name**
    this_entity_name <- id_column_names[length(id_column_names)]
    
    # Create `metadata` list with inferred entity name**
    entity_metadata <- list(name = this_entity_name)
    
    # Assign `entity_level` values based on order (-n to 0)**
    entity_levels <- seq_along(id_column_names) - length(id_column_names)
    
    # Process ID column metadata (`ids_metadata`)**
    ids_metadata <- map2(
      id_column_names,
      entity_levels,
      ~ list(variable = .x, entity_name = .x, entity_level = .y, data_type = factor('id'))
    ) %>%
      process_metadata_list(variable_metadata_defaults)
    
    # Process regular variables (everything after the Descriptors column)**
    variable_column_names <- headers[(descriptor_col_index + 1):length(headers)]

    variables_metadata <- map(
      variable_column_names,
      ~ list(variable = .x, entity_name = this_entity_name, entity_level = 0L, provider_label = list(.x))
    ) %>%
      process_metadata_list(variable_metadata_defaults)
    
    # **9. Combine ID and regular variable metadata**
    variables <- bind_rows(ids_metadata, variables_metadata)
    
  } else {
    # **Load metadata from YAML**
    metadata <- yaml::read_yaml(
      yaml_path,
      handlers = list(seq = function(x) as.list(x))  # Convert sequences (arrays) into lists
    )
    
    # Extract non-list elements as entity metadata
    entity_metadata <- metadata %>% discard(is.list)

    # Process `id_columns`, `variables`, and `categories`, skipping empty ones
    variables <- list(
      process_metadata_list(
        metadata$id_columns %>%
          map(
            ~ list_modify(.x, variable = .x$id_column, id_column = zap(), data_type = factor("id"))
          ),
        variable_metadata_defaults
      ),
      process_metadata_list(
        metadata$variables %>%
          map(
            ~ list_modify(.x, entity_name = entity_metadata$name)
          ),
        variable_metadata_defaults
      ),
      process_metadata_list(
        metadata$categories %>%
          map(
            ~ list_modify(
              .x,
              variable = .x$category,
              category = zap(), # removes the field from the object
              data_type = factor('category'),
              entity_name = entity_metadata$name
            )
          ),
        variable_metadata_defaults
      )
    ) %>%
      compact() %>%   # Remove NULLs to avoid unnecessary binds
      bind_rows()
    
    # process the collections through a function that expects the main
    # field to be called 'variable', so we do a temporary rename of
    # 'category' to 'variable'
    if (!is_empty(metadata$collections)) {
      collections <-
        process_metadata_list(
          metadata$collections %>%
            map(
              ~ list_modify(
                .x,
                variable = .x$category,
                category = zap()
              )          
            ),
          collection_metadata_defaults
        ) %>%
        compact() %>%
        bind_rows() %>%
        # and then rename it back again
        rename(category = variable)
    }
  }

  # Construct entity
  constructor_args <- c(list(data = data, variables = variables, collections = collections), entity_metadata)
  entity <- do.call(entity, constructor_args) %>%
    quiet() %>%
    sync_variable_metadata() %>%
    infer_missing_data_types(.disallowed_data_types = c('id')) %>%
    infer_missing_data_shapes() %>%
    sync_ordinal_data() %>%
    verbose()
  
  return(entity)
}
