#' Entity Collection Validators
#'
#' Validation functions for entity variable collections.
#'
#' @name entity-validators-collections
NULL

#' Validator: Check collections have corresponding variable categories
#' @keywords internal
validate_entity_collections_have_categories <- function(entity) {
  collections <- entity@collections
  
  if (is.null(collections) || nrow(collections) == 0) {
    return(list(valid = TRUE))
  }
  
  orphan_collections <- collections %>%
    anti_join(get_category_metadata(entity), join_by(category == variable)) %>%
    pull(category)
  
  if (length(orphan_collections) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "These variable collections have no corresponding variable category: ",
        paste0(orphan_collections, collapse=', ')
      )
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check collections have required metadata
#' @keywords internal
validate_entity_collections_required_metadata <- function(entity) {
  collections <- entity@collections
  
  if (is.null(collections) || nrow(collections) == 0) {
    return(list(valid = TRUE))
  }
  
  required_collection_fields <- c('member', 'member_plural', 'display_name')
  missing_collection_fields <- required_collection_fields %>%
    map(
      function(column_name) {
        tibble(
          field = column_name,
          collections = list(collections %>% filter(is.na(!!sym(column_name))) %>% pull(category)),
        )
      }
    ) %>%
    bind_rows() %>%
    filter(lengths(collections) > 0)
  
  if (nrow(missing_collection_fields) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "Required metadata fields were missing in the following collections:\n",
        paste(capture.output(kable(missing_collection_fields)), collapse = "\n")
      )
    ))
  }
  
  list(valid = TRUE)
}

#' Validator: Check collection child variables have consistent metadata
#' @keywords internal
validate_entity_collections_homogeneous <- function(entity) {
  collections <- entity@collections
  variables <- entity@variables
  
  if (is.null(collections) || nrow(collections) == 0) {
    return(list(valid = TRUE))
  }
  
  required_homogeneous_fields <- c('data_type', 'data_shape', 'unit', 'impute_zero')
  bad_category_child_fields <- collections %>%
    left_join(variables, join_by(category == parent_variable), 
              suffix = c(".ignore", "")) %>%
    select(category, all_of(required_homogeneous_fields)) %>%
    pivot_longer(
      cols      = -category,
      names_to  = "field",
      values_to = "value",
      values_transform = as.character
    ) %>% 
    group_by(category, field) %>% summarise(
      n_distinct_values = n_distinct(value, na.rm = FALSE),
      values = paste(unique(value), collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(n_distinct_values > 1) %>%
    select(-n_distinct_values)

  if (nrow(bad_category_child_fields) > 0) {
    return(list(
      valid = FALSE,
      fatal = FALSE,
      message = paste0(
        "One or more variable collections were heterogeneous for metadata fields that should be uniform:\n",
        paste(capture.output(kable(
          bad_category_child_fields %>%
            rename(
              "collection/category" = category,
              "variable metadata field"      = field,
              "observed values"     = values
            )
        )), collapse = "\n")
      )
    ))
  }
  
  list(valid = TRUE)
}