#' union_studies
#'
#' @description
#' Combines several studies that share an entity graph into one study, entity by
#' entity, keeping entity and variable stable_ids unchanged.
#'
#' * **Graph.** An entity (matched by stable_id) must have the same parent in every
#'   study that has it. A study may lack an entity the others have; the result has
#'   the union of entities.
#' * **Rows.** Each entity's rows are the rows of every source study that has it.
#' * **Instance ids.** With `prefix_ids = FALSE`, an id found in more than one source
#'   study for the same entity is an error. With `prefix_ids = TRUE`, every entity
#'   instance id and every ancestor id column is prefixed with `<study name>:`.
#' * **Variable metadata.** `data_type` and `data_shape` must agree, except that
#'   `integer`/`number` widens to `number` and `binary`/`categorical` widens to
#'   `categorical`. Any other type or shape disagreement is an error, as is a
#'   differing variable `stable_id` or `multi_value_delimiter`. `vocabulary_order` is
#'   the ordered union of the sources' entries (first study's first), and the
#'   vocabulary itself is derived from the combined data. All other metadata (display names, definitions,
#'   categories, collections, entity names and descriptions) comes from the first
#'   study that has it, with one warning per field where a later study differs.
#' * **Source.** With `source_variable = TRUE`, a string variable `source_dataset`
#'   holding the source study's name is added to every entity directly below the
#'   root (the root itself when it has no children).
#'
#' @param studies a list of `Study` objects, each with a unique name
#' @param name name of the new study
#' @param stable_id stable_id of the new study
#' @param source_variable logical; add the `source_dataset` variable (default `TRUE`)
#' @param prefix_ids logical; prefix entity instance ids with the source study name
#'   (default `FALSE`)
#' @return a new `Study`; metadata other than `name` and `stable_id` comes from the
#'   first study
#' @export
#'
#' @examples
#' \dontrun{
#' combined <- union_studies(
#'   list(study_from_stf("dumps/study-a"), study_from_stf("dumps/study-b")),
#'   name = "combined", stable_id = "combined"
#' )
#' }
union_studies <- function(studies, name, stable_id, source_variable = TRUE, prefix_ids = FALSE) {
  if (!is.list(studies) || length(studies) == 0 || !all(map_lgl(studies, ~ is(.x, "Study")))) {
    stop("`studies` must be a non-empty list of Study objects.")
  }
  study_names <- map_chr(studies, get_study_name)
  if (any(is.na(study_names) | study_names == "") || anyDuplicated(study_names)) {
    stop(glue("Every study must have a unique, non-empty name; got: {toString(study_names)}"))
  }

  graph <- union_entity_graph(studies, study_names)
  target_keys <- if (source_variable) union_source_entity_keys(graph) else character(0)
  diffs <- new.env()

  entity_keys <- unique(graph$key)
  entities <- map(entity_keys, function(key) {
    rows <- graph %>% filter(key == !!key)
    sources <- map(rows$study, function(study_name) {
      e <- studies[[match(study_name, study_names)]] %>%
        get_entities() %>%
        detect(~ union_entity_key(.x) == key)
      if (prefix_ids) e <- union_prefix_ids(e, study_name)
      if (key %in% target_keys) e <- union_add_source_column(e, study_name)
      e
    }) %>% set_names(rows$study)

    union_check_id_columns(sources, key)
    if (!prefix_ids) union_check_id_collisions(sources, key)
    union_entity(sources, key, diffs, add_source_metadata = key %in% target_keys)
  })

  for (field in sort(ls(diffs))) {
    affected <- toString(get(field, envir = diffs))
    warning(
      if (startsWith(field, union_widened_prefix)) {
        glue("union_studies: {field} for: {affected}")
      } else {
        glue("union_studies: {field} differs between source studies; kept the first study's value for: {affected}")
      },
      call. = FALSE
    )
  }

  study_from_entities(entities, metadata_source = studies[[1]], name = name, stable_id = stable_id)
}

union_entity_key <- function(entity) {
  if (length(entity@stable_id) == 1 && !is.na(entity@stable_id)) entity@stable_id else get_entity_name(entity)
}

#' One row per (study, entity): entity key, name and parent key; stops on a graph conflict.
#' @keywords internal
union_entity_graph <- function(studies, study_names) {
  graph <- map2_dfr(studies, study_names, function(study, study_name) {
    entities <- get_entities(study)
    keys <- map_chr(entities, union_entity_key) %>% set_names(map_chr(entities, get_entity_name))
    tibble(
      study = study_name,
      key = unname(keys),
      name = names(keys),
      parent = map_chr(entities, ~ {
        parent_name <- get_parent_name(.x)
        if (is.null(parent_name)) NA_character_ else keys[[parent_name]]
      })
    )
  })

  conflicts <- graph %>%
    group_by(key) %>%
    filter(n_distinct(parent, na.rm = FALSE) > 1 | n_distinct(name) > 1) %>%
    summarise(detail = paste0(study, ": parent ", coalesce(parent, "(none)"), ", name ", name, collapse = "; "))
  if (nrow(conflicts) > 0) {
    stop(to_lines(
      "union_studies: these entities differ in parent or name between studies:",
      glue("  entity '{conflicts$key}' - {conflicts$detail}")
    ))
  }

  roots <- graph %>% filter(is.na(parent)) %>% distinct(key) %>% pull(key)
  if (length(roots) != 1) {
    stop(glue("union_studies: the studies must share one root entity; found: {toString(roots)}"))
  }
  graph
}

#' Keys of the entities that get `source_dataset`: the root's children, else the root.
#' @keywords internal
union_source_entity_keys <- function(graph) {
  root <- graph %>% filter(is.na(parent)) %>% pull(key) %>% unique()
  children <- graph %>% filter(parent %in% root) %>% pull(key) %>% unique()
  if (length(children) > 0) children else root
}

union_id_columns <- function(entity) {
  entity@variables %>% filter(data_type == "id") %>% pull(variable)
}

union_own_id_column <- function(entity) {
  entity@variables %>% filter(data_type == "id", entity_level == 0) %>% pull(variable)
}

union_prefix_ids <- function(entity, study_name) {
  id_columns <- union_id_columns(entity)
  data <- entity@data %>%
    mutate(across(all_of(id_columns), ~ if_else(is.na(.x), NA_character_, paste0(study_name, ":", .x))))
  entity %>% initialize(data = data)
}

union_add_source_column <- function(entity, study_name) {
  if ("source_dataset" %in% c(colnames(entity@data), entity@variables$variable)) {
    stop(glue("union_studies: entity '{get_entity_name(entity)}' in study '{study_name}' already has a 'source_dataset' variable."))
  }
  entity %>% initialize(data = entity@data %>% mutate(source_dataset = study_name))
}

union_check_id_columns <- function(sources, key) {
  signatures <- map_chr(sources, ~ .x@variables %>%
    filter(data_type == "id") %>%
    arrange(entity_level) %>%
    with(paste0(variable, "@", entity_level, collapse = ",")))
  if (n_distinct(signatures) > 1) {
    stop(to_lines(
      glue("union_studies: entity '{key}' has different id columns between studies:"),
      glue("  {names(signatures)}: {signatures}")
    ))
  }
}

union_check_id_collisions <- function(sources, key) {
  ids <- map(sources, ~ { x <- .x@data[[union_own_id_column(.x)]]; x[!is.na(x)] })
  for (i in seq_along(ids)[-1]) {
    for (j in seq_len(i - 1)) {
      shared <- intersect(ids[[j]], ids[[i]])
      if (length(shared) > 0) {
        stop(glue(
          "union_studies: entity '{key}' has {length(shared)} id(s) in both '{names(ids)[j]}' and '{names(ids)[i]}', ",
          "e.g. {toString(head(shared, 5))}. Use prefix_ids = TRUE to keep them apart."
        ))
      }
    }
  }
}

union_widened_prefix <- "widened "

union_widenings <- list(
  data_type = list(mix = c("integer", "number"), to = "number"),
  data_shape = list(mix = c("binary", "categorical"), to = "categorical")
)

# Metadata columns that may not differ between sources (they define the data).
union_strict_fields <- c("stable_id", "multi_value_delimiter")
union_structural_fields <- c("variable", "entity_name", "entity_level", "data_type", "data_shape")

union_record_diff <- function(diffs, field, label) {
  existing <- if (exists(field, envir = diffs, inherits = FALSE)) get(field, envir = diffs) else character(0)
  assign(field, c(existing, label), envir = diffs)
}

#' Merges one variable's metadata rows (one per source study, first study first).
#' @keywords internal
union_variable_row <- function(rows, entity_key, diffs) {
  variable <- rows$variable[1]
  first <- rows[1, ]
  widened_shape <- FALSE

  for (field in names(union_widenings)) {
    values <- unique(as.character(rows[[field]]))
    if (length(values) <= 1) next
    rule <- union_widenings[[field]]
    if (!setequal(values, rule$mix)) {
      stop(glue(
        "union_studies: variable '{variable}' in entity '{entity_key}' has conflicting {field}: ",
        "{paste0(rows$.study, ' = ', as.character(rows[[field]]), collapse = ', ')}"
      ))
    }
    first[[field]] <- factor(rule$to, levels = levels(rows[[field]]))
    union_record_diff(diffs, glue("{union_widened_prefix}{field} to '{rule$to}'"),
                      glue("{entity_key}.{variable}"))
    if (field == "data_shape") widened_shape <- TRUE
  }

  for (field in union_strict_fields) {
    values <- unique(rows[[field]])
    if (length(values) > 1) {
      stop(glue(
        "union_studies: variable '{variable}' in entity '{entity_key}' has conflicting {field}: ",
        "{paste0(rows$.study, ' = ', rows[[field]], collapse = ', ')}"
      ))
    }
  }

  # Every source's pinned values, first study's first; nothing is dropped.
  vocabulary_order <- unique(unlist(rows$vocabulary_order))
  first$vocabulary_order <- list(if (is.null(vocabulary_order)) list() else as.list(vocabulary_order))

  compared <- setdiff(colnames(rows), c(union_structural_fields, union_strict_fields, "vocabulary_order", ".study"))
  for (field in compared) {
    for (i in seq_len(nrow(rows))[-1]) {
      if (!identical(rows[[field]][i], rows[[field]][1])) {
        union_record_diff(diffs, glue("variable {field}"), glue("{entity_key}.{variable} ({rows$.study[i]})"))
        break
      }
    }
  }
  first
}

union_merge_by_first <- function(tables, key_column, label, entity_key, diffs) {
  all_rows <- bind_rows(tables, .id = ".study")
  if (nrow(all_rows) == 0) return(tables[[1]])
  merged <- all_rows %>% group_by(.data[[key_column]]) %>% group_split()
  for (rows in merged) {
    for (field in setdiff(colnames(rows), c(key_column, ".study"))) {
      for (i in seq_len(nrow(rows))[-1]) {
        if (!identical(rows[[field]][i], rows[[field]][1])) {
          union_record_diff(diffs, glue("{label} {field}"), glue("{entity_key}.{rows[[key_column]][1]} ({rows$.study[i]})"))
          break
        }
      }
    }
  }
  all_rows %>% distinct(.data[[key_column]], .keep_all = TRUE) %>% select(-.study)
}

#' Builds the union of one entity's sources (a named list, by study name).
#' @keywords internal
union_entity <- function(sources, key, diffs, add_source_metadata) {
  first <- sources[[1]]

  for (field in c("display_name", "display_name_plural", "description")) {
    values <- map(sources, ~ slot(.x, field))
    for (study_name in names(sources)[-1]) {
      if (!identical(values[[study_name]], values[[1]])) {
        union_record_diff(diffs, glue("entity {field}"), glue("{key} ({study_name})"))
      }
    }
  }

  all_variables <- bind_rows(map(sources, ~ .x@variables), .id = ".study")
  variable_order <- unique(all_variables$variable)
  variables <- map(variable_order, function(v) {
    union_variable_row(all_variables %>% filter(variable == v), key, diffs)
  }) %>% bind_rows() %>% select(-.study)

  numeric_columns <- variables %>% filter(data_type == "number") %>% pull(variable)
  data <- map(sources, ~ .x@data %>%
    mutate(across(any_of(numeric_columns) & where(is.integer), as.numeric))) %>%
    bind_rows()

  if (add_source_metadata) {
    variables <- bind_rows(variables, union_source_variable_metadata(get_entity_name(first)))
  }

  collections <- union_merge_by_first(map(sources, ~ .x@collections), "category", "collection", key, diffs)

  first %>% initialize(
    data = data,
    variables = variables,
    collections = collections,
    children = list()
  )
}

union_source_variable_metadata <- function(entity_name) {
  tibble(variable = "source_dataset") %>%
    expand_grid(variable_metadata_defaults) %>%
    mutate(
      entity_name = entity_name,
      provider_label = list(list("source_dataset")),
      display_name = "Source dataset",
      data_type = factor("string", levels = levels(variable_metadata_defaults$data_type)),
      data_shape = factor("categorical", levels = levels(variable_metadata_defaults$data_shape)),
      stable_id = NA_character_
    ) %>%
    generate_variable_stable_ids()
}
