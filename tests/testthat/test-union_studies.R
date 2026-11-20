# Small studies are built in code, written to STF and read back, so union_studies()
# sees what study_from_stf() produces.

stf_round_trip <- function(study) {
  dir <- withr::local_tempdir(.local_envir = parent.frame(2))
  study %>% export_to_stf(dir)
  study_from_stf(dir) %>% quiet()
}

make_union_source <- function(name, sites, collections, assays = NULL) {
  site <- entity_from_tibble(sites, name = "site", quiet = TRUE) %>%
    redetect_columns_as_variables(setdiff(colnames(sites), "site")) %>%
    set_variable_display_names_from_provider_labels()
  collection <- entity_from_tibble(collections, name = "collection", quiet = TRUE) %>%
    redetect_columns_as_variables(setdiff(colnames(collections), c("site", "collection"))) %>%
    set_parents(names = "site", id_columns = "site") %>%
    set_variable_display_names_from_provider_labels()
  entities <- list(site, collection)
  if (!is.null(assays)) {
    assay <- entity_from_tibble(assays, name = "assay", quiet = TRUE) %>%
      redetect_columns_as_variables(setdiff(colnames(assays), c("site", "collection", "assay"))) %>%
      set_parents(names = c("collection", "site"), id_columns = c("collection", "site")) %>%
      set_variable_display_names_from_provider_labels()
    entities <- c(entities, list(assay))
  }
  study_from_entities(entities, name = name, stable_id = name) %>%
    quiet() %>%
    stf_round_trip()
}

sites_a <- tibble(site = c("S1", "S2", "S3"), region = c("north", "south", "north"))
sites_b <- tibble(site = c("S4", "S5", "S6"), region = c("south", "south", "north"))
collections_a <- tibble(
  site = c("S1", "S1", "S2", "S3"),
  collection = c("C1", "C2", "C3", "C4"),
  count = c(3L, 5L, 7L, 3L),
  caught = c("yes", "no", "yes", "yes")
)
collections_b <- tibble(
  site = c("S4", "S5", "S6", "S6"),
  collection = c("C5", "C6", "C7", "C8"),
  count = c(2L, 4L, 2L, 9L),
  caught = c("no", "no", "yes", "no")
)

entity_rows <- function(study, entity_name) nrow(get_entity(study, entity_name) %>% get_data())

variable_meta <- function(study, entity_name, variable_name) {
  get_entity(study, entity_name) %>%
    get_variable_metadata() %>%
    filter(variable == variable_name)
}

test_that("studies with identical graphs union their rows and validate", {
  a <- make_union_source("A", sites_a, collections_a)
  b <- make_union_source("B", sites_b, collections_b)

  expect_no_warning(u <- union_studies(list(a, b), name = "AB", stable_id = "AB"))

  expect_equal(get_study_name(u), "AB")
  expect_equal(u@stable_id, "AB")
  expect_equal(get_entity_names(u), c("site", "collection"))
  expect_equal(entity_rows(u, "site"), 6)
  expect_equal(entity_rows(u, "collection"), 8)
  expect_equal(get_entity(u, "site") %>% get_data() %>% pull(site), c(sites_a$site, sites_b$site))

  source <- get_entity(u, "collection") %>% get_data() %>% count(source_dataset)
  expect_equal(source$source_dataset, c("A", "B"))
  expect_equal(source$n, c(4L, 4L))
  meta <- variable_meta(u, "collection", "source_dataset")
  expect_equal(meta$display_name, "Source dataset")
  expect_equal(as.character(meta$data_type), "string")
  expect_match(meta$stable_id, "^VAR_")
  expect_false("source_dataset" %in% colnames(get_entity(u, "site") %>% get_data()))

  expect_true(u %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("source_variable = FALSE adds no variable", {
  a <- make_union_source("A", sites_a, collections_a)
  b <- make_union_source("B", sites_b, collections_b)
  u <- union_studies(list(a, b), name = "AB", stable_id = "AB", source_variable = FALSE)
  expect_false("source_dataset" %in% colnames(get_entity(u, "collection") %>% get_data()))
})

test_that("an entity only some studies have is kept with their rows", {
  assays_a <- tibble(
    site = c("S1", "S2"), collection = c("C1", "C3"), assay = c("A1", "A2"),
    result = c("positive", "negative")
  )
  a <- make_union_source("A", sites_a, collections_a, assays_a)
  b <- make_union_source("B", sites_b, collections_b)

  u <- union_studies(list(a, b), name = "AB", stable_id = "AB")

  expect_equal(get_entity_names(u), c("site", "collection", "assay"))
  expect_equal(entity_rows(u, "assay"), 2)
  expect_equal(entity_rows(u, "collection"), 8)
  expect_true(u %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("binary/categorical and integer/number differences widen with a warning", {
  a <- make_union_source("A", sites_a, collections_a)
  collections_b_wider <- collections_b %>%
    mutate(count = c(2.5, 4, 2, 9.5), caught = c("no", "maybe", "yes", "no"))
  b <- make_union_source("B", sites_b, collections_b_wider)
  expect_equal(as.character(variable_meta(a, "collection", "caught")$data_shape), "binary")
  expect_equal(as.character(variable_meta(b, "collection", "count")$data_type), "number")

  warnings <- character()
  u <- withCallingHandlers(
    union_studies(list(a, b), name = "AB", stable_id = "AB"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true("union_studies: widened data_shape to 'categorical' for: collection.caught" %in% warnings)
  expect_true("union_studies: widened data_type to 'number' for: collection.count" %in% warnings)
  expect_length(warnings, 2)
  expect_equal(as.character(variable_meta(u, "collection", "caught")$data_shape), "categorical")
  expect_equal(as.character(variable_meta(u, "collection", "count")$data_type), "number")
  counts <- get_entity(u, "collection") %>% get_data() %>% pull(count)
  expect_type(counts, "double")
  expect_equal(counts, c(3, 5, 7, 3, 2.5, 4, 2, 9.5))
  vocabulary <- get_entity(u, "collection") %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "caught") %>%
    pull(vocabulary) %>%
    unlist()
  expect_setequal(vocabulary, c("yes", "no", "maybe"))
  expect_true(u %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("differing vocabulary_order lists are merged in study order without a warning", {
  a <- make_union_source("A", sites_a, collections_a)
  b <- make_union_source("B", sites_b, collections_b)
  a@root_entity <- a@root_entity %>% quiet() %>% set_variable_metadata("region", vocabulary_order = list("south", "north"))
  b@root_entity <- b@root_entity %>% quiet() %>% set_variable_metadata("region", vocabulary_order = list("north"))

  expect_no_warning(u <- union_studies(list(a, b), name = "AB", stable_id = "AB"))

  expect_equal(variable_meta(u, "site", "region")$vocabulary_order[[1]], list("south", "north"))
})

test_that("a date/string difference is an error naming the variable and values", {
  a <- make_union_source("A", sites_a, collections_a %>% mutate(when = as.Date("2020-01-01") + 0:3))
  b <- make_union_source("B", sites_b, collections_b %>% mutate(when = c("spring", "summer", "spring", "autumn")))

  expect_error(
    union_studies(list(a, b), name = "AB", stable_id = "AB"),
    "variable 'when' in entity 'collection' has conflicting data_type: A = date, B = string",
    fixed = TRUE
  )
})

test_that("an entity with different parents is an error", {
  a <- make_union_source("A", sites_a, collections_a)
  collection_root <- entity_from_tibble(collections_b %>% select(-site), name = "collection", quiet = TRUE) %>%
    redetect_columns_as_variables(c("count", "caught")) %>%
    set_variable_display_names_from_provider_labels()
  b <- study_from_entities(list(collection_root), name = "B", stable_id = "B") %>% quiet() %>% stf_round_trip()

  expect_error(union_studies(list(a, b), name = "AB", stable_id = "AB"), "entity 'collection'")
})

test_that("colliding ids are an error unless prefix_ids = TRUE", {
  a <- make_union_source("A", sites_a, collections_a)
  sites_b_overlap <- sites_b %>% mutate(site = c("S1", "S5", "S6"))
  b <- make_union_source("B", sites_b_overlap, collections_b %>% mutate(site = c("S1", "S5", "S6", "S6")))

  expect_error(
    union_studies(list(a, b), name = "AB", stable_id = "AB"),
    "entity 'site' has 1 id(s) in both 'A' and 'B', e.g. S1",
    fixed = TRUE
  )

  u <- union_studies(list(a, b), name = "AB", stable_id = "AB", prefix_ids = TRUE)
  expect_equal(get_entity(u, "site") %>% get_data() %>% pull(site),
               c("A:S1", "A:S2", "A:S3", "B:S1", "B:S5", "B:S6"))
  collection_data <- get_entity(u, "collection") %>% get_data()
  expect_equal(collection_data$site, c("A:S1", "A:S1", "A:S2", "A:S3", "B:S1", "B:S5", "B:S6", "B:S6"))
  expect_equal(collection_data$collection[1], "A:C1")
  expect_true(u %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("a union survives an export_to_stf() / study_from_stf() round trip", {
  a <- make_union_source("A", sites_a, collections_a)
  b <- make_union_source("B", sites_b, collections_b)
  u <- union_studies(list(a, b), name = "AB", stable_id = "AB")
  source_stable_id <- variable_meta(u, "collection", "source_dataset")$stable_id

  dir <- withr::local_tempdir()
  u %>% quiet() %>% export_to_stf(dir)
  back <- study_from_stf(dir) %>% quiet()

  expect_equal(get_study_name(back), "AB")
  expect_equal(entity_rows(back, "site"), 6)
  expect_equal(entity_rows(back, "collection"), 8)
  expect_equal(variable_meta(back, "collection", "source_dataset")$stable_id, source_stable_id)
  expect_equal(get_entity(back, "collection") %>% get_data() %>% pull(source_dataset),
               rep(c("A", "B"), each = 4))
  expect_true(back %>% validate(profiles = c("baseline", "eda")))
})
