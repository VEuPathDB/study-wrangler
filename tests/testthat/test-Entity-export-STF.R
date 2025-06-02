# most STF testing is from a Study object but here we test the standalone entity file handling

expect_roundtrip <- function(entity, output_dir) {
  expect_silent(
    entity %>% export_entity_to_stf(output_dir)
  )
  tsv_path <- glue("{output_dir}/entity-{get_entity_name(entity)}.tsv")
  expect_silent(
    entity2 <- entity_from_stf(tsv_path)
  )
  expect_equal(
    entity2,
    entity
  )
}


test_that("A regular entity roundtrips via STF directory", {
  study_name <- 'my STF study'
  output_dir <- "./tmp/stf-entity"

  expect_no_error(
    study <- make_study(name = study_name)
  )
  observations <- study %>% get_entity('observation') %>% verbose()
  expect_true(observations %>% quiet() %>% validate())

  expect_roundtrip(observations, output_dir)
})

test_that("An entity with collections roundtrips via STF directory", {
  
  study_name <- 'my STF collections study'
  output_dir <- "./tmp/stf-collections-entity"
  
  expect_no_error(
    study <- make_study(name = study_name)
  )
  observations <- study %>% get_entity('observation') %>% verbose()
  
  category_spec <- list(
    category_name = "integer.measures",
    children = c("Height..cm.", "Weight..kg."),
    display_name = "integer-based anatomical measures",
    definition = "integer-based anatomical measures"
  )
  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(!!!category_spec),
      "Successfully created category 'integer.measures'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'integer.measures'"
  )
  
  expect_true(observations %>% quiet() %>% validate())

  collection_spec = list(
    member = "gene",
    member_plural = "genes",
    label = "raw read count",
    is_proportion = FALSE,
    is_compositional = FALSE,
    normalization_method = "none"
  )
  expect_silent(
    observations <- observations %>% create_variable_collection('integer.measures', !!!collection_spec)
  )
  
  expect_roundtrip(observations, output_dir)  
})
