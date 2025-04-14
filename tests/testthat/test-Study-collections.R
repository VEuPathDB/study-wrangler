test_that("create_variable_collection works", {
  study_name <- 'my collections study'
  expect_no_error(
    study <- make_study(name = study_name)
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()

  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(
          category_name = "integer.measures",
          children = c("Height..cm.", "Weight..kg."),
          display_name = "integer-based anatomical measures",
          definition = "integer-based anatomical measures"
        ),
      "Successfully created category 'integer.measures'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'integer.measures'"
  )
  
  # it should validate
  expect_true(
    observations %>% quiet() %>% validate()
  )
  
  expect_no_error(
    study <- study_from_entities(list(households, participants, observations), name = study_name)
  )
  
  observations <- study %>% get_entity('observation') %>% verbose()
  
  collection_spec = list(
    entity = "observation",
    category = "integer.measures",
    member = "gene",
    memberPlural = "genes",
    label = "raw read count",
    isProportion = FALSE,
    isCompositional = FALSE,
    normalizationMethod = "none"
  )
  
  # Note that the !!!syntax won't work in the console unless it's wrapped
  # in an expect_* function or similar
  expect_no_error(
    study <- study %>% create_variable_collection(!!!collection_spec)
  )
  expect_error(
    study <- study %>% create_variable_collection(!!!collection_spec),
    "variable collection 'integer.measures' for entity 'observation' already exists"
  )
  
  expect_error(
    study <- study %>% delete_variable_collection(entity = "nonentity", category = "integer.measures"),
    "variable collection 'integer.measures' for entity 'nonentity' not found"
  )
  
  expect_no_error(
    study <- study %>% delete_variable_collection(entity = "observation", category = "integer.measures")
  )

  # to do, test basic create-time validation
  bad_collection_spec <- list_assign(collection_spec, entity = "nonentity")
  
})
