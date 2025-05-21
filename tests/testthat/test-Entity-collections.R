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
    observations2 <- observations %>% create_variable_collection(!!!collection_spec)
  )
  expect_error(
    observations2 <- observations2 %>% create_variable_collection(!!!collection_spec),
    "variable collection 'integer.measures' already exists in entity"
  )

  expect_error(
    observations2 <- observations2 %>% delete_variable_collection(category = "nonexistent"),
    "variable collection 'nonexistent' not found and therefore not deleted"
  )

  expect_no_error(
    observations2 <- observations2 %>% delete_variable_collection(category = "integer.measures")
  )

  bad_category_spec <- list_assign(collection_spec, category = "desperate.measures")
  expect_error(
    observations %>% create_variable_collection(!!!bad_category_spec),
    "variable collection cannot be added because category 'desperate.measures' does not exist in entity"
  )
  
  # check that the label-fallback-to-display_name functionality works
  no_label_spec <- list_modify(collection_spec, label = zap())
  expect_no_error(
    observations2 <- observations %>% create_variable_collection(!!!no_label_spec)
  )

  # check that a missing 'update' fails
  no_member_spec <- list_modify(collection_spec, member = zap())
  expect_error(
    observations2 <- observations %>% create_variable_collection(!!!no_member_spec),
    "missing field.+member"
  )
  
})
