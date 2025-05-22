test_that("create_variable_collection works", {
  study_name <- 'my collections study'
  expect_no_error(
    study <- make_study(name = study_name)
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()

  integer.measures.category_spec <- list(
    category_name = "integer.measures",
    children = c("Height..cm.", "Weight..kg."),
    display_name = "integer-based anatomical measures",
    definition = "integer-based anatomical measures"
  )
  
  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(!!!integer.measures.category_spec),
      "Successfully created category 'integer.measures'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'integer.measures'"
  )
  
  # it should validate
  expect_true(
    observations %>% quiet() %>% validate()
  )

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

  # let's delete the variable category from observations2 and check
  # that the entity no longer validates (orphan collection)
  expect_message(
    observations2 <- observations2 %>% delete_variable_category("integer.measures"),
    "Category 'integer.measures' has been deleted"
  )
  # and category with a different name
  desperate.measures_category_spec <- list_assign(integer.measures.category_spec, category_name = "desperate.measures")
  expect_message(
    expect_message(
      observations2 <- observations2 %>% create_variable_category(!!!desperate.measures_category_spec)
    )
  )
  
  expect_warning(
    observations2 %>% validate(),
    "These variable collections have no corresponding variable category: integer.measures"
  )
  
  # check that a missing 'update' fails
  no_member_spec <- list_modify(collection_spec, member = zap())
  expect_error(
    observations2 <- observations %>% create_variable_collection(!!!no_member_spec),
    "missing field.+member"
  )

  # TO DO: make study with entities with collections?  Only needed if we display
  # collections in the inspect(study) overview
#  expect_no_error(
#    study <- study_from_entities(list(households, participants, observations), name = study_name)
#  )
  
})
