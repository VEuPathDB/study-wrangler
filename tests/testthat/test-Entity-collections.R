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
    observations2 <- observations %>% create_variable_collection('integer.measures', !!!collection_spec)
  )
  expect_error(
    observations2 <- observations2 %>% create_variable_collection('integer.measures', !!!collection_spec),
    "variable collection 'integer.measures' already exists in entity"
  )

  expect_error(
    observations2 %>% delete_variable_collection("no.measures"),
    "variable collection 'no.measures' not found and therefore not deleted"
  )

  expect_no_error(
    observations2 %>% delete_variable_collection("integer.measures")
  )

  expect_error(
    observations %>% create_variable_collection('desperate.measures', !!!collection_spec),
    "variable collection cannot be added because category 'desperate.measures' does not exist in entity"
  )
  
  # check that the label-fallback-to-category-display_name functionality works
  no_label_spec <- list_modify(collection_spec, label = zap())
  expect_no_error(
    expect_true(
      observations %>%
        create_variable_collection('integer.measures', !!!no_label_spec) %>%
        slot('collections') %>% # we'll probably provide convenience collection getter methods at some point...
        filter(category %in% 'integer.measures', label %in% 'integer-based anatomical measures') %>%
        nrow() == 1
    )
  )
  
  # let's delete the variable category from observations2 and check
  # that the entity no longer validates (orphan collection)
  expect_message(
    observations3 <- observations2 %>% delete_variable_category("integer.measures"),
    "Category 'integer.measures' has been deleted"
  )
  expect_warning(
    observations3 %>% validate(),
    "These variable collections have no corresponding variable category: integer.measures"
  )
  
  
  # and add a category with a different name
  desperate.measures_category_spec <- list_assign(integer.measures.category_spec, category_name = "desperate.measures")
  expect_message(
    expect_message(
      observations3 <- observations3 %>% create_variable_category(!!!desperate.measures_category_spec)
    )
  )
  expect_warning(
    observations3 %>% validate(),
    "These variable collections have no corresponding variable category: integer.measures"
  )
  
  # Are collections shown in the inspect(study) overview?
  # first make a study with observations2 (this still has category and collections)
  expect_no_error(
    study2 <- study_from_entities(list(households, participants, observations2), name = study_name)
  )
  expect_true(
    study2 %>% quiet() %>% validate()
  )
  output <- capture.output(inspect(study2))

  # household: 0 collections
  patt_household <- r"(^household\s+household\s+households\s+\d+\s+TRUE\s+0$)"
  expect_true(any(grepl(patt_household, output, perl = TRUE)))
  
  # participant: 0 collections
  patt_participant <- r"(^participant\s+participant\s+participants\s+\d+\s+TRUE\s+0$)"
  expect_true(any(grepl(patt_participant, output, perl = TRUE)))
  
  # observation: 1 collection
  patt_observation <- r"(^observation\s+observation\s+observations\s+\d+\s+TRUE\s+1$)"
  expect_true(any(grepl(patt_observation, output, perl = TRUE)))  
})
