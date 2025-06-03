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
    member_plural = "genes",
    label = "raw read count",
    is_proportion = FALSE,
    is_compositional = FALSE,
    normalization_method = "none"
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

#'
#' non-uniform data_shape, data_type, unit, impute_zero should all fail
#'
test_that("required fields are provided and heterogeneous collections do not validate", {
  study_name <- 'my collections study'
  expect_no_error(
    study <- make_study(name = study_name)
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()
  
  # set the units metadata for the underlying variables
  expect_message(
    expect_message(
      observations <- observations %>%
      set_variable_metadata('Height..cm.', unit='cm', display_name='Height') %>%
      set_variable_metadata('Weight..kg.', unit='kg', display_name='Weight')
    )
  )
  
  mixed_unit_category_spec <- list(
    category_name = "mixed_units",
    children = c("Height..cm.", "Weight..kg."),
    display_name = "integer-based anatomical measures",
    definition = "integer-based anatomical measures"
  )
  
  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(!!!mixed_unit_category_spec),
      "Successfully created category 'mixed_units'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'mixed_units'"
  )
  
  # it should validate
  expect_true(
    observations %>% quiet() %>% validate()
  )
  
  expect_silent(
    observations <- observations %>%
      create_variable_collection('mixed_units')
  )
  
  # it should not validate because member and member_plural are missing
  expect_warning(
    observations %>% validate(),
    "Required metadata fields were missing in the following collections.+member.+mixed_units.+member_plural.+mixed_units"
  )

  # add a wrong'un to test set_collection_metadata
  expect_error(
    observations <- observations %>%
      set_collection_metadata('mixed_units', member_oops = 'thingy', member_plural = 'thingies'),
    "invalid field.+member_oops"
  )
  
  # fix up the collection metadata properly
  expect_message(
    observations <- observations %>%
      set_collection_metadata('mixed_units', member = 'thingy', member_plural = 'thingies'),
    "Made metadata update.+member.+member_plural.+mixed_units"
  )

  # but now it should not validate because of the mixed units
  expect_warning(
    expect_false(
      observations %>% validate()
    ),
    "One or more variable collections were heterogeneous for metadata fields that should be uniform.+mixed_units.+cm,\\s*kg"
  )
  
  
})
