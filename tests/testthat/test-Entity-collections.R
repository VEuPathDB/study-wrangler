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

#'
#' check that collection metadata is correctly "hydrated"
#'
test_that("collection metadata is correctly summarised from child variables", {
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
        set_variable_metadata('MUAC..cm.', unit='cm', display_name='MUAC')
    )
  )
  
  centimeter_category_spec <- list(
    category_name = "centimeter_vars",
    children = c("Height..cm.", "MUAC..cm."),
    display_name = "centimeter-based anatomical measures",
    definition = "centimeter-based anatomical measures"
  )
  
  expect_message(
    expect_message(
      observations <- observations %>%
        create_variable_category(!!!centimeter_category_spec),
      "Successfully created category 'centimeter_vars'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'centimeter_vars'"
  )
  
  # it should validate
  expect_true(
    observations %>% quiet() %>% validate()
  )
  
  # create a collection for this category
  observations <- observations %>%
    create_variable_collection(
      'centimeter_vars',
      member = 'measurement',
      member_plural = 'measurements'
    )
  
  # it should not validate because the data_type is not uniform
  expect_warning(
    expect_false(
      observations %>% validate()
    ),
    "One or more variable collections were heterogeneous for metadata fields that should be uniform.+centimeter_vars.+integer, number"
  )
  
  # fix the data_type of one of the child vars
  expect_message(
    observations <- observations %>% set_variable_metadata('Height..cm.', data_type = 'number')
  )
  
  # it should now validate
  expect_true(
    observations %>% quiet() %>% validate()
  )

  suppressMessages( # sometimes "Generating temporary stable_id for entity 'observation'"
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  
  expect_equal(
    hydrated_collections %>% pull('range_min'),
    '12.59'
  )
  expect_equal(
    hydrated_collections %>% pull('range_max'),
    '175'
  )
  
  ## now let's see how display_range_min and max work
  # first let's set it on one of the variables
  expect_message(
    observations <- observations %>% set_variable_metadata('Height..cm.', display_range_min = "1", display_range_max = "200")
  )
  suppressMessages(
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_min'),
    '1'
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_max'),
    '200'
  )

  # now let's set it on another variable, it should take the widest range
  expect_message(
    observations <- observations %>% set_variable_metadata('MUAC..cm.', display_range_min = "0", display_range_max = "500")
  )
  suppressMessages(
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_min'),
    '0'
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_max'),
    '500'
  )

  # now let's set a display_range on the collection directly, inside those ranges
  expect_message(
    observations <- observations %>% set_collection_metadata('centimeter_vars', display_range_min = "10", display_range_max = "100")
  )
  # the final range should still be 0-500
  suppressMessages(
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_min'),
    '0'
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_max'),
    '500'
  )
  
  # now let's set an even wider range on the collection
  expect_message(
    observations <- observations %>% set_collection_metadata('centimeter_vars', display_range_min = "-1000", display_range_max = "1000")
  )
  suppressMessages(
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_min'),
    '-1000'
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_max'),
    '1000'
  )
  
  # now remove the per-variable display_ranges
  expect_message(
    observations <- observations %>% set_variable_metadata('MUAC..cm.', display_range_min = NA, display_range_max = NA)
  )
  expect_message(
    observations <- observations %>% set_variable_metadata('Height..cm.', display_range_min = NA, display_range_max = NA)
  )
  # and set a collection display_range that's actually inside the actual data range
  # https://www.youtube.com/watch?v=O9IJnmbneLc :-)
  expect_message(
    observations <- observations %>% set_collection_metadata('centimeter_vars', display_range_min = "40", display_range_max = "45")
  )
  suppressMessages(
    hydrated_collections <- observations %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_min'),
    '40'
  )
  expect_equal(
    hydrated_collections %>% pull('display_range_max'),
    '45'
  )
})

test_that("collections of date variables work", {
  study_name <- "my collections study"
  expect_no_error(
    study <- make_study(name = study_name)
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  
  # 1. Add two date variables (Build.date and Renovation.date) to households,
  #    then sync their metadata so they appear in metadata tables.
  expect_message(
    households <- households %>%
      modify_data(
        mutate(
          Build.date      = as.Date(c("1990-01-02", "1998-12-20", "2003-04-04")),
          Renovation.date = as.Date(c("2000-05-05", NA, "2002-07-07"))
        )
      ) %>%
      sync_variable_metadata(),
    "Synced variables metadata by adding defaults for: Build.date, Renovation.date"
  )
  
  # 2. Create a variable category that includes both date fields
  expect_message(
    expect_message(
      households <- households %>%
        create_variable_category(
          category_name = "house_dates",
          children      = c("Build.date", "Renovation.date"),
          display_name  = "House dates",
          definition    = "Dates when the house was built and renovated"
        ),
      "Successfully created category 'house_dates'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'house_dates'"
  )
  
  # 3. Create a collection for that category
  households <- households %>%
    create_variable_collection(
      "house_dates",
      member        = "event_date",
      member_plural = "event_dates"
    )
  
  # 4. Before any overrides, get_hydrated_collection_metadata() should pick
  #    the true min and max of the two date fields:
  suppressMessages(
    hydrated_dates <- households %>% get_hydrated_collection_metadata()
  )
  expect_equal(
    hydrated_dates %>% pull("range_min"),
    "1990-01-02"
  )
  expect_equal(
    hydrated_dates %>% pull("range_max"),
    "2003-04-04"
  )
  
  # 5. Now set a display_range on one of the variables (Build.date)
  expect_message(
    households <- households %>%
      set_variable_metadata(
        "Build.date",
        display_range_min = "1990-01-01",
        display_range_max = "2003-12-31"
      )
  )
  suppressMessages(
    hydrated_dates <- households %>% get_hydrated_collection_metadata()
  )
  # Since Renovation.date has no display_range override, the collection's display_range
  # comes from the widest of: Build.date (1991–2000) and Renovation.date (2000–2002).
  expect_equal(
    hydrated_dates %>% pull("display_range_min"),
    "1990-01-01"
  )
  expect_equal(
    hydrated_dates %>% pull("display_range_max"),
    "2003-12-31"
  )
  
  # 6. Next, override Renovation.date's display_range to be narrower than Build.date's
  expect_message(
    households <- households %>%
      set_variable_metadata(
        "Renovation.date",
        display_range_min = "2001-06-01",
        display_range_max = "2001-06-30"
      )
  )
  suppressMessages(
    hydrated_dates <- households %>% get_hydrated_collection_metadata()
  )
  # Now the widest display_range among the two variables is:
  #   Build.date override:      1991-01-01 … 2000-12-31
  #   Renovation.date override: 2001-06-01 … 2001-06-30
  # So the collection display_range_min should be 1991-01-01 and max 2001-06-30.
  expect_equal(
    hydrated_dates %>% pull("display_range_min"),
    "1990-01-01"
  )
  expect_equal(
    hydrated_dates %>% pull("display_range_max"),
    "2003-12-31"
  )
  
  # 7. Now set a display_range directly on the collection that is INSIDE the actual data range
  #    (which is 1990-01-02 … 2003-04-04). Using 1995-01-01 … 2002-12-31 should be allowed:
  expect_message(
    households <- households %>%
      set_collection_metadata(
        "house_dates",
        display_range_min = "1995-01-01",
        display_range_max = "2002-12-31"
      )
  )
  suppressMessages(
    hydrated_dates <- households %>% get_hydrated_collection_metadata()
  )
  # Because the collection range 1995–2002 is inside 1990–2003, it has no effect:
  expect_equal(
    hydrated_dates %>% pull("display_range_min"),
    "1990-01-01"
  )
  expect_equal(
    hydrated_dates %>% pull("display_range_max"),
    "2003-12-31"
  )

  # 8. Next, remove the per-variable display ranges
  expect_message(
    expect_message(
      households <- households %>%
        set_variable_metadata(
          "Build.date",
          display_range_min = NA,
          display_range_max = NA
        ) %>%
        set_variable_metadata(
          "Renovation.date",
          display_range_min = NA,
          display_range_max = NA
        )
    )
  )

  suppressMessages(
    hydrated_dates <- households %>% get_hydrated_collection_metadata()
  )
  # Because 1980–2010 exceeds the true data bounds (1990–2003), these new values should be used:
  expect_equal(
    hydrated_dates %>% pull("display_range_min"),
    "1995-01-01"
  )
  expect_equal(
    hydrated_dates %>% pull("display_range_max"),
    "2002-12-31"
  )
  
})
