test_that("validate(entity) gives correct guidance when there is no ID column", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  expect_no_error(
    households <- entity_from_file(file_path, name = 'household')
  )
  # It should already be valid
  expect_true(
    households %>% quiet() %>% validate()
  )
  
  # now remove the ID column
  expect_message(
    households <- households %>% modify_data(
      select(-c(Household.Id)) 
    ) %>% sync_variable_metadata(),
    "Synced metadata by removing these variables with no data: Household.Id"
  )
  
  # expect it to fail validation and provide a way to fix it
  expect_warning(
    expect_false(
      households %>% validate()
    ),
    "This entity appears to have no ID column.+modify_data.+mutate.+row_number.+validate.+again"
  )
  
  # now we'll fix it
  expect_message(
    expect_message(
      households <- households %>%
        modify_data(mutate(ID = row_number())) %>%
        sync_variable_metadata() %>%
        redetect_column_as_id('ID'),
      "Synced variables metadata by adding defaults for: ID"
    ),
    "Redoing type detection"
  )
  
  # It should now be valid
  expect_true(
    households %>% quiet() %>% validate()
  )

  # check that the entity knows it's an ID column
  expect_equal(
    households %>% get_entity_id_column(),
    "ID"
  )
    
  # and the new ID column should be 1,2,3
  expect_equal(
    households %>% get_data() %>% pull(ID),
    1:nrow(households %>% get_data())
  )

})
