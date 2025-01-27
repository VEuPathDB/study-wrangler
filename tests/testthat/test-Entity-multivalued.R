# first use regular single-valued data
test_that("set_variables_multivalued() and set_variables_univalued() work", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  #participants <- study %>% get_entity('participant') %>% verbose()
  #observations <- study %>% get_entity('observation') %>% verbose()
  
  expect_message(
    households <- households %>%
      set_variables_multivalued('Construction.material' = ';'),
    "Successfully marked the following variables as multi-valued: Construction.material"
  )  
  
  # not allowed to make ordinal variables multi-valued
  expect_error(
    households %>%
      quiet() %>%
      set_variable_ordinal_levels("Owns.property", levels=c("Yes","No")) %>%
      set_variables_multivalued("Owns.property" = ';'),
    "The following variables cannot be multi-valued because they are ordinal: Owns.property"
  )
  
  expect_message(
    households <- households %>%
      set_variables_univalued(c("Construction.material")),
    "Successfully marked the following variables as uni-valued: Construction.material"
  )
  
  # Verify that the variable is no longer multivalued
  expect_false(households@variables %>%
                 filter(variable == "Construction.material") %>%
                 pull(is_multi_valued))
  
  
})


# now we use a new households file with actual delimited multiple values
test_that("Actual multi-valued files can be processed", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/householdsMultiValued.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')

  # It won't validate because Distances.to.well is detected as an ID column (all unique, no NAs)
  expect_message(
    expect_false(
      households %>% validate()
    ),
    "There are multiple ID columns per entity level.+Household.Id, Distances.to.well"
  )
  
  # let's fix that.
  expect_message(
    households <- households %>% redetect_columns_as_variables(columns = c('Distances.to.well')),
    "Redoing type detection"
  )

  # should be good now
  expect_true(
    households %>% quiet() %>% validate()
  )
    
})
