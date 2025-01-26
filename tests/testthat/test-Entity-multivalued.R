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
