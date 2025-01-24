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
  
  
})