test_that("units are properly inpsected and validated", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  #households <- study %>% get_root_entity() %>% verbose()
  #participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()
  
  # check that general inspect has a units section
  expect_output(
    observations %>% quiet() %>% inspect(),
    "Summary of units.+Height..cm.+Teeth.brushed.today.+None of the variables above have units"
  )
  
  # now we'll add a unit
  expect_message(
    observations <- observations %>% set_variable_metadata('Weight..kg.', unit = 'kg'),
    "Made metadata update.+unit"
  )
  
  # check that inspect displays this
  expect_output(
    observations %>% quiet() %>% inspect(),
    "Weight..kg.+kg"
  )
  
  # should validate
  expect_true(
    observations %>% quiet() %>% validate()
  )

  # now we add a unit to something which shouldn't have a unit
  # this will go ahead without issue
  expect_message(
    observations <- observations %>% set_variable_metadata('Observation.date', unit = 'kg'),
    "Made metadata update.+unit"
  )
  
  # but it shouldn't validate
  expect_message(
    expect_false(
      observations %>% validate()
    ),
    "These non-numeric variables should not have units: Observation.date"
  )
    
})
  