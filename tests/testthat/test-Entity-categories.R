test_that("create_variable_category() works", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  participants <- study %>% get_entity('participant') %>% verbose()
  observations <- study %>% get_entity('observation') %>% verbose()
  
  expect_message(
    expect_message(
      households <- households %>%
        create_variable_category(
          category_name = "house_vars",
          children = c("Owns.property", "Construction.material"),
          display_name = "House-related",
          definition = "Things about the house"
        ),
      "Successfully created category 'house_vars'"
    ),
    "Made metadata update.s. to 'display_name', 'definition' for 'house_vars'"
  )
  
  # inspect should still work
  expect_no_error(
    output <- capture_output(
      households %>% quiet() %>% inspect()
    )
  )
  
  # to do: implement list of children and test properly
  expect_no_message(
    households %>% quiet() %>% inspect('house_vars')
  )
  
  # to do: check that you can't overwrite a variable or category
  
  
  
  
})