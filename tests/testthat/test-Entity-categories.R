test_that("create_variable_category() works", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  households <- study %>% get_root_entity() %>% verbose()
  #participants <- study %>% get_entity('participant') %>% verbose()
  #observations <- study %>% get_entity('observation') %>% verbose()
  
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
  
  # Check that children are listed in inspect()
  expect_output(
    households %>% quiet() %>% inspect('house_vars'),
    "Children of category"
  )
  
  # Check that you can't overwrite an existing variable, category, or id_column
  for (old_name in list("Number.of.animals", "house_vars", "Household.Id")) {
    expect_error(
      households <- households %>%
        create_variable_category(
          category_name = old_name,
          children = c("Owns.property", "Construction.material"),
          display_name = "House-related",
          definition = "Things about the house"
        ),
      "Category cannot be created because a variable, category or ID column of the same name exists already."
    )
  }
  
  
  
})