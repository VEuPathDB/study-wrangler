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
  
  # it should validate
  expect_true(
    households %>% quiet() %>% validate()
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
  
  # make a manual mistake in the variable graph and pay the price
  expect_message(
    expect_false(
      households %>%
        quiet() %>%
        set_variable_metadata('Number.of.animals', parent_variable = 'nonexistent') %>%
        verbose() %>%
        validate()
    ),
    "These variables or categories have 'parent_variable' values that do not exist.+Number.of.animals"
  )
  
  # make a circular graph and also pay the price
  expect_message(
    expect_false(
      households %>%
        quiet() %>%
        set_variable_metadata('house_vars', parent_variable = 'Owns.property') %>%
        verbose() %>%
        validate()
    ),
    "Illegal circular path detected in the parent_variable -> variable graph."
  )

  # add a category-of-categories
  expect_message(
    households <- households %>%
      create_variable_category(
        category_name = "vars",
        children = c("house_vars")
      ),
    "Successfully created category 'vars'"
  )
  
  # deleting an "internal" category isn't allowed
  expect_error(
    households <- households %>%
      delete_variable_category("house_vars"),
    "Category 'house_vars' cannot be deleted because it belongs to another category."
  )
  
  # deleting them parent-most first is OK
  expect_message(
    households <- households %>%
      delete_variable_category("vars"),
    "Category 'vars' has been deleted."
  )
  expect_message(
    households <- households %>%
      delete_variable_category("house_vars"),
    "Category 'house_vars' has been deleted."
  )
  
  
})
