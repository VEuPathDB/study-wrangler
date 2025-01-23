test_that("set_variable_ordinal_levels", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(households %>% quiet() %>% validate())
  
  expect_no_error(
    expect_message(
      households <- households %>%
        set_variable_ordinal_levels(
          'Owns.property',
          c("Yes", "No", "Maybe") # extra level not seen in data is allowed
        ),
      "Successfully set 'Owns.property' as an ordinal variable with levels: Yes, No, Maybe"
    )
  )
  
  expect_true(households %>% quiet() %>% validate())
  
  expect_error(
    households <- households %>%
      set_variable_ordinal_levels(
        'Owns.property',
        c("Yes", "Maybe") # leaving out levels in the data is not allowed
      ),
    "The levels you provide must include all the observed levels in the data.+must also include these levels: No"
  )
  
  # vocabulary in hydrated metadata must equal the levels.
  my_levels <- c("Yes", "No", "Maybe")
  expect_equal(
    households %>%
      quiet() %>%
      set_variable_ordinal_levels('Owns.property', levels = my_levels) %>%
      get_hydrated_variable_metadata() %>%
      filter(variable == 'Owns.property') %>%
      pull(vocabulary) %>%
      unlist(),
    my_levels
  )

  # check this also works for number vars
  # but note that factor levels are always character vectors, so we need
  # the `as.character()` fudge at the end.
  num_levels <- 1:5
  expect_equal(
    households %>%
      quiet() %>%
      set_variable_ordinal_levels('Number.of.animals', levels = num_levels) %>%
      get_hydrated_variable_metadata() %>%
      filter(variable == 'Number.of.animals') %>%
      pull(vocabulary) %>%
      unlist(),
    as.character(num_levels)
  )
  
  # a half-baked ordinal should not validate
  expect_false(
    households %>% quiet() %>%
      set_variable_metadata('Number.of.animals', data_shape = 'ordinal') %>%
      validate()
  )

  # same here:
  expect_false(
    households %>% quiet() %>%
      set_variable_metadata('Number.of.animals', ordinal_levels = as.list(1:5)) %>%
      validate()
  )
  
  # check that a properly constructed ordinal number var validates!
  expect_message(
    households <- households %>%
      set_variable_ordinal_levels('Number.of.animals', levels = num_levels),
    "Successfully set 'Number.of.animals' as an ordinal variable with levels: 1, 2, 3, 4, 5"
  )
  expect_true(households %>% quiet() %>% validate())
  
})