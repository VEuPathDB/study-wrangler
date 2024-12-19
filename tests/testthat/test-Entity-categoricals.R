test_that("validate(households) alerts to categorical variables with non-factor data columns", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(households %>% quiet() %>% validate())
  
  # mutate a column naively (not wrapped in `factor()`)
  households <- households %>%
    modify_data(
      mutate(Owns.property = if_else(Owns.property == 'Yes', 'Sure thing', 'Not really'))
    )
  # validate again
  expect_message(
    expect_false(validate(households)),
    "Categorical column.+are not R factors.+Owns.property"
  )

  # now mutate it back to a factor
  households <- households %>%
    modify_data(
      mutate(Owns.property = factor(Owns.property))
    )
  # should now be valid
  expect_true(households %>% quiet() %>% validate())
})

test_that("Using forcats manipulations on categoricals does not break validation", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(households %>% quiet() %>% validate())

  households <- households %>%
    modify_data(
      mutate(Owns.property = fct_expand(Owns.property, "It's complicated"))
    )  
  expect_true(households %>% quiet() %>% validate())
  expect_output(
    inspect_variable(households, "Owns.property"),
    "It's complicated"
  )
})

test_that("Even very large vocabularies are reported in inspect_variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name="household")
  expect_true(households %>% quiet() %>% validate())

  # add new factor levels, "aaa":"zzz"
  households <- households %>%
    modify_data(
      mutate(Owns.property = fct_expand(Owns.property, paste0(letters, letters, letters)))
    )
  expect_true(households %>% quiet() %>% validate())
  expect_output(
    inspect_variable(households, "Owns.property"),
    "zzz"
  )
})
