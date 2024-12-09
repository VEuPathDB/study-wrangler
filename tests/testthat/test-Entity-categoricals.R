test_that("validate(households) alerts to categorical variables with non-factor data columns", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(validate(households, quiet=TRUE))
  
  # mutate a column naively (not wrapped in `factor()`)
  households@data <- households@data %>% mutate(Owns.property = if_else(Owns.property == 'Yes', 'Sure thing', 'Not really'))
  # validate again
  expect_message(
    expect_false(validate(households)),
    "Categorical column.+are not R factors.+Owns.property"
  )

  # now mutate it back to a factor
  households@data <- households@data %>% mutate(Owns.property = factor(Owns.property))
  # should now be valid
  expect_true(validate(households, quiet=TRUE))

  # households@data <- households@data %>% mutate(Owns.property = factor(if_else(Owns.property == 'Yes', 'Sure thing', 'Not really')))
  
})



