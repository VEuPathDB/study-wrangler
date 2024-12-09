test_that("validate(households) warns about missing entity name and returns FALSE", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path)

  # validate
  expect_message(
    is_valid <- validate(households),
    "Entity is missing required 'name' metadata"
  )
  
  # fix it erroneously
  expect_warning(
    households <- households %>% set_entity_name(''),
    "Warning: Entity name is missing or not plain alphanumeric"
  )
  
  # fix it erroneously
  expect_warning(
    households <- households %>% set_entity_name("a bad-name$"),
    "Warning: Entity name is missing or not plain alphanumeric"
  )
  
  # fix it
  expect_message(
    expect_message(
      expect_message(
        households <- households %>% set_entity_name('household'),
        "Adding entity name 'household'..."
      ),
      "Note: added default display_name,"
    ),
    "Note: added default display_name_plural,"
  )
  
  # should be OK now
  expect_true(validate(households, quiet=TRUE))
})


test_that("validate(households) returns TRUE for original households fixture data, given a name", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(validate(households, quiet=TRUE))
})

test_that("validate() fails and warns about missing metadata", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # add a new data column
  households@data <-
    households@data %>%
      mutate(newColumn = 42)
    
  # validate
  expect_warning(
    is_valid <- validate(households),
    "Variable metadata is missing for these data columns: newColumn"
  )
  expect_false(is_valid)
  
  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced variables metadata by adding defaults for: newColumn"
  )
  
  # now it should be valid
  expect_true(validate(households, quiet=TRUE))
})

test_that("validate() fails and warns about extra metadata", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # remove a new data column
  households@data <-
    households@data %>%
    select(-c('Owns.property'))
  
  # validate
  expect_warning(
    is_valid <- validate(households),
    "These variables have metadata but no data columns: Owns.property"
  )
  expect_false(is_valid)

  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced metadata by removing these variables with no data: Owns.property"
  )
  
  # now it should be valid
  expect_true(validate(households, quiet=TRUE))
})

test_that("validate(households) warns about multiple ID columns per entity_level", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')

  # fake a new ID column
  households@data <- households@data %>%
    mutate(dupeId = Household.Id)
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "adding.+dupeId"
  )
  
  expect_message(
    is_valid <- validate(households),
    "There are multiple ID columns per entity level"
  )
  expect_false(is_valid)
  
  # fix it
  expect_message(
    households <- households %>%
      set_variable_metadata('dupeId', data_type='string'),
    "Made metadata update"
  )
  
  # it should now be fixed
  expect_true(validate(households, quiet=TRUE))
  
})


test_that("validate(households) warns about NAs in ID columns", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')
  expect_true(validate(households, quiet=TRUE))

  households@data[2,'Household.Id'] <- NA
  
  expect_message(
    expect_false(validate(households)),
    "ID columns contain NA values.+Household.Id"
  )
})

test_that("validate(households) warns about duplicates in ID columns", {
  # Example file path
  file_path <- testthat::test_path("fixtures/households.tsv")
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')
  expect_true(validate(households, quiet=TRUE))
  
  households@data[2,'Household.Id'] <- 'H001'
  
  expect_message(
    expect_false(validate(households)),
    "ID columns contain duplicates.+Household.Id"
  )
})

