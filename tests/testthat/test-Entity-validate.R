test_that("validate(households) warns about missing entity name and returns FALSE", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path)

  # validate
  expect_warning(
    is_valid <- validate(households),
    "Entity is missing required 'name' metadata.+To set an entity name.+set_entity_name"
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
  expect_true(households %>% quiet() %>% validate())
})


test_that("validate(households) returns TRUE for original households fixture data, given a name", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # validate it
  expect_true(households %>% quiet() %>% validate())
})

test_that("validate() fails and warns about missing metadata", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # add a new data column
  households <- households %>% modify_data(mutate(newColumn = 42))
    
  # validate
  expect_warning(
    is_valid <- validate(households),
    "Variable metadata is missing for these data columns:\\s+newColumn.+To synchronize variable metadata.+sync_variable_metadata"
  )
  expect_false(is_valid)
  
  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced variables metadata by adding defaults for: newColumn"
  )
  
  # now it should be valid
  expect_true(households %>% quiet() %>% validate())
})

test_that("sync_variable_metadata() gracefully handles completely missing variables metadata", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")

  # crudely replace it with an empty tibble (NULL not allowed in that slot)
  households@variables <- tibble()
  
  expect_warning(
    validate(households),
    "Variables' metadata is empty. Ensure metadata is correctly populated.+To reset the metadata.+sync_variable_metadata"
  )
  
  # now follow the advice given
  expect_message(
    expect_message(
      households <- households %>% sync_variable_metadata(),
      "Reinitializing empty or corrupted variable metadata"
    ),
    "Synced variables metadata by adding defaults"
  )
  
})
  
test_that("validate() fails and warns about extra metadata", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")
  # remove a new data column
  households <- households %>% modify_data(select(-c('Owns.property')))
  
  # validate
  expect_warning(
    is_valid <- validate(households),
    "These variables have metadata but no data columns:\\s+Owns.property.+To synchronize variable metadata.+sync_variable_metadata"
  )
  expect_false(is_valid)

  # we then fix the issue
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "Synced metadata by removing these variables with no data: Owns.property"
  )
  
  # now it should be valid
  expect_true(households %>% quiet() %>% validate())
})

test_that("validate() fails when any data_shape is NA", {
  
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name="household")

  # can only fake this the nasty way
  households@variables <- households@variables %>%
    mutate(data_shape = if_else(variable == 'Owns.property', NA, data_shape))
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "NAs found in critical variable metadata.+Owns.property"
  )
})



test_that("validate(households) warns about multiple ID columns per entity_level", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')

  # fake a new ID column
  households <- households %>% modify_data(mutate(dupeId = Household.Id))
  expect_message(
    households <- households %>%
      sync_variable_metadata(),
    "adding.+dupeId"
  )
  
  expect_warning(
    is_valid <- validate(households),
    "There are multiple ID columns per entity level"
  )
  expect_false(is_valid)
  
  # fix it
  expect_message(
    households <- households %>%
      redetect_columns_as_variables('dupeId'),
    "Redoing type detection"
  )
  
  # it should now be fixed
  expect_true(households %>% quiet() %>% validate())
  
})

test_that("validate(households) warns about NAs in ID columns", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')
  expect_true(households %>% quiet() %>% validate())

  households@data[2,'Household.Id'] <- NA
  
  expect_warning(
    expect_false(validate(households)),
    "ID columns contain NA values.+Household.Id"
  )
})

test_that("validate(households) warns about duplicates in ID columns", {
  # Example file path
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  # Create an Entity object
  households <- entity_from_file(file_path, name='household')
  expect_true(households %>% quiet() %>% validate())
  
  households@data[2,'Household.Id'] <- 'H001'
  
  expect_warning(
    expect_false(validate(households)),
    "ID columns contain duplicates.+Household.Id"
  )
})

test_that("validate() warns about mangled variable metadata columns", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  expect_true(households %>% quiet() %>% validate())

  # well we can't even set an illegal non-integer entity_level the nice way
  expect_error(
    households <- households %>%
      set_variable_metadata('Household.Id', entity_level = 1.5),
    "Cannot assign.+because it takes an integer"
  )

  # and definitely not a string
  expect_error(
    households <- households %>%
      set_variable_metadata('Household.Id', entity_level = "parent"),
    "Cannot assign.+because it takes an integer"
  )
  
  # and we can't mess up data_type either :-)  
  expect_error(
    households <- households %>%
      set_variable_metadata('Household.Id', data_type = 'sparkles'),
    "Cannot assign.+because it takes a factor"
  )
  
  # nor data_shape
  expect_error(
    households <- households %>%
      set_variable_metadata('Household.Id', data_shape = 'circle'),
    "Cannot assign.+because it takes a factor"
  )

  # nor a multi-valued metadata like provider_label - this must be given a list
  # and the help message shows you how
  expect_error(
    households <- households %>%
      set_variable_metadata('Household.Id', provider_label = 'something'),
    "Cannot assign.+because it takes a list.+set_variable_metadata.+list"
  )
  
    
})


test_that("validate() complains about no ID column at all", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # function to remove a column at load-time
  lose_id_column <- function(data) {
    return(data %>% select(-c("Household Id")))
  }
  
  households <- entity_from_file(file_path, name='household', preprocess_fn = lose_id_column)
  
  expect_warning(
    expect_false(validate(households)),
    "This entity appears to have no ID column."
  )
  
  # see test-Entity-make-id-column.R for repair task testing
})


test_that("validate() complains about integer columns with non-integer data", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # modify the whole data column to doubles
  households <- households %>% modify_data(mutate(Number.of.animals = Number.of.animals + 0.1))
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "Number.of.animals.+contains non-integer values.+redetect_columns_as_variables.+set_variable_metadata"
  )
})

test_that("validate() complains about number columns with non-numeric data", {
  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation')
  
  # Modify the whole data column to append a non-numeric string
  observations <- observations %>% modify_data(mutate(MUAC..cm. = paste(MUAC..cm., "cm")))
  
  expect_warning(
    expect_false(
      validate(observations)
    ),
    "MUAC..cm..+contains non-numeric values.+redetect_columns_as_variables"
  )
})

test_that("validate() complains about ID column with wrong entity_name", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household', quiet=TRUE)
  households <- households %>% set_variable_metadata('Household.Id', entity_name='garbage') %>% verbose()
  expect_warning(
    expect_false(
      validate(households)
    ),
    "ID column 'Household.Id' has incorrect `entity_name`"
  )

})

test_that("validate() complains about date columns with non-ISO-8601 dates", {
  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation')
  
  # mess up the date a bit - it's still a date but the column type is now character
  observations <- observations %>% modify_data(mutate(Observation.date = chartr("-", "/", Observation.date)))
  
  expect_warning(
    expect_false(
      validate(observations)
    ),
    "The column 'Observation.date' is declared as 'date' but R does not currently recognise it as a date.+modify_data.+as.Date.+format.*%m-%d-%Y"
  )
  
  # let's see if we can fix it with the recommended command
  expect_silent(
    observations <- observations %>% modify_data(mutate(Observation.date = as.Date(Observation.date)))
  )
  expect_true(observations %>% quiet() %>% validate())

  # mess up the date a bit more - now the character column has non-dates
  observations <- observations %>% modify_data(mutate(Observation.date = str_replace(Observation.date, "09-29", "09-31")))
  
  # now it can't be fixed with a simple `as.Date()`
  expect_error(
    observations <- observations %>% modify_data(mutate(Observation.date = as.Date(Observation.date))),
    "Caused by error in.+charToDate.+character string is not in a standard unambiguous format"
  )
})

test_that("validate() complains about variables with NA data_type and provides remedial advice", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # artificially set data_type to NA for one variable
  households@variables <- households@variables %>%
    mutate(data_type = if_else(variable == 'Owns.property', NA, data_type))
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "Metadata data_type must not be NA.+Owns.property.+To re-detect data types.+redetect_columns_as_variables"
  )
})

test_that("validate() complains about units on non-numeric variables and provides remedial advice", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # artificially add units to a non-numeric variable (Owns.property is a logical/factor)
  households <- households %>% set_variable_metadata('Owns.property', unit = 'yes/no')
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "These non-numeric variables should not have units.+Owns.property.+To remove units.+set_variable_metadata.+unit = NA"
  )
})

test_that("validate() complains about multi-valued variables that are not character type and provides remedial advice", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # artificially set a numeric column as multi-valued
  households <- households %>% set_variable_metadata('Number.of.animals', is_multi_valued = TRUE, multi_value_delimiter = ',')
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "multi-valued but their R data type is not 'character'.+Number.of.animals.+set_variable_metadata.+is_multi_valued = FALSE.+multi_value_delimiter = NA"
  )
})

test_that("validate() complains about ordinal variables that are not factors and provides remedial advice", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # artificially set a character column as ordinal (but not convert to factor)
  households <- households %>% set_variable_metadata('Construction.material', data_shape = 'ordinal')
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "data_shape 'ordinal' but their data columns are not R factors.+Construction.material.+modify_data.+as.factor"
  )
})

test_that("validate() complains about ordinal level inconsistencies and provides remedial advice", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')
  
  # Create an ordinal variable that has no levels defined
  households <- households %>% 
    set_variable_metadata('Construction.material', data_shape = 'ordinal', data_type = 'string') %>%
    modify_data(mutate(Construction.material = as.factor(Construction.material)))
  
  expect_warning(
    expect_false(
      validate(households)
    ),
    "Ordinal variable.+Construction.material.+has no ordinal_levels defined.+set_variable_ordinal_levels.+start from scratch"
  )
})

