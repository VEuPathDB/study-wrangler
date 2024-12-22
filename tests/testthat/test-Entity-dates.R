test_that("set_variable_as_date() works", {
  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation')
  
  # mess up the date a bit - it's still a date but the column type is now character
  observations <- observations %>% modify_data(mutate(Observation.date = chartr("-", "/", Observation.date)))
  
  expect_message(
    expect_false(
      validate(observations)
    ),
    "The column 'Observation.date' is declared as 'date' but R does not currently recognise it as a date"
  )
  
  # let's see if we can fix it with `set_variable_as_date()`
  expect_error(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "All rows in column 'Observation.date' failed to convert to date"
  )
  
  # let's fix it back to ISO-8601
  observations <- observations %>% modify_data(mutate(Observation.date = chartr("/", "-", Observation.date)))

  # but because it's now just a string we expect validation to flag this
  expect_message(
    expect_false(
      validate(observations)
    ),
    "The column 'Observation.date' is declared as 'date' but R does not currently recognise it as a date"
  )

  # try again 
  expect_message(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "Made metadata update.s. to 'data_type' for 'Observation.date'"
  )
  
  # now it gets detected as a date column
  expect_output(
    observations %>% inspect_variable('Observation.date'),
    "data_type\\s+date"
  )
  
  # let's make one bad date
  observations <- observations %>% modify_data(mutate(Observation.date = str_replace(Observation.date, "11-30", "11-31")))

  # we expect to have lost formal date format
  expect_message(
    expect_false(
      validate(observations)
    ),
    "The column 'Observation.date' is declared as 'date' but R does not currently recognise it as a date"
  )

  # and we expect to not be allowed to convert it back to date
  expect_error(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "1 rows in column 'Observation.date' failed to convert to date.+Problematic values.+2023-11-31"
  )
  
})

test_that("See how non-ISO-8601 inputs work - YYYY/MM/DD", {
  
  # change to 2021/01/01
  date_changer <- function(data) {
    data <- data %>% mutate(`Observation date` = chartr("-", "/", `Observation date`))
    return(data)
  }

  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation', preprocess_fn = date_changer)
  
  # so it gets detected as a date column
  expect_output(
    observations %>% inspect_variable('Observation.date'),
    "data_type\\s+date"
  )

  # and is actually represented with dashes in proper ISO-8601 format
  expect_false(
    observations %>% get_data() %>% pull(Observation.date) %>% str_detect("/") %>% any()
  )
  expect_true(
    observations %>% get_data() %>% pull(Observation.date) %>% str_detect("^\\d{4}-\\d{2}-\\d{2}$") %>% all()
  )
  
  
})

test_that("See how non-ISO-8601 inputs work - YY-MM-DD", {
  
  # change to 21-01-01
  date_changer <- function(data) {
    data <- data %>% mutate(`Observation date` = str_replace(`Observation date`, "^20", ""))
    return(data)
  }

  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation', preprocess_fn = date_changer)
  
  # so it gets detected as an ID column - that's better than it getting detected as a date :-)
  expect_output(
    observations %>% inspect_variable('Observation.date'),
    "data_type\\s+id"
  )
  
  # redetect as a variable
  expect_message(
    observations <- observations %>% redetect_columns_as_variables('Observation.date'),
    "Redoing type detection"
  )

  # now it's a string - phew - we didn't want it to be a date
  expect_output(
    observations %>% inspect_variable('Observation.date'),
    "data_type\\s+string"
  )
  
  # let's see what happens if we try to make it a date
  expect_error(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "All rows in column 'Observation.date' failed to convert to date"
  )
  
  # good - we want to be strict about this
  
})

