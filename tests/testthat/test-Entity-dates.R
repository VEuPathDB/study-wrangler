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
  expect_message(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "Made metadata update.s. to 'data_type' for 'Observation.date'"
  )

  # mess up the date a bit more - now the character column has non-dates
  observations <- observations %>% modify_data(mutate(Observation.date = str_replace(Observation.date, "09-29", "09-31")))

  # expect some useful feedback for the dodgy date
  expect_error(
    observations <- observations %>% set_variable_as_date('Observation.date'),
    "Observation.date.+failed to convert to date.+Problematic values.+2023-09-31"
  )
  
  # fix the september issue
  observations <- observations %>% modify_data(mutate(Observation.date = str_replace(Observation.date, "09-31", "09-28")))
    # mess up every date
  observations <- observations %>% modify_data(mutate(Observation.date = str_replace(Observation.date, "^20", "")))
  
  expect_true(FALSE)
  # oh, it reads them in like this: "23-09-28" "23-05-04" "23-11-30" "23-04-21" (<date> column type)
})

test_that("See how non-ISO-8601 inputs work", {
  
  # change to 2021/01/01
  date_changer <- function(data) {
    data <- data %>% mutate(`Observation date` = chartr("-", "/", `Observation date`))
    return(data)
  }
  
  file_path <- system.file("extdata", "toy_example/participant_observations.tsv", package = 'study.wrangler')
  observations <- entity_from_file(file_path, name = 'observation', preprocess_fn = date_changer)
  
  expect_true(TRUE)
})

