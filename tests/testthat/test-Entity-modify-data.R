test_that("modify_data() sees variables local to the calling function", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = 'household')

  # wrangle() scripts call modify_data() from inside a function, so the
  # pipeline must be evaluated in the caller's environment, not modify_data()'s
  wrangle <- function(entity) {
    suffix <- "!"
    cols <- c("Owns.property")
    entity %>%
      modify_data(mutate(across(any_of(cols), ~ paste0(.x, suffix))))
  }

  modified <- wrangle(households)
  expect_equal(
    modified@data$Owns.property,
    paste0(households@data$Owns.property, "!")
  )
})

test_that("modify_data() supports braced pipelines using `.`", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = 'household')

  wrangle <- function(entity) {
    drop_col <- "Owns.property"
    entity %>%
      modify_data({
        (.) %>% select(-all_of(drop_col))
      })
  }

  modified <- wrangle(households)
  expect_false("Owns.property" %in% names(modified@data))
})
