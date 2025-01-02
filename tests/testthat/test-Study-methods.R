test_that("get_study_id() works", {
  expect_no_error(
    study <- make_study()
  )
  
  expect_equal(
    study %>% get_study_name(),
    NA_character_
  )
  
  expect_error(
    study_id <- study %>% get_study_id()
  )
  
  expect_no_error(
    study <- study %>% set_study_name('amazing study')
  )
  
  expect_equal(
    study %>% get_study_id(),
    'GCfFdZ1SzOf'
  )
})

  
  