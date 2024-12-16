test_that("validate(study) does the right thing", {
  
  expect_silent(study <- make_study(name='my study'))
  
  expect_message(
    expect_true(
      validate(study)
    ),
    "Entity is valid"
  )
  
})

test_that("validate(study) fails for things", {
  
  expect_silent(study <- make_study())
  
  expect_message(
    expect_false(
      validate(study)
    ),
    "Study name is missing"
  )
  
  # oh noes, let's add a name
  expect_message(
    study <- study %>% set_study_name('Brian'),
    "Adding study name.+Brian"
  )

  expect_true(
    study %>% set_quiet() %>% validate()
  )
  
})