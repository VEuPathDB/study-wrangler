test_that("inspect(study) works" , {
  expect_silent(
    study <- make_study()  
  )
  
  expect_no_error(
    expect_output(
      inspect(study),
      "Number of entities\\s+3\\b.+Entities.+household.+participant.+observation"
    )
  )

  
})