test_that("participants fixture loads and validates", {
  file_path <- testthat::test_path("fixtures/participants.tsv")
  participants <- entity_from_file(file_path, name="participant")
  expect_true(validate(participants, quiet=TRUE))
})

test_that("set_parents() works and the result validates", {
  file_path <- testthat::test_path("fixtures/participants.tsv")
  participants <- entity_from_file(file_path, name="participant")
  expect_true(validate(participants, quiet=TRUE))

  expect_message(
    participants <- participants %>%
      set_parents(names=c("household"), ids=c("Household.Id")),
    "Parent entity relationships and columns have been set"
  )

  expect_true(validate(participants, quiet=TRUE))
})


