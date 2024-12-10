test_that("participants fixture loads and validates", {
  file_path <- testthat::test_path("fixtures/participants.tsv")
  participants <- entity_from_file(file_path, name="participant")
  expect_true(validate(participants, quiet=TRUE))
})

