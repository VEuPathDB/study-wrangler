test_that("participants fixture loads and fails validation due to wrongly infered ID column", {
  file_path <- testthat::test_path("fixtures/participants.tsv")
  expect_no_error(
    participants <- entity_from_file(file_path, name="participant")
  )
  expect_message(
    validate(participants),
    "There are multiple ID columns per entity level.+Participant.Id, Name"
  )
  
  # now we fix it
  expect_message(
    participants <- participants %>%
      redo_type_detection_as_variables_only('Name'),
    "Redoing type detection"
  )

  # and now it should validate
  expect_true(validate(participants, quiet=TRUE))
})

# test_that("set_parents() works and the result validates", {
#   file_path <- testthat::test_path("fixtures/participants.tsv")
#   participants <- entity_from_file(file_path, name="participant")
#   expect_true(validate(participants, quiet=TRUE))
# 
#   expect_message(
#     participants <- participants %>%
#       set_parents(names=c("household"), columns=c("Household.Id")),
#     "Parent entity relationships and columns have been set"
#   )
# 
#   expect_true(validate(participants, quiet=TRUE))
# })


# test_that("participant_observations fixture loads and validates", {
#   
#   
#   
# })

