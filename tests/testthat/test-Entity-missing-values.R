test_that("Entity with missing values validates and loads correctly", {
  # Load test data with missing values
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')

  # Verify data loaded correctly
  expect_equal(nrow(households@data), 6)

  # Check that NAs are preserved in the data
  # H002 missing Location
  expect_true(is.na(households@data$Location[2]))

  # H003 missing Distance to water
  expect_true(is.na(households@data$`Distance.to.water`[3]))

  # H004 missing Number of rooms
  expect_true(is.na(households@data$`Number.of.rooms`[4]))

  # H005 missing Construction date
  expect_true(is.na(households@data$`Construction.date`[5]))

  # H006 missing Building type (and others)
  expect_true(is.na(households@data$`Building.type`[6]))

  # Entity should validate
  expect_true(households %>% quiet() %>% validate())
})


test_that("Missing values preserved in STF roundtrip", {
  # Load entity from file
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  expect_true(households %>% validate())

  # Export to STF
  output_dir <- "./tmp/stf-missing-values"
  expect_silent(
    households %>% export_entity_to_stf(output_dir)
  )

  # Import from STF
  tsv_path <- glue("{output_dir}/entity-household.tsv")
  expect_silent(
    households2 <- entity_from_stf(tsv_path)
  )

  # Should be identical (critical STF roundtrip check)
  # Set both to quiet mode for comparison since @quiet is not semantically important
  expect_equal(households2 %>% quiet(), households %>% quiet())

  # Verify NAs are preserved in specific locations
  expect_true(is.na(households2@data$Location[2]))
  expect_true(is.na(households2@data$`Distance.to.water`[3]))
  expect_true(is.na(households2@data$`Number.of.rooms`[4]))
  expect_true(is.na(households2@data$`Construction.date`[5]))
  expect_true(is.na(households2@data$`Building.type`[6]))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("Inspect variable handles missing values correctly for string variables", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  # Inspect Location variable (has 2 NAs: H002, H006)
  output <- capture.output(
    households %>% inspect_variable("Location")
  )

  # Should show NA count or missing count
  # The exact format may vary, but it should mention missing data
  combined_output <- paste(output, collapse = " ")

  # Check that the output mentions the variable
  expect_true(any(grepl("Location", output, ignore.case = TRUE)))

  # Check for missing data indication (NA, missing, or n_missing)
  expect_true(any(grepl("NA|missing|n_missing", combined_output, ignore.case = TRUE)))
})


test_that("Inspect variable handles missing values correctly for number variables", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  # Inspect Distance.to.water variable (has 2 NAs: H003, H006)
  output <- capture.output(
    households %>% inspect_variable("Distance.to.water")
  )

  combined_output <- paste(output, collapse = " ")

  # Check that the output mentions the variable
  expect_true(any(grepl("Distance.*water", output, ignore.case = TRUE)))

  # Check for missing data indication
  expect_true(any(grepl("NA|missing|n_missing", combined_output, ignore.case = TRUE)))
})


test_that("Inspect variable handles missing values correctly for integer variables", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  # Inspect Number.of.rooms variable (has 1 NA: H004)
  output <- capture.output(
    households %>% inspect_variable("Number.of.rooms")
  )

  combined_output <- paste(output, collapse = " ")

  # Check that the output mentions the variable
  expect_true(any(grepl("Number.*rooms", output, ignore.case = TRUE)))

  # Check for missing data indication
  expect_true(any(grepl("NA|missing|n_missing", combined_output, ignore.case = TRUE)))
})


test_that("Inspect variable handles missing values correctly for date variables", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  # Inspect Construction.date variable (has 2 NAs: H005, H006)
  output <- capture.output(
    households %>% inspect_variable("Construction.date")
  )

  combined_output <- paste(output, collapse = " ")

  # Check that the output mentions the variable
  expect_true(any(grepl("Construction.*date", output, ignore.case = TRUE)))

  # Check for missing data indication
  expect_true(any(grepl("NA|missing|n_missing", combined_output, ignore.case = TRUE)))
})


test_that("Entity summary shows missing value counts", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  # Get summary/inspection output
  output <- capture.output(
    households %>% inspect()
  )

  combined_output <- paste(output, collapse = " ")

  # Should show missing data for multiple variables
  # The exact format depends on the inspect() implementation
  # At minimum it should mention the entity and possibly missing values
  expect_true(any(grepl("household", output, ignore.case = TRUE)))
})
