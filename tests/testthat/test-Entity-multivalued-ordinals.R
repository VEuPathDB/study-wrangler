test_that("Multi-valued ordinal variables can be created and validated", {
  # Load test data with multi-valued ordinals
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household')

  # Mark variables as multi-valued
  expect_silent(
    households <- households %>%
      quiet() %>%
      set_variables_multivalued(
        'Satisfaction.levels' = ';',
        'Priority.scores' = ';',
        'Construction.materials' = ';',
        'Distances.to.well' = ';',
        'Birth.dates' = ';',
        'Ages.of.children' = ';'
      ) %>%
      verbose()  # Switch back to verbose for message testing
  )

  # Set ordinal levels on multi-valued string variable
  # This should now work (previously blocked)
  expect_message(
    households <- households %>%
      set_variable_ordinal_levels(
        'Satisfaction.levels',
        levels = c("Low", "Medium", "High")
      ),
    "Successfully set 'Satisfaction.levels' as an ordinal variable with levels: Low, Medium, High"
  )

  # Verify metadata is set correctly
  satisfaction_meta <- households@variables %>%
    filter(variable == 'Satisfaction.levels')

  expect_equal(as.character(satisfaction_meta$data_shape), 'ordinal')
  expect_equal(as.character(satisfaction_meta$data_type), 'string')
  expect_equal(satisfaction_meta$is_multi_valued, TRUE)
  expect_equal(satisfaction_meta$multi_value_delimiter, ';')
  expect_equal(unlist(satisfaction_meta$ordinal_levels[[1]]), c("Low", "Medium", "High"))

  # Verify data is NOT converted to factor (should remain character)
  expect_type(households@data$Satisfaction.levels, "character")

  # Set ordinal levels on multi-valued integer variable
  expect_message(
    households <- households %>%
      set_variable_ordinal_levels(
        'Priority.scores',
        levels = 1:5
      ),
    "Successfully set 'Priority.scores' as an ordinal variable with levels: 1, 2, 3, 4, 5"
  )

  # Verify integer ordinal metadata
  priority_meta <- households@variables %>%
    filter(variable == 'Priority.scores')

  expect_equal(as.character(priority_meta$data_shape), 'ordinal')
  expect_equal(as.character(priority_meta$data_type), 'integer')
  expect_equal(priority_meta$is_multi_valued, TRUE)
  expect_equal(as.character(unlist(priority_meta$ordinal_levels[[1]])), as.character(1:5))

  # Verify data is character (not factor, not integer)
  expect_type(households@data$Priority.scores, "character")

  # Entity should validate
  expect_true(households %>% quiet() %>% validate())
})


test_that("Multi-valued ordinals reject invalid levels", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued('Satisfaction.levels' = ';')

  # Attempting to set ordinal levels that don't include all observed values should fail
  # Data contains: "High;Medium", "Low;Medium;High", "Medium", "High", "Low;High"
  # So all of Low, Medium, High are present
  expect_error(
    households %>%
      set_variable_ordinal_levels(
        'Satisfaction.levels',
        levels = c("Low", "Medium")  # Missing "High"
      ),
    "The levels you provide must include all the observed levels in the data.+must also include these levels: High"
  )

  # Providing levels that include all observed values plus extras should work
  expect_no_error(
    households %>%
      set_variable_ordinal_levels(
        'Satisfaction.levels',
        levels = c("Very Low", "Low", "Medium", "High", "Very High")
      )
  )
})


test_that("Multi-valued ordinals have correct vocabulary in hydrated metadata", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued('Satisfaction.levels' = ';')

  ordinal_levels <- c("Low", "Medium", "High")

  households <- households %>%
    quiet() %>%
    set_variable_ordinal_levels('Satisfaction.levels', levels = ordinal_levels)

  # Get hydrated metadata
  hydrated <- households %>%
    quiet() %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == 'Satisfaction.levels')

  # Vocabulary should equal the ordinal levels in the same order
  expect_equal(
    hydrated$vocabulary[[1]],
    ordinal_levels
  )

  # Check that distinct_values_count reflects expanded values
  # Data: "High;Medium", "Low;Medium;High", "Medium", "High", "Low;High"
  # Expanded: High, Medium, Low, Medium, High, Medium, High, Low, High
  # Distinct: Low, Medium, High = 3
  expect_equal(hydrated$distinct_values_count, 3)
})


test_that("Multi-valued ordinals roundtrip through STF format", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued(
      'Satisfaction.levels' = ';',
      'Priority.scores' = ';'
    ) %>%
    set_variable_ordinal_levels('Satisfaction.levels', levels = c("Low", "Medium", "High")) %>%
    set_variable_ordinal_levels('Priority.scores', levels = 1:5) %>%
    redetect_columns_as_variables('Distances.to.well')

  expect_true(households %>% quiet() %>% validate())

  # Export to STF
  output_dir <- "./tmp/stf-multivalued-ordinals"
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

  # Verify key properties preserved
  sat_meta2 <- households2@variables %>%
    filter(variable == 'Satisfaction.levels')

  expect_equal(as.character(sat_meta2$data_shape), 'ordinal')
  expect_equal(sat_meta2$is_multi_valued, TRUE)
  expect_equal(unlist(sat_meta2$ordinal_levels[[1]]), c("Low", "Medium", "High"))

  # Data should remain as delimited character strings
  expect_type(households2@data$Satisfaction.levels, "character")
  expect_equal(
    households2@data$Satisfaction.levels[1],
    "High;Medium"
  )
})


test_that("inspect_variable shows correct info for multi-valued ordinals", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued('Satisfaction.levels' = ';') %>%
    set_variable_ordinal_levels('Satisfaction.levels', levels = c("Low", "Medium", "High"))

  # inspect_variable should show expanded vocabulary
  output <- capture.output(
    households %>% quiet() %>% inspect_variable("Satisfaction.levels")
  )

  # Should show all three levels
  expect_true(any(grepl("Low", output)))
  expect_true(any(grepl("Medium", output)))
  expect_true(any(grepl("High", output)))

  # Should indicate it's multi-valued
  expect_true(any(grepl("multi.?valued|is_multi_valued", output, ignore.case = TRUE)))

  # Should indicate it's ordinal
  expect_true(any(grepl("ordinal", output, ignore.case = TRUE)))
})
