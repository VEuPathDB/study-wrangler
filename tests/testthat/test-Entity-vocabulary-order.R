test_that("set_variable_vocabulary_order with a character vector", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household")
  expect_true(households %>% quiet() %>% validate())

  # Basic setter with a partial order vector
  expect_message(
    households <- households %>%
      set_variable_vocabulary_order("Owns.property", order = c("No", "Yes")),
    "Successfully set vocabulary_order for 'Owns.property': No, Yes"
  )

  # Hydrated vocabulary should pin the given order first, then append remaining
  vocab <- households %>%
    quiet() %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  expect_equal(vocab[1:2], c("No", "Yes"))
  # Any remaining observed values (e.g. "Maybe") come after — none in this toy data
  # but the first two must be in the specified order
})

test_that("set_variable_vocabulary_order with a sort function", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household")

  # A sort function should be applied to observed values and stored as a character vector
  expect_message(
    households <- households %>%
      set_variable_vocabulary_order("Owns.property", order = sort),
    "Successfully set vocabulary_order for 'Owns.property'"
  )

  # After applying sort(), all values should appear in alphabetical order
  vocab <- households %>%
    quiet() %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  expect_equal(vocab, sort(vocab))
})

test_that("vocabulary_order pins partial order and appends remaining values", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household")

  # Pin only "Yes"; anything else should appear after it
  expect_message(
    households <- households %>%
      set_variable_vocabulary_order("Owns.property", order = c("Yes")),
    "Successfully set vocabulary_order"
  )

  vocab <- households %>%
    quiet() %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  expect_equal(vocab[[1]], "Yes")
  # Other observed values come after
  expect_true(length(vocab) >= 1)
})

test_that("vocabulary_order on non-existent variable gives a useful error", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  expect_error(
    households %>% set_variable_vocabulary_order("Nonexistent.var", order = c("a", "b")),
    "No such variable 'Nonexistent.var'"
  )
})

test_that("vocabulary_order rejects values not present in the data", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  expect_error(
    households %>% set_variable_vocabulary_order("Owns.property", order = c("Yes", "Typo_Value")),
    "not found in the data"
  )
})

test_that("vocabulary_order on an ordinal variable is rejected with helpful message", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet() %>%
    set_variable_ordinal_levels("Owns.property", c("Yes", "No", "Maybe"))

  expect_error(
    households %>% set_variable_vocabulary_order("Owns.property", order = c("No", "Yes")),
    "set_variable_ordinal_levels"
  )
})

test_that("vocabulary_order on a continuous variable is rejected", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  expect_error(
    households %>% set_variable_vocabulary_order("Number.of.animals", order = c("1", "2")),
    "categorical or binary"
  )
})

test_that("validate() catches vocabulary_order values not in data", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  # Inject an invalid vocabulary_order directly via set_variable_metadata to bypass the setter
  households_bad <- households %>%
    set_variable_metadata("Owns.property", vocabulary_order = list("Yes", "Ghost_Value"))

  expect_warning(
    expect_false(households_bad %>% verbose() %>% validate()),
    "not found in the data"
  )
})

test_that("validate() catches vocabulary_order on a non-categorical variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  # Inject directly via set_variable_metadata
  households_bad <- households %>%
    set_variable_metadata("Number.of.animals", vocabulary_order = list("1", "2"))

  expect_warning(
    expect_false(households_bad %>% verbose() %>% validate()),
    "vocabulary_order is only applicable to categorical or binary"
  )
})

test_that("clearing vocabulary_order restores lexicographic vocabulary", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet()

  households_ordered <- households %>%
    set_variable_vocabulary_order("Owns.property", order = c("No", "Yes"))

  # Now clear it
  households_cleared <- households_ordered %>%
    set_variable_metadata("Owns.property", vocabulary_order = list())

  vocab_ordered <- households_ordered %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  vocab_cleared <- households_cleared %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  # Cleared should be lexicographic (i.e. sorted)
  expect_equal(vocab_cleared, sort(vocab_cleared))
  # Ordered should differ
  expect_false(identical(vocab_ordered, vocab_cleared))
})

test_that("STF round-trip preserves vocabulary_order", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name = "household") %>% quiet() %>%
    set_variable_vocabulary_order("Owns.property", order = c("No", "Yes"))

  output_dir <- "./tmp/stf-vocabulary-order"

  expect_silent(households %>% export_entity_to_stf(output_dir))

  tsv_path <- glue::glue("{output_dir}/entity-household.tsv")
  expect_silent(
    households2 <- entity_from_stf(tsv_path)
  )

  # vocabulary_order metadata should be identical after round-trip
  order_original <- households %>%
    get_variable_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary_order) %>%
    unlist()

  order_roundtrip <- households2 %>%
    get_variable_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary_order) %>%
    unlist()

  expect_equal(order_roundtrip, order_original)

  # And the hydrated vocabulary should also match
  vocab_original <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  vocab_roundtrip <- households2 %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Owns.property") %>%
    pull(vocabulary) %>%
    unlist()

  expect_equal(vocab_roundtrip, vocab_original)
})
