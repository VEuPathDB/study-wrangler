test_that("get_hydrated_variable_and_category_metadata(entity) works", {
  # Create a study and retrieve entities
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  households <- study %>% get_root_entity()
  participants <- study %>% get_entity('participant')
  observations <- study %>% get_entity('observation')
  
  # Test that hydrated variable metadata runs without errors or messages
  expect_no_error(
    expect_no_message(
      {
        households_hvmd <- households %>% get_hydrated_variable_and_category_metadata()
        participants_hvmd <- participants %>% get_hydrated_variable_and_category_metadata()
        observations_hvmd <- observations %>% get_hydrated_variable_and_category_metadata()
      }
    )
  )
  
  # List of expected hydrated columns
  hydrated_columns <- c(
    "vocabulary", "precision", "distinct_values_count", "range_min", 
    "range_max", "bin_width_computed", "lower_quartile", "median", 
    "upper_quartile", "mean"
  )
  
  # Test that hydrated columns are present in all hydrated tibbles
  for (hvmd in list(households_hvmd, participants_hvmd, observations_hvmd)) {
    expect_true(all(hydrated_columns %in% names(hvmd)))
  }
  
  # Test that stable_id is all NA in regular metadata
  for (entity in list(households, participants, observations)) {
    regular_metadata <- entity %>% get_variable_metadata()
    expect_true(all(is.na(regular_metadata$stable_id)))
  }
  
  # Test that stable_id and parent_stable_id are truthy in hydrated metadata
  # and that other things are hydrated
  for (hvmd in list(households_hvmd, participants_hvmd, observations_hvmd)) {
    expect_true(all(!is.na(hvmd$stable_id)))
    expect_true(all(!is.na(hvmd$parent_stable_id)))
    expect_true(is.character(hvmd$stable_id))
    expect_true(is.character(hvmd$parent_stable_id))
    expect_true(is.list(hvmd$vocabulary))
    expect_true(is.integer(hvmd$precision))
    # check that all IDs are different
    expect_false(hvmd %>% pull(stable_id) %>% duplicated() %>% any())
  }
})

test_that("numeric/date stats are hydrated for categorical number, integer and date variables (#82)", {
  n <- 500
  data <- tibble(
    thing_id = sprintf("T%04d", seq_len(n)),
    int_cat = rep(c(1L, 2L, 3L, 5L, 8L), length.out = n),
    num_cat = rep(c(0.5, 1.25, 2.5), length.out = n),
    date_cat = rep(as.Date(c("2020-01-15", "2021-06-30", "2023-11-02")), length.out = n),
    # high-cardinality integer code, e.g. an external ID
    int_code = 100000L + sample.int(900000L, n),
    str_cat = rep(c("a", "b", "c"), length.out = n)
  ) %>%
    mutate(
      int_cont = int_cat,
      num_cont = num_cat,
      date_cont = date_cat
    )

  entity <- entity_from_tibble(data, name = "thing", skip_type_convert = TRUE) %>%
    quiet() %>%
    set_variable_metadata("int_cat", data_type = "integer", data_shape = "categorical") %>%
    set_variable_metadata("num_cat", data_type = "number", data_shape = "categorical") %>%
    set_variable_metadata("date_cat", data_type = "date", data_shape = "categorical") %>%
    set_variable_metadata("int_code", data_type = "integer", data_shape = "categorical") %>%
    set_variable_metadata("str_cat", data_type = "string", data_shape = "categorical") %>%
    set_variable_metadata("int_cont", data_type = "integer", data_shape = "continuous") %>%
    set_variable_metadata("num_cont", data_type = "number", data_shape = "continuous") %>%
    set_variable_metadata("date_cont", data_type = "date", data_shape = "continuous")

  expect_no_error(hvmd <- entity %>% get_hydrated_variable_and_category_metadata())

  stats_columns <- c(
    "mean", "bin_width_computed", "range_min", "lower_quartile",
    "median", "upper_quartile", "range_max"
  )
  row_for <- function(var) hvmd %>% filter(variable == var)

  # categorical numeric/date variables get exactly the stats their continuous twins get
  for (pair in list(c("int_cat", "int_cont"), c("num_cat", "num_cont"), c("date_cat", "date_cont"))) {
    expect_equal(
      row_for(pair[1]) %>% select(all_of(stats_columns)),
      row_for(pair[2]) %>% select(all_of(stats_columns)),
      label = pair[1]
    )
  }

  # continuous outputs are what they were before
  expect_equal(row_for("int_cont")$range_min, "1")
  expect_equal(row_for("int_cont")$range_max, "8")
  expect_equal(row_for("num_cont")$range_max, "2.5")
  expect_equal(row_for("date_cont")$range_min, "2020-01-15")
  expect_equal(row_for("date_cont")$range_max, "2023-11-02")
  expect_match(row_for("date_cont")$bin_width_computed, "^(day|week|month|year)$")

  # categoricals keep their vocabulary
  expect_equal(unlist(row_for("int_cat")$vocabulary), c("1", "2", "3", "5", "8"))
  expect_equal(unlist(row_for("date_cat")$vocabulary), c("2020-01-15", "2021-06-30", "2023-11-02"))

  # high-cardinality integer code: bin width computes and is a positive integer
  code_row <- row_for("int_code")
  expect_equal(code_row$range_min, as.character(min(data$int_code)))
  expect_equal(code_row$range_max, as.character(max(data$int_code)))
  expect_false(is.na(code_row$bin_width_computed))
  expect_gt(as.numeric(code_row$bin_width_computed), 0)
  expect_equal(code_row$distinct_values_count, n_distinct(data$int_code))

  # string categoricals are unchanged: vocabulary, no numeric stats
  str_row <- row_for("str_cat")
  expect_equal(unlist(str_row$vocabulary), c("a", "b", "c"))
  expect_true(all(is.na(str_row %>% select(all_of(stats_columns)))))
})
