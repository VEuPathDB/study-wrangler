multivalue_specs = list(
  'Construction.materials' = ';',
  'Distances.to.well' = ';',
  'Birth.dates' = ';',
  'Ages.of.children' = ';',
  'Satisfaction.levels' = ';',
  'Priority.scores' = ';'
)

# convenience helper
set_variables_multivalued_from_list <- function(entity, specs) {
  rlang::exec(set_variables_multivalued, entity, !!!specs)
}

test_that("VDI export expands multi-valued string variables", {
  # Load test data with multi-valued variables
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)
  
  expect_silent(expect_true(households %>% quiet() %>% validate()))

  # Get stable_id for Construction.materials before export
  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  construction_stable_id <- hydr_metadata %>%
    filter(variable == "Construction.materials") %>%
    pull(stable_id)

  # Create a simple study
  study <- study_from_entities(
    list(households),
    name = "Multi-valued test study"
  )

  expect_true(study %>% quiet() %>% validate())

  # Export to VDI
  output_dir <- "./tmp/vdi-multivalued-string"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read the attributevalue cache file for households
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  expect_true(length(attributevalue_file) == 1)

  # Read the cache file
  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Construction.materials (R equivalent of grep | wc -l)
  construction_rows <- attr_data %>%
    filter(attribute_stable_id == construction_stable_id) %>%
    nrow()

  # Calculate expected count from raw data
  # H001: "Concrete;Timber" -> 2 values
  # H002: "Timber;Straw" -> 2 values
  # H003: NA/empty -> 0 values
  # H004: "Straw;Mud" -> 2 values
  # H005: "Mud" -> 1 value
  # Total: 7 expanded values
  expect_equal(construction_rows, 7)

  # Verify specific expanded values exist
  construction_values <- attr_data %>%
    filter(attribute_stable_id == construction_stable_id) %>%
    pull(string_value)

  expect_true("Concrete" %in% construction_values)
  expect_true("Timber" %in% construction_values)
  expect_true("Straw" %in% construction_values)
  expect_true("Mud" %in% construction_values)

  # Verify entity IDs are replicated correctly
  # H001 should have 2 rows for Construction.materials
  h001_construction_rows <- attr_data %>%
    filter(attribute_stable_id == construction_stable_id) %>%
    filter(household_stable_id == "H001") %>%
    nrow()

  expect_equal(h001_construction_rows, 2)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export expands multi-valued number variables", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  distance_stable_id <- hydr_metadata %>%
    filter(variable == "Distances.to.well") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multi-valued number test"
  )

  output_dir <- "./tmp/vdi-multivalued-number"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows (grep | wc -l equivalent)
  distance_rows <- attr_data %>%
    filter(attribute_stable_id == distance_stable_id) %>%
    nrow()

  # Expected: H001: 1, H002: 2, H003: 2, H004: 1, H005: 2 = 8 total
  expect_equal(distance_rows, 8)

  # Verify number_value column is used (not string_value)
  distance_data <- attr_data %>%
    filter(attribute_stable_id == distance_stable_id)

  expect_true(all(!is.na(distance_data$number_value)))
  expect_true(all(is.na(distance_data$string_value)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export expands multi-valued date variables", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  birthdate_stable_id <- hydr_metadata %>%
    filter(variable == "Birth.dates") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multi-valued date test"
  )

  output_dir <- "./tmp/vdi-multivalued-date"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows
  birthdate_rows <- attr_data %>%
    filter(attribute_stable_id == birthdate_stable_id) %>%
    nrow()

  # Expected: H001: 1, H002: 2, H003: 0, H004: 3, H005: 1 = 7 total
  expect_equal(birthdate_rows, 7)

  # Verify date_value column is used
  birthdate_data <- attr_data %>%
    filter(attribute_stable_id == birthdate_stable_id)

  expect_true(all(!is.na(birthdate_data$date_value)))
  expect_true(all(is.na(birthdate_data$string_value)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export expands multi-valued integer variables", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  ages_stable_id <- hydr_metadata %>%
    filter(variable == "Ages.of.children") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multi-valued integer test"
  )

  output_dir <- "./tmp/vdi-multivalued-integer"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows
  ages_rows <- attr_data %>%
    filter(attribute_stable_id == ages_stable_id) %>%
    nrow()

  # Expected: H001: 1, H002: 2, H003: 0, H004: 3, H005: 1 = 7 total
  expect_equal(ages_rows, 7)

  # Verify number_value is used (integers stored as numbers in VDI)
  ages_data <- attr_data %>%
    filter(attribute_stable_id == ages_stable_id)

  expect_true(all(!is.na(ages_data$number_value)))
  expect_true(all(is.na(ages_data$string_value)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export expands multi-valued ordinal variables", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  satisfaction_stable_id <- hydr_metadata %>%
    filter(variable == "Satisfaction.levels") %>%
    pull(stable_id)

  priority_stable_id <- hydr_metadata %>%
    filter(variable == "Priority.scores") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multi-valued ordinal test"
  )

  expect_true(study %>% quiet() %>% validate())

  output_dir <- "./tmp/vdi-multivalued-ordinal"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Test string ordinal (Satisfaction.levels)
  satisfaction_rows <- attr_data %>%
    filter(attribute_stable_id == satisfaction_stable_id) %>%
    nrow()

  # Expected: H001: 2 (High;Medium), H002: 3 (Low;Medium;High), H003: 1 (Medium),
  #           H004: 1 (High), H005: 2 (Low;High) = 9 total
  expect_equal(satisfaction_rows, 9)

  # Verify expanded values match ordinal levels
  satisfaction_values <- attr_data %>%
    filter(attribute_stable_id == satisfaction_stable_id) %>%
    pull(string_value) %>%
    unique()

  expect_setequal(satisfaction_values, c("Low", "Medium", "High"))

  # Test integer ordinal (Priority.scores)
  priority_rows <- attr_data %>%
    filter(attribute_stable_id == priority_stable_id) %>%
    nrow()

  # Expected: H001: 2 (3;1), H002: 3 (1;2;3), H003: 1 (2), H004: 3 (5;3;1), H005: 2 (4;5) = 11 total
  expect_equal(priority_rows, 11)

  # Integer ordinals should be stored as numbers
  priority_data <- attr_data %>%
    filter(attribute_stable_id == priority_stable_id)

  expect_true(all(!is.na(priority_data$number_value)))
  expect_true(all(is.na(priority_data$string_value)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export correctly expands mixed multi-valued variable types (string, number, date)", {
  file_path <- system.file("extdata", "toy_example/householdsMultiValuedOrdinals.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet() %>%
    set_variables_multivalued_from_list(multivalue_specs)

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()
  construction_stable_id <- hydr_metadata %>%
    filter(variable == "Construction.materials") %>%
    pull(stable_id)

  distance_stable_id <- hydr_metadata %>%
    filter(variable == "Distances.to.well") %>%
    pull(stable_id)

  birthdate_stable_id <- hydr_metadata %>%
    filter(variable == "Birth.dates") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multiple multi-valued test"
  )

  output_dir <- "./tmp/vdi-multiple-multivalued"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Verify all variables are expanded independently
  household_abbrev <- get_entity_abbreviation(study, "household")
  attributevalue_file <- list.files(
    output_dir,
    pattern = glue("^attributevalue_.*{household_abbrev}\\.cache$"),
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Each variable should have its own rows
  construction_rows <- attr_data %>% filter(attribute_stable_id == construction_stable_id) %>% nrow()
  distance_rows <- attr_data %>% filter(attribute_stable_id == distance_stable_id) %>% nrow()
  birthdate_rows <- attr_data %>% filter(attribute_stable_id == birthdate_stable_id) %>% nrow()

  expect_equal(construction_rows, 7)
  expect_equal(distance_rows, 8)
  expect_equal(birthdate_rows, 7)

  # Total attributevalue rows should be sum of all variables (multi-valued and single-valued)
  # This ensures proper pmap-style independent expansion
  total_rows <- nrow(attr_data)
  expect_true(total_rows > construction_rows + distance_rows + birthdate_rows)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})
