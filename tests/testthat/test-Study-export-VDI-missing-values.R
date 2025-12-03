test_that("VDI export excludes rows with missing string values", {
  # Load test data with missing values
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  expect_silent(expect_true(households %>% validate()))

  # Get stable_id for Location before export
  location_stable_id <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Location") %>%
    pull(stable_id)

  # Create a simple study
  study <- study_from_entities(
    list(households),
    name = "Missing values test study"
  )

  expect_true(study %>% quiet() %>% validate())

  # Export to VDI
  output_dir <- "./tmp/vdi-missing-string"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read the attributevalue cache file for households
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  expect_true(length(attributevalue_file) == 1)

  # Read the cache file
  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Location variable
  location_rows <- attr_data %>%
    filter(attribute_stable_id == location_stable_id) %>%
    nrow()

  # Expected count: H001, H003, H004, H005 = 4 rows
  # (H002 and H006 have missing Location and should be excluded)
  expect_equal(location_rows, 4)

  # Verify specific values exist
  location_values <- attr_data %>%
    filter(attribute_stable_id == location_stable_id) %>%
    pull(string_value)

  expect_true("Boston" %in% location_values)
  expect_true("Seattle" %in% location_values)
  expect_true("Portland" %in% location_values)
  expect_true("Austin" %in% location_values)

  # Verify no empty strings
  expect_false("" %in% location_values)
  expect_false(any(is.na(location_values)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export excludes rows with missing number values", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  distance_stable_id <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Distance.to.water") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Missing number test"
  )

  output_dir <- "./tmp/vdi-missing-number"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Distance.to.water variable
  distance_rows <- attr_data %>%
    filter(attribute_stable_id == distance_stable_id) %>%
    nrow()

  # Expected: H001, H002, H004, H005 = 4 rows
  # (H003 and H006 have missing Distance to water and should be excluded)
  expect_equal(distance_rows, 4)

  # Verify number_value column is used (not string_value)
  distance_data <- attr_data %>%
    filter(attribute_stable_id == distance_stable_id)

  expect_true(all(!is.na(distance_data$number_value)))
  expect_true(all(is.na(distance_data$string_value)))

  # Verify specific values
  expect_true(1.5 %in% distance_data$number_value)
  expect_true(2.3 %in% distance_data$number_value)
  expect_true(0.8 %in% distance_data$number_value)
  expect_true(3.2 %in% distance_data$number_value)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export excludes rows with missing integer values", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  rooms_stable_id <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Number.of.rooms") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Missing integer test"
  )

  output_dir <- "./tmp/vdi-missing-integer"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Number.of.rooms variable
  rooms_rows <- attr_data %>%
    filter(attribute_stable_id == rooms_stable_id) %>%
    nrow()

  # Expected: H001, H002, H003, H005, H006 = 5 rows
  # (H004 has missing Number of rooms and should be excluded)
  expect_equal(rooms_rows, 5)

  # Verify number_value is used (integers stored as numbers in VDI)
  rooms_data <- attr_data %>%
    filter(attribute_stable_id == rooms_stable_id)

  expect_true(all(!is.na(rooms_data$number_value)))
  expect_true(all(is.na(rooms_data$string_value)))

  # Verify specific values
  expect_true(3 %in% rooms_data$number_value)
  expect_true(4 %in% rooms_data$number_value)
  expect_true(5 %in% rooms_data$number_value)
  expect_true(2 %in% rooms_data$number_value)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export excludes rows with missing date values", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  construction_date_stable_id <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Construction.date") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Missing date test"
  )

  output_dir <- "./tmp/vdi-missing-date"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Construction.date variable
  date_rows <- attr_data %>%
    filter(attribute_stable_id == construction_date_stable_id) %>%
    nrow()

  # Expected: H001, H002, H003, H004 = 4 rows
  # (H005 and H006 have missing Construction date and should be excluded)
  expect_equal(date_rows, 4)

  # Verify date_value column is used
  date_data <- attr_data %>%
    filter(attribute_stable_id == construction_date_stable_id)

  expect_true(all(!is.na(date_data$date_value)))
  expect_true(all(is.na(date_data$string_value)))

  # Verify specific values
  expect_true(as.Date("2015-06-10") %in% date_data$date_value)
  expect_true(as.Date("2016-08-15") %in% date_data$date_value)
  expect_true(as.Date("2017-03-22") %in% date_data$date_value)
  expect_true(as.Date("2018-11-30") %in% date_data$date_value)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export excludes rows with missing categorical values", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  building_type_stable_id <- households %>%
    get_hydrated_variable_and_category_metadata() %>%
    filter(variable == "Building.type") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Missing categorical test"
  )

  output_dir <- "./tmp/vdi-missing-categorical"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Read attributevalue file
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Count rows for Building.type variable
  building_type_rows <- attr_data %>%
    filter(attribute_stable_id == building_type_stable_id) %>%
    nrow()

  # Expected: H001, H002, H003, H004, H005 = 5 rows
  # (H006 has missing Building type and should be excluded)
  expect_equal(building_type_rows, 5)

  # Verify string_value column is used (categorical stored as string)
  building_type_data <- attr_data %>%
    filter(attribute_stable_id == building_type_stable_id)

  expect_true(all(!is.na(building_type_data$string_value)))

  # Verify specific values
  building_type_values <- building_type_data$string_value
  expect_true("House" %in% building_type_values)
  expect_true("Apartment" %in% building_type_values)
  expect_true("Townhouse" %in% building_type_values)

  # Verify no empty strings
  expect_false("" %in% building_type_values)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export correctly handles mixed missing and present values", {
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()

  location_stable_id <- hydr_metadata %>%
    filter(variable == "Location") %>%
    pull(stable_id)

  distance_stable_id <- hydr_metadata %>%
    filter(variable == "Distance.to.water") %>%
    pull(stable_id)

  rooms_stable_id <- hydr_metadata %>%
    filter(variable == "Number.of.rooms") %>%
    pull(stable_id)

  construction_date_stable_id <- hydr_metadata %>%
    filter(variable == "Construction.date") %>%
    pull(stable_id)

  building_type_stable_id <- hydr_metadata %>%
    filter(variable == "Building.type") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "Multiple missing values test"
  )

  output_dir <- "./tmp/vdi-multiple-missing"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  # Verify all variables are handled correctly
  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # Each variable should have its own row count (excluding NAs)
  location_rows <- attr_data %>% filter(attribute_stable_id == location_stable_id) %>% nrow()
  distance_rows <- attr_data %>% filter(attribute_stable_id == distance_stable_id) %>% nrow()
  rooms_rows <- attr_data %>% filter(attribute_stable_id == rooms_stable_id) %>% nrow()
  construction_date_rows <- attr_data %>% filter(attribute_stable_id == construction_date_stable_id) %>% nrow()
  building_type_rows <- attr_data %>% filter(attribute_stable_id == building_type_stable_id) %>% nrow()

  # Expected counts (excluding NAs)
  expect_equal(location_rows, 4)  # H001, H003, H004, H005
  expect_equal(distance_rows, 4)  # H001, H002, H004, H005
  expect_equal(rooms_rows, 5)     # H001, H002, H003, H005, H006
  expect_equal(construction_date_rows, 4)  # H001, H002, H003, H004
  expect_equal(building_type_rows, 5)      # H001, H002, H003, H004, H005

  # Total rows should be sum of all non-NA values (plus the ID column which all 6 households have)
  # ID column: 6 rows
  # Location: 4 rows
  # Distance to water: 4 rows
  # Number of rooms: 5 rows
  # Construction date: 4 rows
  # Building type: 5 rows
  # Total for these 5 variables: 4 + 4 + 5 + 4 + 5 = 22 rows
  # Plus one more variable (the ID column is included in the count above)

  total_rows_for_test_variables <- location_rows + distance_rows + rooms_rows +
                                    construction_date_rows + building_type_rows
  expect_equal(total_rows_for_test_variables, 22)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("VDI export handles entity with household H006 having multiple missing values", {
  # This test specifically checks that H006, which has missing values in multiple columns,
  # does not create any rows for those variables in the VDI export
  file_path <- system.file("extdata", "toy_example/householdsMissingValues.tsv", package = 'study.wrangler')
  households <- entity_from_file(file_path, name='household') %>%
    quiet()

  hydr_metadata <- households %>% get_hydrated_variable_and_category_metadata()

  location_stable_id <- hydr_metadata %>%
    filter(variable == "Location") %>%
    pull(stable_id)

  distance_stable_id <- hydr_metadata %>%
    filter(variable == "Distance.to.water") %>%
    pull(stable_id)

  construction_date_stable_id <- hydr_metadata %>%
    filter(variable == "Construction.date") %>%
    pull(stable_id)

  building_type_stable_id <- hydr_metadata %>%
    filter(variable == "Building.type") %>%
    pull(stable_id)

  study <- study_from_entities(
    list(households),
    name = "H006 multiple missing test"
  )

  output_dir <- "./tmp/vdi-h006-missing"
  expect_no_error(
    study %>% quiet() %>% export_to_vdi(output_directory = output_dir)
  )

  attributevalue_file <- list.files(
    output_dir,
    pattern = "^attributevalue_.*househld\\.cache$",
    full.names = TRUE
  )

  attr_data <- readr::read_tsv(
    attributevalue_file,
    col_names = c("household_stable_id", "attribute_stable_id", "string_value", "number_value", "date_value"),
    show_col_types = FALSE
  )

  # H006 should NOT have rows for variables where it has missing values
  # H006 has missing: Location, Distance to water, Construction date, Building type

  h006_location_rows <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == location_stable_id) %>%
    nrow()
  expect_equal(h006_location_rows, 0)

  h006_distance_rows <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == distance_stable_id) %>%
    nrow()
  expect_equal(h006_distance_rows, 0)

  h006_construction_date_rows <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == construction_date_stable_id) %>%
    nrow()
  expect_equal(h006_construction_date_rows, 0)

  h006_building_type_rows <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == building_type_stable_id) %>%
    nrow()
  expect_equal(h006_building_type_rows, 0)

  # H006 SHOULD have a row for Number of rooms (value = 3)
  rooms_stable_id <- hydr_metadata %>%
    filter(variable == "Number.of.rooms") %>%
    pull(stable_id)

  h006_rooms_rows <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == rooms_stable_id) %>%
    nrow()
  expect_equal(h006_rooms_rows, 1)

  h006_rooms_value <- attr_data %>%
    filter(household_stable_id == "H006" & attribute_stable_id == rooms_stable_id) %>%
    pull(number_value)
  expect_equal(h006_rooms_value, 3)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})
