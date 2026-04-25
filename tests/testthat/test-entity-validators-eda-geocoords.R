test_that("geocoordinate validator passes when no geocoordinate variables present", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Use original households data (no lat/lng columns)
  households <- entity_from_file(file_path, name = 'household') %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should pass validation (no geocoordinate variables)
  expect_true(households %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("geocoordinate validator gives advisory message for single orphan latitude variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add only a latitude column (as character since preprocess_fn gets character data)
  add_latitude_only <- function(data) {
    return(data %>% mutate(latitude = c("40.7", "41.0", "42.0")))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_latitude_only) %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should pass but give advisory message
  expect_message(
    expect_message(
      is_valid <- validate(households, profiles = c("baseline", "eda")),
      "Advisory messages.*Found single geocoordinate variable 'latitude' without its pair"
    ),
    "Entity is valid."
  )
  expect_true(is_valid)
})

test_that("geocoordinate validator gives advisory message for single orphan longitude variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add only a longitude column (as character since preprocess_fn gets character data)
  add_longitude_only <- function(data) {
    return(data %>% mutate(longitude = c("-74.0", "-73.0", "-72.0")))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_longitude_only) %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should pass but give advisory message
  expect_message(
    expect_message(
      is_valid <- validate(households, profiles = c("baseline", "eda")),
      "Advisory messages.*Found single geocoordinate variable 'longitude' without its pair"
    ),
    "Entity is valid."
  )
  expect_true(is_valid)
})

test_that("geocoordinate validator detects variables by provider_label", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add columns with non-obvious names (as character since preprocess_fn gets character data)
  add_geo_columns <- function(data) {
    return(data %>% mutate(
      coord_x = c("-74.0", "-73.0", "-72.0"),
      coord_y = c("40.7", "41.0", "42.0")
    ))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_columns) %>%
    quiet() %>%
    set_variable_metadata('coord_x', provider_label = list(c("longitude"))) %>%
    set_variable_metadata('coord_y', provider_label = list(c("latitude"))) %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should give validation warning since metadata is not yet set correctly
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude
  expect_warning(
    validate(households, profiles = c("baseline", "eda")),
    glue::glue("Validation issues found.*Longitude variable 'coord_x' must have stable_id = '{expected_lng_stable_id}'")
  )
})

test_that("geocoordinate validator fails when more than 2 geocoordinate variables present", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add too many geocoordinate columns (as character since preprocess_fn gets character data)
  add_too_many_geo <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0"),
      lat = c("41.7", "42.0", "43.0")
    ))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_too_many_geo) %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Validation issues found.*Found more than 2 geocoordinate variables: latitude, lat, longitude"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when 2 variables but not one lat and one lng", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add two latitude columns (as character since preprocess_fn gets character data)
  add_two_lats <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      lat = c("41.7", "42.0", "43.0")
    ))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_two_lats) %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Validation issues found.*Found 2 geocoordinate variables but not exactly one latitude and one longitude"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when latitude variable has wrong stable_id", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add proper lat/lng columns (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }
  
  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = 'WRONG_ID', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    glue::glue("Validation issues found.*Latitude variable 'latitude' must have stable_id = '{expected_lat_stable_id}'")
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when latitude variable has wrong data_type", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add proper lat/lng columns (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = expected_lat_stable_id, data_type = 'string') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Validation issues found.*Latitude variable 'latitude' must have data_type = 'number'"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when longitude variable has wrong stable_id", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add proper lat/lng columns (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = expected_lat_stable_id, data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'WRONG_ID', data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    glue::glue("Validation issues found.*Longitude variable 'longitude' must have stable_id = '{expected_lng_stable_id}'")
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when longitude variable has wrong data_type", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add proper lat/lng columns (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = expected_lat_stable_id, data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'number') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Validation issues found.*Longitude variable 'longitude' must have data_type = 'longitude'"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator passes when both variables have correct metadata", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add proper lat/lng columns (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_display_names_from_provider_labels() %>%
    infer_geo_variables_for_eda() %>%
    verbose()

  # Should pass validation, include baseline checks too
  expect_true(households %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("geocoordinate validator detects partial matches in variable names", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add columns with partial name matches (as character since preprocess_fn gets character data)
  add_partial_names <- function(data) {
    return(data %>% mutate(
      original_lat = c("40.7", "41.0", "42.0"),
      original_long = c("-74.0", "-73.0", "-72.0")
    ))
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_partial_names) %>%
    quiet() %>%
    set_variable_display_names_from_provider_labels() %>%
    infer_geo_variables_for_eda() %>%
    verbose()

  # Should pass validation (partial matches should work)
  expect_true(households %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("geocoordinate validator ignores false positives", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add a column that contains "long" but isn't longitude
  add_false_positive <- function(data) {
    return(data %>% mutate(
      `long term carer` = c("yes", "no", "yes")
    ))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_false_positive) %>%
    quiet() %>% set_variable_display_names_from_provider_labels() %>% verbose()
  
  # Should give advisory message about the orphan
  expect_message(
    expect_message(
      is_valid <- validate(households, profiles = c("baseline", "eda")),
      "Advisory messages.*Found single geocoordinate variable 'long.term.carer' without its pair"
    ),
    "Entity is valid."
  )
  expect_true(is_valid)
})

test_that("geocoordinate validator combines multiple validation errors", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add lat/lng with multiple errors (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = 'WRONG_LAT_ID', data_type = 'string') %>%
    set_variable_metadata('longitude', stable_id = 'WRONG_LNG_ID', data_type = 'number') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  # Should fail with multiple error messages
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    glue("Validation issues found.*Latitude variable 'latitude' must have stable_id = '{expected_lat_stable_id}'.*Latitude variable 'latitude' must have data_type = 'number'.*Longitude variable 'longitude' must have stable_id = '{expected_lng_stable_id}'.*Longitude variable 'longitude' must have data_type = 'longitude'")
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator provides remedial guidance and fix works", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  # Add lat/lng with incorrect metadata (as character since preprocess_fn gets character data)
  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = 'WRONG_ID', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'WRONG_ID', data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  # Should fail validation with remedial guidance (uses 'entity' as fallback name in test context)
  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "To automatically set the correct metadata and generate geohash variables, use:.*entity.*%>%.*infer_geo_variables_for_eda\\(\\)"
  )
  expect_false(is_valid)

  # Apply the suggested fix
  households <- households %>% quiet() %>% infer_geo_variables_for_eda()

  # Should now pass validation
  expect_true(households %>% validate(profiles = c("baseline", "eda")))
})

# ── encode_geohash unit tests ────────────────────────────────────────────────

test_that("encode_geohash produces correct output for known coordinates", {
  # (40.7, -74.0) is the New York City area; well-known geohash prefix "dr"
  expect_equal(encode_geohash(40.7, -74.0, 1), "d")
  expect_equal(encode_geohash(40.7, -74.0, 2), "dr")
})

test_that("encode_geohash returns NA for NA coordinates", {
  expect_equal(encode_geohash(NA, -74.0, 1), NA_character_)
  expect_equal(encode_geohash(40.7, NA, 1), NA_character_)
})

# ── infer_geo_variables_for_eda geohash generation tests ────────────────────

test_that("infer_geo_variables_for_eda sets display_type for lat and lng variables", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    infer_geo_variables_for_eda()

  vars <- households@variables
  expect_equal(as.character(vars %>% filter(variable == 'latitude')  %>% pull(display_type)), 'latitude')
  expect_equal(as.character(vars %>% filter(variable == 'longitude') %>% pull(display_type)), 'longitude')
})

test_that("infer_geo_variables_for_eda generates six geohash columns with correct metadata", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    infer_geo_variables_for_eda()

  expected_ids <- get_config()$export$eda$stable_ids$geoaggregator
  vars <- households@variables

  for (col_name in names(expected_ids)) {
    row <- vars %>% filter(variable == col_name)
    expect_equal(nrow(row), 1, info = paste("missing metadata row for", col_name))
    expect_equal(as.character(row$display_type), 'geoaggregator', info = col_name)
    expect_equal(row$stable_id, expected_ids[[col_name]], info = col_name)
    expect_equal(as.character(row$data_type), 'string', info = col_name)
    expect_equal(as.character(unlist(row$hidden)), 'everywhere', info = col_name)
  }

  # Data should also contain the six columns
  for (col_name in names(expected_ids)) {
    expect_true(col_name %in% colnames(households@data), info = col_name)
    expect_type(households@data[[col_name]], "character")
  }

  # Verify geohash values for the first row (lat=40.7, lng=-74.0)
  expect_equal(households@data[["geohash_1"]][1], "d")
  expect_equal(households@data[["geohash_2"]][1], "dr")
})

# ── Validator display_type tests ─────────────────────────────────────────────

test_that("geocoordinate validator fails when latitude variable has wrong display_type", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude',  stable_id = expected_lat_stable_id, data_type = 'number',    display_type = 'default') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude', display_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Latitude variable 'latitude' must have display_type = 'latitude'"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when longitude variable has wrong display_type", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude',  stable_id = expected_lat_stable_id, data_type = 'number',    display_type = 'latitude') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude', display_type = 'default') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "Longitude variable 'longitude' must have display_type = 'longitude'"
  )
  expect_false(is_valid)
})

test_that("geocoordinate validator fails when no geoaggregator variables are present", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  add_geo_coords <- function(data) {
    data %>% mutate(
      latitude  = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    )
  }

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  # Set correct lat/lng metadata including display_type, but no geohash/geoaggregator variables
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude',  stable_id = expected_lat_stable_id, data_type = 'number',    display_type = 'latitude') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude', display_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
    verbose()

  expect_warning(
    is_valid <- validate(households, profiles = c("baseline", "eda")),
    "No geoaggregator variables found"
  )
  expect_false(is_valid)
})