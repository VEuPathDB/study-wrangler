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

  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_metadata('latitude', stable_id = expected_lat_stable_id, data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = expected_lng_stable_id, data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
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
  
  expected_lat_stable_id <- get_config()$export$eda$stable_ids$latitude
  expected_lng_stable_id <- get_config()$export$eda$stable_ids$longitude
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_partial_names) %>%
    quiet() %>%
    set_variable_metadata('original_lat', stable_id = expected_lat_stable_id, data_type = 'number') %>%
    set_variable_metadata('original_long', stable_id = expected_lng_stable_id, data_type = 'longitude') %>%
    set_variable_display_names_from_provider_labels() %>%
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
    "To automatically set the correct metadata for geocoordinate variables, use:.*entity.*%>%.*infer_geo_variables_for_eda\\(\\)"
  )
  expect_false(is_valid)

  # Apply the suggested fix
  households <- households %>% quiet() %>% infer_geo_variables_for_eda()

  # Should now pass validation
  expect_true(households %>% validate(profiles = c("baseline", "eda")))
})