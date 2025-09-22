test_that("geocoordinate validator passes when no geocoordinate variables present", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Use original households data (no lat/lng columns)
  households <- entity_from_file(file_path, name = 'household')
  
  # Should pass validation (no geocoordinate variables)
  expect_true(households %>% quiet() %>% validate(profiles = "eda"))
})

test_that("geocoordinate validator gives advisory message for single orphan latitude variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add only a latitude column (as character since preprocess_fn gets character data)
  add_latitude_only <- function(data) {
    return(data %>% mutate(latitude = c("40.7", "41.0", "42.0")))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_latitude_only)
  
  # Should pass but give advisory message
  expect_message(
    is_valid <- validate(households, profiles = "eda"),
    "Advisory messages.*Found single geocoordinate variable 'latitude' without its pair"
  )
  expect_true(is_valid)
})

test_that("geocoordinate validator gives advisory message for single orphan longitude variable", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add only a longitude column (as character since preprocess_fn gets character data)
  add_longitude_only <- function(data) {
    return(data %>% mutate(longitude = c("-74.0", "-73.0", "-72.0")))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_longitude_only)
  
  # Should pass but give advisory message
  expect_message(
    is_valid <- validate(households, profiles = "eda"),
    "Advisory messages.*Found single geocoordinate variable 'longitude' without its pair"
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
    verbose()
  
  # Should give validation warning since metadata is not yet set correctly
  expect_warning(
    validate(households, profiles = "eda"),
    "Validation issues found.*Longitude variable 'coord_x' must have stable_id = 'OBI_0001621'"
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_too_many_geo)
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_two_lats)
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    set_variable_metadata('latitude', stable_id = 'WRONG_ID', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'OBI_0001621', data_type = 'longitude')
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Validation issues found.*Latitude variable 'latitude' must have stable_id = 'OBI_0001620'"
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    set_variable_metadata('latitude', stable_id = 'OBI_0001620', data_type = 'string') %>%
    set_variable_metadata('longitude', stable_id = 'OBI_0001621', data_type = 'longitude')
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    set_variable_metadata('latitude', stable_id = 'OBI_0001620', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'WRONG_ID', data_type = 'longitude')
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Validation issues found.*Longitude variable 'longitude' must have stable_id = 'OBI_0001621'"
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
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    set_variable_metadata('latitude', stable_id = 'OBI_0001620', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'OBI_0001621', data_type = 'number')
  
  # Should fail validation
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
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
    set_variable_metadata('latitude', stable_id = 'OBI_0001620', data_type = 'number') %>%
    set_variable_metadata('longitude', stable_id = 'OBI_0001621', data_type = 'longitude')
  
  # Should pass validation
  expect_true(validate(households, profiles = "eda"))
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
    set_variable_metadata('original_lat', stable_id = 'OBI_0001620', data_type = 'number') %>%
    set_variable_metadata('original_long', stable_id = 'OBI_0001621', data_type = 'longitude')
  
  # Should pass validation (partial matches should work)
  expect_true(validate(households, profiles = "eda"))
})

test_that("geocoordinate validator ignores false positives", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  # Add a column that contains "long" but isn't longitude
  add_false_positive <- function(data) {
    return(data %>% mutate(
      `long term carer` = c("yes", "no", "yes")
    ))
  }
  
  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_false_positive)
  
  # Should give advisory message about the orphan
  expect_message(
    is_valid <- validate(households, profiles = "eda"),
    "Advisory messages.*Found single geocoordinate variable 'long.term.carer' without its pair"
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
    set_variable_metadata('latitude', stable_id = 'WRONG_LAT_ID', data_type = 'string') %>%
    set_variable_metadata('longitude', stable_id = 'WRONG_LNG_ID', data_type = 'number')
  
  # Should fail with multiple error messages
  expect_warning(
    is_valid <- validate(households, profiles = "eda"),
    "Validation issues found.*Latitude variable 'latitude' must have stable_id = 'OBI_0001620'.*Latitude variable 'latitude' must have data_type = 'number'.*Longitude variable 'longitude' must have stable_id = 'OBI_0001621'.*Longitude variable 'longitude' must have data_type = 'longitude'"
  )
  expect_false(is_valid)
})