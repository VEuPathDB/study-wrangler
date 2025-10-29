test_that("STF-Lite study without geo coordinates validates with EDA profile", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir) %>%
    quiet() %>% set_study_name('STF-Lite EDA Test') %>% verbose() %>%
    map_entities(~ .x %>% quiet() %>% set_variable_display_names_from_provider_labels())

  # Should validate with baseline and EDA profiles (no geo coordinates to worry about)
  expect_true(study %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("STF-Lite with geo coordinates fails EDA validation without infer_geo_variables", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir) %>%
    quiet() %>% set_study_name('STF-Lite Geo Test') %>% verbose() %>%
    map_entities(~ .x %>% quiet() %>% set_variable_display_names_from_provider_labels())

  # Should fail EDA validation because geo metadata is not set correctly
  expect_warning(
    expect_warning(
      is_valid <- validate(study, profiles = c("baseline", "eda")),
      "Validation issues found"
    ),
    "Fatal issue encountered"
  )
  expect_false(is_valid)
})

test_that("STF-Lite with geo coordinates passes EDA validation after infer_geo_variables", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir) %>%
    quiet() %>% set_study_name('STF-Lite Geo Test') %>% verbose() %>%
    map_entities(~ {
      .x %>% quiet() %>%
        set_variable_display_names_from_provider_labels() %>%
        infer_geo_variables()
    })

  # Should now pass baseline and EDA validation
  expect_true(study %>% quiet() %>% validate(profiles = c("baseline", "eda")))
})

test_that("infer_geo_variables sets correct metadata for latitude", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir)

  site <- study %>% get_entity('site') %>% quiet() %>% infer_geo_variables()

  # Check latitude metadata
  lat_metadata <- site %>% get_variable_metadata() %>% filter(variable == "latitude")

  expect_equal(as.character(lat_metadata$data_type), "number")
  expect_equal(lat_metadata$stable_id, get_config()$export$eda$stable_ids$latitude)
})

test_that("infer_geo_variables sets correct metadata for longitude", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir)

  site <- study %>% get_entity('site') %>% quiet() %>% infer_geo_variables()

  # Check longitude metadata
  lng_metadata <- site %>% get_variable_metadata() %>% filter(variable == "longitude")

  expect_equal(as.character(lng_metadata$data_type), "longitude")
  expect_equal(lng_metadata$stable_id, get_config()$export$eda$stable_ids$longitude)
})

test_that("infer_geo_variables detects multiple naming patterns", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir)

  site <- study %>% get_entity('site')

  # The fixture uses "latitude" and "longitude" - should detect them
  site_with_geo <- site %>% quiet() %>% infer_geo_variables()

  lat_metadata <- site_with_geo %>% get_variable_metadata() %>% filter(variable == "latitude")
  lng_metadata <- site_with_geo %>% get_variable_metadata() %>% filter(variable == "longitude")

  expect_equal(nrow(lat_metadata), 1)
  expect_equal(nrow(lng_metadata), 1)
  expect_equal(as.character(lat_metadata$data_type), "number")
  expect_equal(as.character(lng_metadata$data_type), "longitude")
})

test_that("infer_geo_variables returns modified entity", {
  stf_lite_geo_dir <- system.file("extdata", "stf-lite-geo", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_geo_dir)

  site <- study %>% get_entity('site') %>% quiet()

  # Should return an entity object
  site_with_geo <- site %>% infer_geo_variables()

  expect_s4_class(site_with_geo, "Entity")

  # Should have the geo metadata set
  vars <- site_with_geo %>% get_variable_metadata()
  lat_row <- vars %>% filter(variable == "latitude")
  lng_row <- vars %>% filter(variable == "longitude")

  expect_true(nrow(lat_row) == 1)
  expect_true(nrow(lng_row) == 1)
})

test_that("infer_geo_variables works on entities not from STF-Lite", {
  # Create an entity from scratch with lat/long columns
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')

  add_geo_coords <- function(data) {
    return(data %>% mutate(
      latitude = c("40.7", "41.0", "42.0"),
      longitude = c("-74.0", "-73.0", "-72.0")
    ))
  }

  households <- entity_from_file(file_path, name = 'household', preprocess_fn = add_geo_coords) %>%
    quiet() %>%
    set_variable_display_names_from_provider_labels() %>%
    infer_geo_variables()

  # Check that metadata was set correctly
  lat_metadata <- households %>% get_variable_metadata() %>% filter(variable == "latitude")
  lng_metadata <- households %>% get_variable_metadata() %>% filter(variable == "longitude")

  expect_equal(as.character(lat_metadata$data_type), "number")
  expect_equal(lat_metadata$stable_id, get_config()$export$eda$stable_ids$latitude)
  expect_equal(as.character(lng_metadata$data_type), "longitude")
  expect_equal(lng_metadata$stable_id, get_config()$export$eda$stable_ids$longitude)

  # Should pass baseline and EDA validation
  expect_true(households %>% validate(profiles = c("baseline", "eda")))
})
