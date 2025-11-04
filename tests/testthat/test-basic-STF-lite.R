test_that("STF-Lite study loads from fixtures", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')

  study <- study_from_stf(stf_lite_dir)
  expect_s4_class(study, "Study")
})

test_that("STF-Lite study has correct entity names", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir)

  entity_names <- study %>% get_entities() %>% map_chr(get_entity_name)

  expect_contains(entity_names, "site")
  expect_contains(entity_names, "sample")
})

test_that("STF-Lite entities have correct data", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir)

  site <- study %>% get_entity('site')
  sample <- study %>% get_entity('sample')

  # Check row counts
  expect_equal(nrow(site@data), 3)
  expect_equal(nrow(sample@data), 5)

  # Check that ID columns exist
  expect_true("site" %in% colnames(site@data))
  expect_true("site" %in% colnames(sample@data))
  expect_true("sample" %in% colnames(sample@data))

  # Check some variable columns
  expect_true("site_name" %in% colnames(site@data))
  expect_true("sample_type" %in% colnames(sample@data))
})

test_that("STF-Lite parent-child relationships are correct", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir)

  site <- study %>% get_entity('site')
  sample <- study %>% get_entity('sample')

  # Sample should have site as parent
  sample_parents <- sample %>% get_parents()
  expect_equal(sample_parents$names, c("site"))

  # Site should have no parents (root entity)
  site_parents <- site %>% get_parents()
  expect_equal(length(site_parents$names), 0)
})

test_that("STF-Lite study validates with default profile", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir)

  # Study won't have a name, which causes a warning
  expect_warning(
    is_valid <- validate(study),
    "Study name is missing"
  )

  # Add a name and it should fully validate
  study <- study %>% quiet() %>% set_study_name('STF-Lite Test Study')
  expect_true(study %>% quiet() %>% validate())
})

test_that("STF-Lite child references to parent IDs are valid", {
  stf_lite_dir <- system.file("extdata", "stf-lite", package = 'study.wrangler')
  study <- study_from_stf(stf_lite_dir)

  site <- study %>% get_entity('site')
  sample <- study %>% get_entity('sample')

  # All parent IDs in sample should exist in site
  site_ids <- site@data$site
  sample_parent_ids <- sample@data$site

  expect_true(all(sample_parent_ids %in% site_ids))
})
