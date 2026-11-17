# Tests for set_entity_stable_ids() helper function

test_that("set_entity_stable_ids() sets stable_ids correctly", {
  study <- make_study(name = 'test study') %>% quiet()
  entity_names <- get_entity_names(study)

  # Set stable_ids for first two entities
  study <- study %>%
    set_entity_stable_ids(entity_names[1:2], c('ENT_custom1', 'ENT_custom2'))

  # Verify they were set
  entities <- get_entities(study)
  expect_equal(get_stable_id(entities[[1]]), 'ENT_custom1')
  expect_equal(get_stable_id(entities[[2]]), 'ENT_custom2')
})

test_that("set_entity_stable_ids() with default uses entity names", {
  study <- make_study(name = 'test study') %>% quiet()
  entity_names <- get_entity_names(study)

  # Set stable_ids using defaults (entity names)
  study <- study %>% set_entity_stable_ids(entity_names[1:2])

  entities <- get_entities(study)
  expect_equal(get_stable_id(entities[[1]]), entity_names[1])
  expect_equal(get_stable_id(entities[[2]]), entity_names[2])
})

test_that("set_entity_stable_ids() validates input lengths", {
  study <- make_study(name = 'test study') %>% quiet()

  # Mismatched vector lengths should error
  expect_error(
    study %>% set_entity_stable_ids(c('household'), c('ENT_1', 'ENT_2')),
    "same length"
  )
})

test_that("set_entity_stable_ids() validates entity existence", {
  study <- make_study(name = 'test study') %>% quiet()

  # Non-existent entity should error
  expect_error(
    study %>% set_entity_stable_ids(c('nonexistent'), c('ENT_1')),
    "do not exist"
  )
})

test_that("set_entity_stable_ids() handles empty inputs", {
  study <- make_study(name = 'test study') %>% quiet()

  # Empty input should return unchanged study with message
  expect_message(
    result <- study %>% verbose() %>% set_entity_stable_ids(c(), c()),
    "No entities specified"
  )
  expect_identical(result@root_entity, study@root_entity)
})

read_vdi_cache <- function(output_dir, file_name) {
  readr::read_tsv(file.path(output_dir, file_name), col_names = FALSE,
                  col_types = readr::cols(.default = "c"))
}

test_that("a study's stable_id is exported to study.cache and every entity's study_stable_id", {
  study <- make_study(name = 'my study', stable_id = 'DANICA-1') %>% quiet()
  expect_equal(get_study_id(study), 'DANICA-1')

  output_dir <- withr::local_tempdir()
  study %>% export_to_vdi(output_directory = output_dir)

  expect_equal(read_vdi_cache(output_dir, 'study.cache') %>% pull(2), 'DANICA-1')
  expect_equal(
    read_vdi_cache(output_dir, 'entitytypegraph.cache') %>% pull(2),
    rep('DANICA-1', 3)
  )
})

test_that("a study without stable_id still gets a STUDY_ id from its name", {
  study <- make_study(name = 'my study') %>% quiet()
  expect_true(is.na(study@stable_id))
  expect_match(get_study_id(study), '^STUDY_')
  expect_equal(
    get_study_id(study),
    prefixed_alphanumeric_id(seed_string = 'my study', prefix = 'STUDY_', length = 10)
  )
})

test_that("get_study_abbreviation() ignores stable_id", {
  with_id <- make_study(name = 'my study', stable_id = 'DANICA-1')
  without_id <- make_study(name = 'my study')
  expect_equal(get_study_abbreviation(with_id), get_study_abbreviation(without_id))
})

test_that("a study's stable_id round-trips through STF", {
  stf_directory <- withr::local_tempdir()
  study1 <- make_study(name = 'my study', stable_id = 'DANICA-1', quiet_entities = FALSE)
  study1 %>% export_to_stf(stf_directory)

  expect_equal(yaml::read_yaml(file.path(stf_directory, 'study.yaml'))$stable_id, 'DANICA-1')
  study2 <- study_from_stf(stf_directory)
  expect_equal(study2@stable_id, 'DANICA-1')
  expect_equal(study2, study1)
})

test_that("an unset stable_id is not written to study.yaml", {
  stf_directory <- withr::local_tempdir()
  make_study(name = 'my study') %>% export_to_stf(stf_directory)
  expect_false('stable_id' %in% names(yaml::read_yaml(file.path(stf_directory, 'study.yaml'))))
})

test_that("set_entity_stable_ids() respects quiet mode", {
  study <- make_study(name = 'test study') %>% quiet()
  entity_names <- get_entity_names(study)

  # Quiet mode should not produce messages
  expect_silent(
    study %>% set_entity_stable_ids(entity_names[1], c('ENT_1'))
  )

  # Verbose mode should produce message
  expect_message(
    study %>% verbose() %>% set_entity_stable_ids(entity_names[1], c('ENT_1')),
    "Successfully set stable_ids for 1 entity"
  )
})
