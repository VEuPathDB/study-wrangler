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
