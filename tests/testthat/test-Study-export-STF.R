test_that("Study exports to STF directory", {
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  output_dir <- "./tmp/stf"
  
  expect_no_error(
    study %>% export_to_stf(output_dir)
  )
  
  # List files in the output directory
  output_files <- list.files(output_dir, full.names = FALSE)
  
  # and check that the files we need are there
  expect_true("study.yaml" %in% output_files)
  entity_names = study %>% get_entities() %>% map_chr(get_entity_name)
  for (entity_name in entity_names) {
    expect_true(glue("entity-{entity_name}.yaml") %in% output_files)
  }
  
})

test_that("Minimal STF (no YAML metadata) loads and validates", {
  stf_directory <- 'tmp/stf-minimal'
  make_minimal_stf(stf_directory)
  
  expect_no_error(
    study <- study_from_stf(stf_directory)
  )
  
  # all it lacks is a name
  expect_message(
    validate(study),
    "Study name is missing"
  )
  
  study <- study %>% quiet() %>% set_study_name('minimal')
  
  # should now validate
  expect_true(validate(study))
  
  unlink(stf_directory, recursive = TRUE)
})

test_that("The basic study roundtrips via regular STF", {
  stf_directory <- 'tmp/stf-full'
  study1 <- make_study()
  study1 %>% export_to_stf(stf_directory)
  study2 <- study_from_stf(stf_directory)
  expect_equal(study1, study2)
  
  unlink(stf_directory, recursive = TRUE)
  
})
