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

## Note: the main test will be a round trip


