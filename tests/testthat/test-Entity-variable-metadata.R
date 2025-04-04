test_that("attributes like 'hidden' that are lists of factors work properly", {
  file_path <- system.file("extdata", "toy_example/households.tsv", package = 'study.wrangler')
  
  households <- entity_from_file(file_path, name = 'household')
  
  expect_true(
    households %>% quiet() %>% validate()
  )

  # lists should work
  expect_no_error(
    expect_message(
      households <- households %>%
        set_variable_metadata('Number.of.animals', hidden=list('variableTree', 'download')),
      "Made metadata update"
    )
  )
  
  # dump to VDI to check that it is JSONified
  # (you have to run this manually and check the entitygraph*.cache file)
  output_dir <- "./tmp/vdi-hidden-vars"
  study <- study_from_entities(list(households), name="hidden var test")
  expect_no_error(
    # capture messages about entity stable ID generation
    messages <- capture_messages(
      study %>% export_to_vdi(output_directory = output_dir)
    )
  )
  # Clean up
  unlink(output_dir, recursive = TRUE)
  
  
  # scalars should fail
  expect_error(
    households <- households %>%
      set_variable_metadata('Number.of.animals', hidden='download'),
    "because it takes a list"
  )
  
  # bad values should fail
  expect_error(
    households <- households %>%
      set_variable_metadata('Number.of.animals', hidden=list('downXload')),
    "Cannot assign value.s. 'downXload' to metadata field 'hidden'"
  )
  
});