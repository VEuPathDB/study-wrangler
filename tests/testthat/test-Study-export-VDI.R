test_that("Study exports to VDI artifact", {
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  expect_equal(
    study %>% get_study_name(),
    'my study'
  )
  
  expect_error(
    study %>% export_to_vdi(),
    "output_directory not provided"
  )
  
  output_dir <- "./tmp"
  
  expect_no_error(
    # capture messages about entity stable ID generation
    messages <- capture_messages(
      study %>% export_to_vdi(output_directory = output_dir)
    )
  )

  expect_true(all(grepl("Generating temporary stable_id for entity", messages)))
  
  # Verify the directory was created
  expect_true(dir.exists(output_dir))
  
  # more tests here
  
  
  # Clean up
  unlink(output_dir, recursive = TRUE)
})
