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
  
  # List files in the output directory
  output_files <- list.files(output_dir, full.names = FALSE)
  
  # Verify the presence of files
  expect_true(length(grep("^ancestors.*\\.cache$", output_files)) == 3)  # 3 ancestors*.cache files
  expect_true(length(grep("^attributegraph.*\\.cache$", output_files)) == 3)  # 3 attributegraph*.cache files
  expect_true(length(grep("^attributevalue.*\\.cache$", output_files)) == 3)  # 3 attributevalue*.cache files
  expect_true("entitytypegraph.cache" %in% output_files)
  expect_true("install.json" %in% output_files)
  expect_true("study.cache" %in% output_files)
  
  # Clean up
  unlink(output_dir, recursive = TRUE)
})
