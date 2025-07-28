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
  
  output_dir <- "./tmp/vdi"
  
  expect_no_error(
    # capture messages about entity stable ID generation
    messages <- capture_messages(
      study %>% export_to_vdi(output_directory = output_dir)
    )
  )

  ## with hydrated metadata caching this is not always true:
  # expect_true(all(grepl("Generating temporary stable_id for entity", messages)))
  
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
  
  # Verify install.json is non-empty
  install_json_path <- file.path(output_dir, "install.json")
  expect_true(file.size(install_json_path) > 0)
  
  # check the is_many_to_one_with_parent column is correct
  entitytypegraph.cache <- file.path(output_dir, 'entitytypegraph.cache')
  expect_equal(
    entitytypegraph.cache %>% readr::read_tsv(col_names = FALSE, show_col_types = FALSE) %>% pull(9),
    c(0,1,1)
  )
  
  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("A study with collections exports to VDI", {
  expect_no_error(
    study <- make_study_with_collections(name = 'my collections study')
  )
  
  expect_true(
    study %>% quiet() %>% validate()
  )
  
  output_dir <- "./tmp/vdi-collections"
  
  expect_no_error(
    # capture messages about entity stable ID generation
    messages <- capture_messages(
      study %>% export_to_vdi(output_directory = output_dir)
    )
  )
  
  # Verify the directory was created
  expect_true(dir.exists(output_dir))
  
  # List files in the output directory
  output_files <- list.files(output_dir, full.names = FALSE)
  
  # Verify the presence of files
  expect_true(length(grep("^ancestors.*\\.cache$", output_files)) == 3)  # 3 ancestors*.cache files
  expect_true(length(grep("^attributegraph.*\\.cache$", output_files)) == 3)  # 3 attributegraph*.cache files
  expect_true(length(grep("^attributevalue.*\\.cache$", output_files)) == 3)  # 3 attributevalue*.cache files
  expect_true(length(grep("^collection_.*\\.cache$", output_files)) > 0)  # collection_*.cache files
  expect_true(length(grep("^collectionattribute_.*\\.cache$", output_files)) > 0)  # collectionattribute_*.cache files
  expect_true("entitytypegraph.cache" %in% output_files)
  expect_true("install.json" %in% output_files)
  expect_true("study.cache" %in% output_files)
  
  # Verify install.json is non-empty
  install_json_path <- file.path(output_dir, "install.json")
  expect_true(file.size(install_json_path) > 0)
  
  # Clean up
  unlink(output_dir, recursive = TRUE)
})
