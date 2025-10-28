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

  # Snapshot testing for VDI export consistency
  # ============================================
  # Snapshot tests capture the exact output of the VDI export and store it in
  # tests/testthat/_snaps/Study-export-VDI.md for regression testing.
  #
  # On first run, snapshots are created. On subsequent runs, the output is compared
  # against the stored snapshot. If they differ, the test fails with a clear diff.
  #
  # To regenerate snapshots (e.g., after intentional format changes):
  #   1. Delete tests/testthat/_snaps/Study-export-VDI.md
  #   2. Run this test - it will create a new baseline
  #   3. Review the new snapshot carefully before committing
  #
  # See: https://testthat.r-lib.org/articles/snapshotting.html

  # Parse and snapshot install.json
  install_json <- jsonlite::fromJSON(install_json_path)
  expect_snapshot({
    cat("=== install.json structure ===\n")
    cat(jsonlite::toJSON(install_json, pretty = TRUE, auto_unbox = TRUE))
  })

  # Snapshot all cache files (raw TSV content)
  cache_files <- sort(list.files(output_dir, pattern = "\\.cache$", full.names = TRUE))

  expect_snapshot({
    cat("=== Cache files (raw TSV) ===\n")
    for (f in cache_files) {
      cat("\n## ", basename(f), "\n")
      cat(readLines(f), sep = "\n")
    }
  })

  # Clean up
  unlink(output_dir, recursive = TRUE)
})
