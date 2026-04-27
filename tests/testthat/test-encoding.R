test_that("entity_from_file handles ISO-8859-1 encoded files", {
  file_path <- system.file("extdata", "toy_example/householdsISO8859.tsv", package = 'study.wrangler')

  expect_no_error(
    result <- entity_from_file(file_path)
  )

  expect_s4_class(result, "Entity")

  names <- result@data$Owner.name
  expect_true("Müller" %in% names)
  expect_true("Ångström" %in% names)
  expect_true("Martínez" %in% names)
})

test_that("entity_from_file handles Windows-1252 encoded files", {
  file_path <- system.file("extdata", "toy_example/householdsWindows1252.tsv", package = 'study.wrangler')

  expect_no_error(
    result <- entity_from_file(file_path)
  )

  expect_s4_class(result, "Entity")

  # € (U+20AC) is byte 0x80 in Windows-1252 — absent from ISO-8859-1 and invalid UTF-8
  notes <- result@data$Notes
  expect_true(any(grepl("€", notes, fixed = TRUE)))
})
