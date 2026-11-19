# read_tsv()'s trim_ws leaves quoted cells alone, so a quoted value ending in a
# line break must be trimmed on read to match its trimmed vocabulary_order entry.

make_line_break_stf <- function(output_dir) {
  sites <- entity_from_tibble(
    tibble(
      site = c("S1", "S2", "S3"),
      country = c("Kenya", "Mali", "Kenya"),
      habitat = c("forest", "river", "forest")
    ),
    name = "site",
    quiet = TRUE
  ) %>%
    set_variable_metadata("country", display_name = "country",
                          vocabulary_order = list("Kenya", "Mali")) %>%
    set_variable_metadata("habitat", display_name = "habitat")

  study_from_entities(list(sites), name = "line breaks") %>%
    quiet() %>%
    export_to_stf(output_dir)

  tsv_path <- file.path(output_dir, "entity-site.tsv")
  tsv <- readChar(tsv_path, file.size(tsv_path), useBytes = TRUE)
  tsv <- sub("\tKenya\t", "\t\"Kenya\n\"\t", tsv, fixed = TRUE)
  tsv <- sub("\tMali\t", "\t\"Mali\r\n\"\t", tsv, fixed = TRUE)
  writeBin(charToRaw(tsv), tsv_path)
}

test_that("quoted data cells ending in a line break are trimmed to match vocabulary_order", {
  stf_dir <- withr::local_tempdir()
  make_line_break_stf(stf_dir)
  raw_tsv <- readChar(file.path(stf_dir, "entity-site.tsv"), 1e5, useBytes = TRUE)
  expect_match(raw_tsv, "\"Kenya\n\"", fixed = TRUE)
  expect_match(raw_tsv, "\"Mali\r\n\"", fixed = TRUE)

  expect_no_warning(study <- study_from_stf(stf_dir) %>% quiet())
  sites <- study %>% get_entity("site")
  expect_equal(sites %>% get_data() %>% pull(country), c("Kenya", "Mali", "Kenya"))
  expect_true(study %>% validate(profiles = c("baseline", "eda")))
})
