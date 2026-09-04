# Ordinal variables are stored as factors regardless of their data_type.
# These tests pin the code paths that must read a factor's labels rather than
# assuming the column is a plain character or numeric vector.

make_ordinal_stf <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    yaml::as.yaml(list(name = "ordinal demo", entities = list("participant"))),
    file.path(dir, "study.yaml")
  )

  metadata <- list(
    name = "participant",
    display_name = "Participant",
    display_name_plural = "Participants",
    id_columns = list(list(
      id_column = "participant.id", entity_name = "participant", entity_level = 0L
    )),
    variables = list(
      list(
        variable = "facility_code",
        display_name = "Facility code",
        data_type = "integer",
        data_shape = "ordinal",
        ordinal_levels = list("101", "102", "234"),
        provider_label = list("facility")
      ),
      list(
        variable = "age_group",
        display_name = "Age group",
        data_type = "string",
        data_shape = "ordinal",
        ordinal_levels = list("0-6 months", "6-12 months"),
        provider_label = list("age")
      )
    )
  )
  writeLines(yaml::as.yaml(metadata), file.path(dir, "entity-participant.yaml"))

  # the Descriptors marker is what makes this the wide STF format
  readr::write_tsv(
    tibble::tibble(
      "participant.id \\\\ Descriptors" = c("p1", "p2", "p3"),
      facility_code = c("101", "234", "102"),
      age_group = c("0-6 months", "6-12 months", "0-6 months")
    ),
    file.path(dir, "entity-participant.tsv")
  )

  dir
}

test_that("integer ordinals are not reported as non-integer", {
  stf_dir <- make_ordinal_stf(file.path(tempdir(), "ordinal-stf-int"))
  entity <- study_from_stf(stf_dir, validate = FALSE) %>% get_entity("participant")

  # the column really is a factor whose labels, not codes, carry the values
  expect_s3_class(entity@data$facility_code, "factor")
  expect_equal(levels(entity@data$facility_code), c("101", "102", "234"))

  result <- validate_entity_integer_data_types(entity)
  expect_true(result$valid)
})

test_that("string length validation tolerates factor columns", {
  stf_dir <- make_ordinal_stf(file.path(tempdir(), "ordinal-stf-str"))
  entity <- study_from_stf(stf_dir, validate = FALSE) %>% get_entity("participant")

  expect_s3_class(entity@data$age_group, "factor")

  result <- validate_entity_string_value_length(entity)
  expect_true(result$valid)
})

test_that("a study with ordinal variables validates under the eda profile", {
  stf_dir <- make_ordinal_stf(file.path(tempdir(), "ordinal-stf-validate"))
  study <- study_from_stf(stf_dir, validate = FALSE) %>% quiet()

  expect_true(validate(study, profiles = c("baseline", "eda")))
})

test_that("integer ordinals export to VDI as their label values", {
  stf_dir <- make_ordinal_stf(file.path(tempdir(), "ordinal-stf-vdi"))
  vdi_dir <- file.path(tempdir(), "ordinal-vdi")
  unlink(vdi_dir, recursive = TRUE)
  dir.create(vdi_dir, recursive = TRUE, showWarnings = FALSE)

  study <- study_from_stf(stf_dir, validate = FALSE) %>% quiet()
  expect_no_error(export_to_vdi(study, vdi_dir))

  attribute_file <- list.files(vdi_dir, pattern = "^attributevalue_", full.names = TRUE)
  expect_length(attribute_file, 1)

  # columns are: id, attribute_stable_id, string_value, number_value, date_value
  values <- readr::read_tsv(
    attribute_file, col_names = FALSE, col_types = readr::cols(.default = "c")
  )
  numbers <- values$X4[!is.na(values$X4)]

  # facility_code is the only numeric variable, so its values stand alone here;
  # they must be the labels 101/102/234, never the factor codes 1/2/3
  expect_equal(sort(unique(as.numeric(numbers))), c(101, 102, 234))
})
