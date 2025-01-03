vdi_study_table_def <- list(
  name = "study",
  type = "table",
  is_preexisting_table = TRUE,
  fields = list(
    list(
      maxLength = "32",
      name = "user_dataset_id",
      isNullable = "No",
      type = "SQL_VARCHAR",
      macro = "USER_DATASET_ID",
      cacheFileIndex = 0
    ),
    list(
      maxLength = "200",
      cacheFileIndex = 1,
      isNullable = "No",
      type = "SQL_VARCHAR",
      name = "stable_id"
    ),
    list(
      name = "internal_abbrev",
      isNullable = "Yes",
      type = "SQL_VARCHAR",
      maxLength = "75",
      cacheFileIndex = 2
    ),
    list(
      name = "modification_date",
      type = "SQL_DATE",
      isNullable = "No",
      cacheFileIndex = 3,
      macro = "MODIFICATION_DATE"
    )
  )
)

vdi_entitytypegraph_table_def <- list(
  type = "table",
  is_preexisting_table = TRUE,
  name = "entitytypegraph",
  fields = list(
    list(
      name = "stable_id",
      type = "SQL_VARCHAR",
      isNullable = "No",
      cacheFileIndex = 0,
      maxLength = "255"
    ),
    list(
      isNullable = "No",
      type = "SQL_VARCHAR",
      name = "study_stable_id",
      maxLength = "200",
      cacheFileIndex = 1
    ),
    list(
      name = "parent_stable_id",
      isNullable = "Yes",
      type = "SQL_VARCHAR",
      maxLength = "255",
      cacheFileIndex = 2
    ),
    list(
      cacheFileIndex = 3,
      maxLength = "50",
      name = "internal_abbrev",
      type = "SQL_VARCHAR",
      isNullable = "No"
    ),
    list(
      isNullable = "Yes",
      type = "SQL_VARCHAR",
      name = "description",
      maxLength = "4000",
      cacheFileIndex = 4
    ),
    list(
      maxLength = "200",
      cacheFileIndex = 5,
      name = "display_name",
      isNullable = "No",
      type = "SQL_VARCHAR"
    ),
    list(
      cacheFileIndex = 6,
      maxLength = "200",
      name = "display_name_plural",
      type = "SQL_VARCHAR",
      isNullable = "Yes"
    ),
    list(
      name = "has_attribute_collections",
      isNullable = "Yes",
      prec = "1",
      type = "SQL_NUMBER",
      cacheFileIndex = 7
    ),
    list(
      isNullable = "Yes",
      prec = "1",
      type = "SQL_NUMBER",
      name = "is_many_to_one_with_parent",
      cacheFileIndex = 8
    ),
    list(
      name = "cardinality",
      type = "SQL_NUMBER",
      isNullable = "Yes",
      prec = "38",
      cacheFileIndex = 9
    )
  )
)
