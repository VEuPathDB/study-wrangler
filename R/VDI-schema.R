### TABLES and FIELDS ###

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

ancestors_table_field_def <- list(
  name = "####_stable_id",
  type = "SQL_VARCHAR",
  maxLength = "200",
  isNullable = "NO",
  cacheFileIndex = NA
)


attributegraph_table_fields <- list(
  list(
    cacheFileIndex = 0,
    isNullable = "YES",
    maxLength = "6",
    name = "bin_width_computed",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 1,
    isNullable = "YES",
    maxLength = "1",
    name = "bin_width_override",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 2,
    isNullable = "YES",
    maxLength = "11",
    name = "data_shape",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 3,
    isNullable = "YES",
    maxLength = "9",
    name = "data_type",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 4,
    isNullable = "YES",
    maxLength = "4000",
    name = "definition",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 5,
    isNullable = "YES",
    maxLength = "1500",
    name = "display_name",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 6,
    isNullable = "YES",
    name = "display_order",
    prec = 3,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 7,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_max",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 8,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_min",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 9,
    isNullable = "YES",
    maxLength = "20",
    name = "display_type",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 10,
    isNullable = "YES",
    name = "distinct_values_count",
    prec = 10,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 11,
    isNullable = "YES",
    name = "has_study_dependent_vocabulary",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 12,
    isNullable = "YES",
    name = "has_values",
    prec = 10,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 13,
    isNullable = "YES",
    maxLength = "64",
    name = "hidden",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 14,
    isNullable = "YES",
    name = "impute_zero",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 15,
    list(
      cacheFileIndex = 14,
      isNullable = "YES",
      name = "impute_zero",
      prec = 1,
      type = "SQL_NUMBER"
    ),
    list(
      cacheFileIndex = 14,
      isNullable = "YES",
      name = "impute_zero",
      prec = 1,
      type = "SQL_NUMBER"
    ),
    list(
      cacheFileIndex = 14,
      isNullable = "YES",
      name = "impute_zero",
      prec = 1,
      type = "SQL_NUMBER"
    ),
    list(
      cacheFileIndex = 14,
      isNullable = "YES",
      name = "impute_zero",
      prec = 1,
      type = "SQL_NUMBER"
    ),
    list(
      cacheFileIndex = 14,
      isNullable = "YES",
      name = "impute_zero",
      prec = 1,
      type = "SQL_NUMBER"
    ),
    isNullable = "YES",
    name = "is_featured",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 16,
    isNullable = "YES",
    name = "is_merge_key",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 17,
    isNullable = "YES",
    name = "is_multi_valued",
    prec = "NA",
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 18,
    isNullable = "YES",
    name = "is_repeated",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 19,
    isNullable = "YES",
    name = "is_temporal",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 20,
    isNullable = "YES",
    maxLength = "16",
    name = "lower_quartile",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 21,
    isNullable = "YES",
    maxLength = "16",
    name = "mean",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 22,
    isNullable = "YES",
    maxLength = "16",
    name = "median",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 23,
    isNullable = "YES",
    maxLength = "255",
    name = "parent_stable_id",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 24,
    isNullable = "YES",
    name = "precision",
    prec = 10,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 25,
    isNullable = "YES",
    maxLength = "4000",
    name = "provider_label",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 26,
    isNullable = "YES",
    maxLength = "16",
    name = "range_max",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 27,
    isNullable = "YES",
    maxLength = "16",
    name = "range_min",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 28,
    isNullable = "YES",
    maxLength = "1",
    name = "scale",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 29,
    isNullable = "NO",
    maxLength = "255",
    name = "stable_id",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 30,
    isNullable = "YES",
    maxLength = "1",
    name = "unit",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 31,
    isNullable = "YES",
    maxLength = "16",
    name = "upper_quartile",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 32,
    isNullable = "YES",
    maxLength = "200",
    name = "variable_spec_to_impute_zeroes_for",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 33,
    isNullable = "YES",
    maxLength = "66",
    name = "vocabulary",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 34,
    isNullable = "YES",
    maxLength = "200",
    name = "weighting_variable_spec",
    type = "SQL_VARCHAR"
  )
)

attributevalue_table_fields <- list(
  list(
    maxLength = 200,
    cacheFileIndex = 0,
    isNullable = "NO",
    type = "SQL_VARCHAR",
    name = "####_stable_id"
  ),
  list(
    name = "attribute_stable_id",
    isNullable = "NO",
    type = "SQL_VARCHAR",
    maxLength = 255,
    cacheFileIndex = 1
  ),
  list(
    cacheFileIndex = 2,
    maxLength = 1000,
    name = "string_value",
    type = "SQL_VARCHAR",
    isNullable = "YES"
  ),
  list(
    cacheFileIndex = 3,
    name = "number_value",
    prec = "NA",
    isNullable = "YES",
    type = "SQL_NUMBER"
  ),
  list(
    name = "date_value",
    isNullable = "YES",
    type = "SQL_DATE",
    cacheFileIndex = 4
  )
)

## Collections
#  (the lengths and precisions will be set from the data)

collections_table_fields <- list(
  list(
    cacheFileIndex = 0,
    isNullable = "NO",
    maxLength = "999",
    name = "stable_id",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 1,
    isNullable = "YES",
    maxLength = "999",
    name = "display_name",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 2,
    isNullable = "YES",
    prec = "9",
    name = "num_members",
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 3,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_min",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 4,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_max",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 5,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_min",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 6,
    isNullable = "YES",
    maxLength = "16",
    name = "display_range_max",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 7,
    isNullable = "YES",
    name = "impute_zero",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 8,
    isNullable = "NO",
    maxLength = "999",
    name = "data_type",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 9,
    isNullable = "NO",
    maxLength = "999",
    name = "data_shape",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 10,
    isNullable = "YES",
    maxLength = "999",
    name = "unit",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 11,
    isNullable = "YES",
    name = "precision",
    prec = 9,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 12,
    isNullable = "YES",
    name = "is_proportion",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 13,
    isNullable = "YES",
    name = "is_compositional",
    prec = 1,
    type = "SQL_NUMBER"
  ),
  list(
    cacheFileIndex = 13,
    isNullable = "YES",
    maxLength = "999",
    name = "normalization_method",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 14,
    isNullable = "YES",
    maxLength = "999",
    name = "member",
    type = "SQL_VARCHAR"
  ),
  list(
    cacheFileIndex = 15,
    isNullable = "YES",
    maxLength = "999",
    name = "member_plural",
    type = "SQL_VARCHAR"
  )
)

### INDEXES ###

# Primary key index for the ancestors table
ancestors_pkey_def <- list(
  isPrimary = 1,
  isUnique = 1,
  tableName = "####",
  type = "index",
  name = "####_pkey",
  orderedColumns = list("####_stable_id")
)

# Primary key index for the attributegraph table
attributegraph_pkey_def <- list(
  isPrimary = 1,
  isUnique = 1,
  tableName = "####",
  type = "index",
  name = "####_pkey",
  orderedColumns = list("stable_id")
)

# Secondary indexes for attributevalue table
# (it has no primary key)
attributevalue_index_defs <- list(
  list(
    isPrimary = 0,
    isUnique = 0,
    tableName = "####",
    type = "index",
    name = "####_ix1",
    orderedColumns = list("attribute_stable_id", "####_stable_id")
  ),
  list(
    isPrimary = 0,
    isUnique = 0,
    tableName = "####",
    type = "index",
    name = "####_ix2",
    orderedColumns = list("attribute_stable_id", "string_value", "####_stable_id")
  ),
  list(
    isPrimary = 0,
    isUnique = 0,
    tableName = "####",
    type = "index",
    name = "####_ix3",
    orderedColumns = list("attribute_stable_id", "date_value", "####_stable_id")
  ),
  list(
    isPrimary = 0,
    isUnique = 0,
    tableName = "####",
    type = "index",
    name = "####_ix4",
    orderedColumns = list("attribute_stable_id", "number_value", "####_stable_id")
  )
)
