# A study with collections exports to VDI

    Code
      cat("=== install.json structure ===\n")
    Output
      === install.json structure ===
    Code
      cat(jsonlite::toJSON(install_json, pretty = TRUE, auto_unbox = TRUE))
    Output
      [
        {
          "name": "study",
          "type": "table",
          "is_preexisting_table": true,
          "fields": [
            {
              "maxLength": "32",
              "name": "user_dataset_id",
              "isNullable": "No",
              "type": "SQL_VARCHAR",
              "macro": "USER_DATASET_ID",
              "cacheFileIndex": 0
            },
            {
              "maxLength": "200",
              "name": "stable_id",
              "isNullable": "No",
              "type": "SQL_VARCHAR",
              "cacheFileIndex": 1
            },
            {
              "maxLength": "75",
              "name": "internal_abbrev",
              "isNullable": "Yes",
              "type": "SQL_VARCHAR",
              "cacheFileIndex": 2
            },
            {
              "name": "modification_date",
              "isNullable": "No",
              "type": "SQL_DATE",
              "macro": "MODIFICATION_DATE",
              "cacheFileIndex": 3
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "entitytypegraph",
          "type": "table",
          "is_preexisting_table": true,
          "fields": [
            {
              "name": "stable_id",
              "type": "SQL_VARCHAR",
              "isNullable": "No",
              "cacheFileIndex": 0,
              "maxLength": "255"
            },
            {
              "name": "study_stable_id",
              "type": "SQL_VARCHAR",
              "isNullable": "No",
              "cacheFileIndex": 1,
              "maxLength": "200"
            },
            {
              "name": "parent_stable_id",
              "type": "SQL_VARCHAR",
              "isNullable": "Yes",
              "cacheFileIndex": 2,
              "maxLength": "255"
            },
            {
              "name": "internal_abbrev",
              "type": "SQL_VARCHAR",
              "isNullable": "No",
              "cacheFileIndex": 3,
              "maxLength": "50"
            },
            {
              "name": "description",
              "type": "SQL_VARCHAR",
              "isNullable": "Yes",
              "cacheFileIndex": 4,
              "maxLength": "4000"
            },
            {
              "name": "display_name",
              "type": "SQL_VARCHAR",
              "isNullable": "No",
              "cacheFileIndex": 5,
              "maxLength": "200"
            },
            {
              "name": "display_name_plural",
              "type": "SQL_VARCHAR",
              "isNullable": "Yes",
              "cacheFileIndex": 6,
              "maxLength": "200"
            },
            {
              "name": "has_attribute_collections",
              "type": "SQL_NUMBER",
              "isNullable": "Yes",
              "cacheFileIndex": 7,
              "prec": "1"
            },
            {
              "name": "is_many_to_one_with_parent",
              "type": "SQL_NUMBER",
              "isNullable": "Yes",
              "cacheFileIndex": 8,
              "prec": "1"
            },
            {
              "name": "cardinality",
              "type": "SQL_NUMBER",
              "isNullable": "Yes",
              "cacheFileIndex": 9,
              "prec": "38"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "ancestors_sde41bbea90_househld",
          "type": "table",
          "fields": [
            {
              "name": "househld_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 0
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "ancestors_sde41bbea90_househld_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "ancestors_sde41bbea90_househld",
          "orderedColumns": "househld_stable_id"
        },
        {
          "name": "attributegraph_sde41bbea90_househld",
          "type": "table",
          "fields": [
            {
              "cacheFileIndex": 0,
              "isNullable": "YES",
              "maxLength": 4,
              "name": "bin_width_computed",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 1,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "bin_width_override",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "maxLength": 11,
              "name": "data_shape",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "maxLength": 7,
              "name": "data_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "definition",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 5,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_name",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 6,
              "isNullable": "YES",
              "name": "display_order",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 7,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 8,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 9,
              "isNullable": "YES",
              "maxLength": 7,
              "name": "display_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 10,
              "isNullable": "YES",
              "name": "distinct_values_count",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 11,
              "isNullable": "YES",
              "name": "has_study_dependent_vocabulary",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 12,
              "isNullable": "YES",
              "name": "has_values",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 13,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "hidden",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 14,
              "isNullable": "YES",
              "name": "impute_zero",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 15,
              "isNullable": "YES",
              "name": "is_featured",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 16,
              "isNullable": "YES",
              "name": "is_merge_key",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 17,
              "isNullable": "YES",
              "name": "is_multi_valued",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 18,
              "isNullable": "YES",
              "name": "is_repeated",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 19,
              "isNullable": "YES",
              "name": "is_temporal",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 20,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "lower_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 21,
              "isNullable": "YES",
              "maxLength": 16,
              "name": "mean",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 22,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "median",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 23,
              "isNullable": "YES",
              "maxLength": 12,
              "name": "parent_stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 24,
              "isNullable": "YES",
              "name": "precision",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 25,
              "isNullable": "YES",
              "maxLength": 25,
              "name": "provider_label",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 26,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 27,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 28,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "scale",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 29,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 30,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "unit",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 31,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "upper_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 32,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "variable_spec_to_impute_zeroes_for",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 33,
              "isNullable": "YES",
              "maxLength": 21,
              "name": "vocabulary",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 34,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "weighting_variable_spec",
              "type": "SQL_VARCHAR"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributegraph_sde41bbea90_househld_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "attributegraph_sde41bbea90_househld",
          "orderedColumns": "stable_id"
        },
        {
          "name": "attributevalue_sde41bbea90_househld",
          "type": "table",
          "fields": [
            {
              "maxLength": 200,
              "cacheFileIndex": 0,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "househld_stable_id"
            },
            {
              "maxLength": 255,
              "cacheFileIndex": 1,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "attribute_stable_id"
            },
            {
              "maxLength": 8,
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "type": "SQL_VARCHAR",
              "name": "string_value"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "type": "SQL_NUMBER",
              "name": "number_value"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "type": "SQL_DATE",
              "name": "date_value"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributevalue_sde41bbea90_househld_ix1",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_househld",
          "orderedColumns": ["attribute_stable_id", "househld_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_househld_ix2",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_househld",
          "orderedColumns": ["attribute_stable_id", "string_value", "househld_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_househld_ix3",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_househld",
          "orderedColumns": ["attribute_stable_id", "date_value", "househld_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_househld_ix4",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_househld",
          "orderedColumns": ["attribute_stable_id", "number_value", "househld_stable_id"]
        },
        {
          "name": "ancestors_sde41bbea90_partcpnt",
          "type": "table",
          "fields": [
            {
              "name": "househld_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 0
            },
            {
              "name": "partcpnt_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 1
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "ancestors_sde41bbea90_partcpnt_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "ancestors_sde41bbea90_partcpnt",
          "orderedColumns": "partcpnt_stable_id"
        },
        {
          "name": "attributegraph_sde41bbea90_partcpnt",
          "type": "table",
          "fields": [
            {
              "cacheFileIndex": 0,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "bin_width_computed",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 1,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "bin_width_override",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "maxLength": 11,
              "name": "data_shape",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "maxLength": 6,
              "name": "data_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "definition",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 5,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_name",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 6,
              "isNullable": "YES",
              "name": "display_order",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 7,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 8,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 9,
              "isNullable": "YES",
              "maxLength": 7,
              "name": "display_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 10,
              "isNullable": "YES",
              "name": "distinct_values_count",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 11,
              "isNullable": "YES",
              "name": "has_study_dependent_vocabulary",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 12,
              "isNullable": "YES",
              "name": "has_values",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 13,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "hidden",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 14,
              "isNullable": "YES",
              "name": "impute_zero",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 15,
              "isNullable": "YES",
              "name": "is_featured",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 16,
              "isNullable": "YES",
              "name": "is_merge_key",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 17,
              "isNullable": "YES",
              "name": "is_multi_valued",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 18,
              "isNullable": "YES",
              "name": "is_repeated",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 19,
              "isNullable": "YES",
              "name": "is_temporal",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 20,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "lower_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 21,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "mean",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 22,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "median",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 23,
              "isNullable": "YES",
              "maxLength": 12,
              "name": "parent_stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 24,
              "isNullable": "YES",
              "name": "precision",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 25,
              "isNullable": "YES",
              "maxLength": 15,
              "name": "provider_label",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 26,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 27,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 28,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "scale",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 29,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 30,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "unit",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 31,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "upper_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 32,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "variable_spec_to_impute_zeroes_for",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 33,
              "isNullable": "YES",
              "maxLength": 64,
              "name": "vocabulary",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 34,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "weighting_variable_spec",
              "type": "SQL_VARCHAR"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributegraph_sde41bbea90_partcpnt_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "attributegraph_sde41bbea90_partcpnt",
          "orderedColumns": "stable_id"
        },
        {
          "name": "attributevalue_sde41bbea90_partcpnt",
          "type": "table",
          "fields": [
            {
              "maxLength": 200,
              "cacheFileIndex": 0,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "partcpnt_stable_id"
            },
            {
              "maxLength": 255,
              "cacheFileIndex": 1,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "attribute_stable_id"
            },
            {
              "maxLength": 8,
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "type": "SQL_VARCHAR",
              "name": "string_value"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "type": "SQL_NUMBER",
              "name": "number_value"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "type": "SQL_DATE",
              "name": "date_value"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributevalue_sde41bbea90_partcpnt_ix1",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_partcpnt",
          "orderedColumns": ["attribute_stable_id", "partcpnt_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_partcpnt_ix2",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_partcpnt",
          "orderedColumns": ["attribute_stable_id", "string_value", "partcpnt_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_partcpnt_ix3",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_partcpnt",
          "orderedColumns": ["attribute_stable_id", "date_value", "partcpnt_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_partcpnt_ix4",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_partcpnt",
          "orderedColumns": ["attribute_stable_id", "number_value", "partcpnt_stable_id"]
        },
        {
          "name": "ancestors_sde41bbea90_observtn",
          "type": "table",
          "fields": [
            {
              "name": "househld_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 0
            },
            {
              "name": "partcpnt_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 1
            },
            {
              "name": "observtn_stable_id",
              "type": "SQL_VARCHAR",
              "maxLength": "200",
              "isNullable": "NO",
              "cacheFileIndex": 2
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "ancestors_sde41bbea90_observtn_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "ancestors_sde41bbea90_observtn",
          "orderedColumns": "observtn_stable_id"
        },
        {
          "name": "attributegraph_sde41bbea90_observtn",
          "type": "table",
          "fields": [
            {
              "cacheFileIndex": 0,
              "isNullable": "YES",
              "maxLength": 5,
              "name": "bin_width_computed",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 1,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "bin_width_override",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "data_shape",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "maxLength": 8,
              "name": "data_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "maxLength": 33,
              "name": "definition",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 5,
              "isNullable": "YES",
              "maxLength": 33,
              "name": "display_name",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 6,
              "isNullable": "YES",
              "name": "display_order",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 7,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 8,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 9,
              "isNullable": "YES",
              "maxLength": 7,
              "name": "display_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 10,
              "isNullable": "YES",
              "name": "distinct_values_count",
              "type": "SQL_NUMBER",
              "prec": 2
            },
            {
              "cacheFileIndex": 11,
              "isNullable": "YES",
              "name": "has_study_dependent_vocabulary",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 12,
              "isNullable": "YES",
              "name": "has_values",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 13,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "hidden",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 14,
              "isNullable": "YES",
              "name": "impute_zero",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 15,
              "isNullable": "YES",
              "name": "is_featured",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 16,
              "isNullable": "YES",
              "name": "is_merge_key",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 17,
              "isNullable": "YES",
              "name": "is_multi_valued",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 18,
              "isNullable": "YES",
              "name": "is_repeated",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 19,
              "isNullable": "YES",
              "name": "is_temporal",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 20,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "lower_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 21,
              "isNullable": "YES",
              "maxLength": 17,
              "name": "mean",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 22,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "median",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 23,
              "isNullable": "YES",
              "maxLength": 12,
              "name": "parent_stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 24,
              "isNullable": "YES",
              "name": "precision",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 25,
              "isNullable": "YES",
              "maxLength": 23,
              "name": "provider_label",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 26,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 27,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 28,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "scale",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 29,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 30,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "unit",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 31,
              "isNullable": "YES",
              "maxLength": 10,
              "name": "upper_quartile",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 32,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "variable_spec_to_impute_zeroes_for",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 33,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "vocabulary",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 34,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "weighting_variable_spec",
              "type": "SQL_VARCHAR"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributegraph_sde41bbea90_observtn_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "attributegraph_sde41bbea90_observtn",
          "orderedColumns": "stable_id"
        },
        {
          "name": "attributevalue_sde41bbea90_observtn",
          "type": "table",
          "fields": [
            {
              "maxLength": 200,
              "cacheFileIndex": 0,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "observtn_stable_id"
            },
            {
              "maxLength": 255,
              "cacheFileIndex": 1,
              "isNullable": "NO",
              "type": "SQL_VARCHAR",
              "name": "attribute_stable_id"
            },
            {
              "maxLength": 1,
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "type": "SQL_VARCHAR",
              "name": "string_value"
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "type": "SQL_NUMBER",
              "name": "number_value"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "type": "SQL_DATE",
              "name": "date_value"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "attributevalue_sde41bbea90_observtn_ix1",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_observtn",
          "orderedColumns": ["attribute_stable_id", "observtn_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_observtn_ix2",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_observtn",
          "orderedColumns": ["attribute_stable_id", "string_value", "observtn_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_observtn_ix3",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_observtn",
          "orderedColumns": ["attribute_stable_id", "date_value", "observtn_stable_id"]
        },
        {
          "name": "attributevalue_sde41bbea90_observtn_ix4",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 0,
          "tableName": "attributevalue_sde41bbea90_observtn",
          "orderedColumns": ["attribute_stable_id", "number_value", "observtn_stable_id"]
        },
        {
          "name": "collection_sde41bbea90_observtn",
          "type": "table",
          "fields": [
            {
              "cacheFileIndex": 0,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 1,
              "isNullable": "YES",
              "maxLength": 33,
              "name": "display_name",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 2,
              "isNullable": "YES",
              "name": "num_members",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 3,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 4,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "display_range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 5,
              "isNullable": "YES",
              "maxLength": 2,
              "name": "range_min",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 6,
              "isNullable": "YES",
              "maxLength": 3,
              "name": "range_max",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 7,
              "isNullable": "YES",
              "name": "impute_zero",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 8,
              "isNullable": "NO",
              "maxLength": 7,
              "name": "data_type",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 9,
              "isNullable": "NO",
              "maxLength": 10,
              "name": "data_shape",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 10,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "unit",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 11,
              "isNullable": "YES",
              "name": "precision",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 12,
              "isNullable": "YES",
              "name": "is_proportion",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 13,
              "isNullable": "YES",
              "name": "is_compositional",
              "type": "SQL_NUMBER",
              "prec": 1
            },
            {
              "cacheFileIndex": 13,
              "isNullable": "YES",
              "maxLength": 1,
              "name": "normalization_method",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 14,
              "isNullable": "YES",
              "maxLength": 11,
              "name": "member",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 15,
              "isNullable": "YES",
              "maxLength": 12,
              "name": "member_plural",
              "type": "SQL_VARCHAR"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "collection_sde41bbea90_observtn_pkey",
          "type": "index",
          "fields": {},
          "isPrimary": 1,
          "isUnique": 1,
          "tableName": "collection_sde41bbea90_observtn",
          "orderedColumns": "stable_id"
        },
        {
          "name": "collectionattribute_sde41bbea90_observtn",
          "type": "table",
          "fields": [
            {
              "cacheFileIndex": 0,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "collection_stable_id",
              "type": "SQL_VARCHAR"
            },
            {
              "cacheFileIndex": 1,
              "isNullable": "NO",
              "maxLength": 12,
              "name": "attribute_stable_id",
              "type": "SQL_VARCHAR"
            }
          ],
          "orderedColumns": {}
        },
        {
          "name": "collectionattribute_sde41bbea90_observtn_idx1",
          "type": "index",
          "fields": {},
          "isPrimary": 0,
          "isUnique": 1,
          "tableName": "collectionattribute_sde41bbea90_observtn",
          "orderedColumns": "attribute_stable_id"
        }
      ]

---

    Code
      cat("=== Cache files (raw TSV) ===\n")
    Output
      === Cache files (raw TSV) ===
    Code
      for (f in cache_files) {
        cat("\n## ", basename(f), "\n")
        lines <- readLines(f)
        cat(sort(lines), sep = "\n")
      }
    Output
      
      ##  ancestors_sde41bbea90_househld.cache 
      H001
      H002
      H003
      
      ##  ancestors_sde41bbea90_observtn.cache 
      H001	H001-P1	H001-P1-Obs1
      H001	H001-P2	H001-P2-Obs1
      H001	H001-P2	H001-P2-Obs2
      H001	H001-P3	H001-P3-Obs1
      H002	H002-P1	H002-P1-Obs1
      H002	H002-P1	H002-P1-Obs2
      H002	H002-P2	H002-P2-Obs1
      H003	H003-P1	H003-P1-Obs1
      H003	H003-P2	H003-P2-Obs1
      H003	H003-P3	H003-P3-Obs1
      H003	H003-P3	H003-P3-Obs2
      
      ##  ancestors_sde41bbea90_partcpnt.cache 
      H001	H001-P1
      H001	H001-P2
      H001	H001-P3
      H002	H002-P1
      H002	H002-P2
      H003	H003-P1
      H003	H003-P2
      H003	H003-P3
      
      ##  attributegraph_sde41bbea90_househld.cache 
      		categorical	string						default	2		1			0	0	0	0	0				ENT_6a7855ea		["Construction material"]				VAR_3b2f7286				["Concrete","Timber"]	
      		categorical	string						default	2		1			0	0	0	0	0				ENT_6a7855ea		["Owns property"]				VAR_94999355				["No","Yes"]	
      1		continuous	integer						default	2		1			0	0	0	0	0	3	3.33333333333333	3	ENT_6a7855ea	0	["Number of animals"]	4	3		VAR_1513b065		3.5			
      week		continuous	date						default	3		1			0	0	0	0	0	2021-02-03	2021-02-15	2021-02-28	ENT_6a7855ea		["Enrollment date"]	2021-03-13	2021-01-09		VAR_ec493713		2021-03-06			
      
      ##  attributegraph_sde41bbea90_observtn.cache 
      			category	integer-based anatomical measures	integer-based anatomical measures				default			0			0	0	0	0	0				ENT_7c02c7e8						VAR_60b0b28d					
      1		continuous	integer						default	4		1			0	0	0	0	0	1	0.888888888888889	1	ENT_7c02c7e8	0	["Teeth brushed today"]	2	0		VAR_74213e92		1			
      4.812		continuous	number						default	8		1			0	0	0	0	0	16.66	21.8671428571429	19.83	ENT_7c02c7e8	2	["MUAC (cm)"]	31.84	12.59		VAR_156a6e1b		27.745			
      7		continuous	integer						default	11		1			0	0	0	0	0	131.5	145.090909090909	140	VAR_60b0b28d	0	["Height (cm)"]	175	127		VAR_7d2a26d7		155			
      9		continuous	integer						default	11		1			0	0	0	0	0	36.5	51.5454545454545	45	VAR_60b0b28d	0	["Weight (kg)"]	91	32		VAR_0789e56a		62.5			
      month		continuous	date						default	11		1			0	0	0	0	0	2023-03-26	2023-06-11	2023-05-04	ENT_7c02c7e8		["Observation date"]	2023-11-30	2023-01-05		VAR_5c3c88eb		2023-09-03			
      
      ##  attributegraph_sde41bbea90_partcpnt.cache 
      		categorical	string						default	2		1			0	0	0	0	0				ENT_7a211045		["Sex"]				VAR_e301dd60				["Female","Male"]	
      		categorical	string						default	3		1			0	0	0	0	0				ENT_7a211045		["Family Role"]				VAR_c4dab5fd				["Child","Parent","Relative"]	
      		categorical	string						default	6		1			0	0	0	0	0				ENT_7a211045		["Nickname"]				VAR_ce2bd99c				["Bobby","Chuck","Di","Hells","Micky"]	
      		categorical	string						default	8		1			0	0	0	0	0				ENT_7a211045		["Name"]				VAR_709a2322				["Alice","Anna","Bob","Charlie","Diana","Eve","Helen","Michael"]	
      
      ##  attributevalue_sde41bbea90_househld.cache 
      H001	VAR_1513b065		4	
      H001	VAR_3b2f7286	Concrete		
      H001	VAR_94999355	Yes		
      H001	VAR_ec493713			2021-01-09
      H002	VAR_1513b065		3	
      H002	VAR_3b2f7286	Timber		
      H002	VAR_94999355	No		
      H002	VAR_ec493713			2021-02-28
      H003	VAR_1513b065		3	
      H003	VAR_3b2f7286	Concrete		
      H003	VAR_94999355	Yes		
      H003	VAR_ec493713			2021-03-13
      
      ##  attributevalue_sde41bbea90_observtn.cache 
      H001-P1-Obs1	VAR_0789e56a		35	
      H001-P1-Obs1	VAR_156a6e1b		15.82	
      H001-P1-Obs1	VAR_5c3c88eb			2023-09-29
      H001-P1-Obs1	VAR_74213e92		1	
      H001-P1-Obs1	VAR_7d2a26d7		145	
      H001-P2-Obs1	VAR_0789e56a		47	
      H001-P2-Obs1	VAR_5c3c88eb			2023-05-04
      H001-P2-Obs1	VAR_74213e92		1	
      H001-P2-Obs1	VAR_7d2a26d7		135	
      H001-P2-Obs2	VAR_0789e56a		80	
      H001-P2-Obs2	VAR_156a6e1b		28.81	
      H001-P2-Obs2	VAR_5c3c88eb			2023-11-30
      H001-P2-Obs2	VAR_74213e92		0	
      H001-P2-Obs2	VAR_7d2a26d7		129	
      H001-P3-Obs1	VAR_0789e56a		51	
      H001-P3-Obs1	VAR_5c3c88eb			2023-04-21
      H001-P3-Obs1	VAR_74213e92		1	
      H001-P3-Obs1	VAR_7d2a26d7		133	
      H002-P1-Obs1	VAR_0789e56a		32	
      H002-P1-Obs1	VAR_156a6e1b		26.68	
      H002-P1-Obs1	VAR_5c3c88eb			2023-08-08
      H002-P1-Obs1	VAR_7d2a26d7		175	
      H002-P1-Obs2	VAR_0789e56a		34	
      H002-P1-Obs2	VAR_156a6e1b		12.59	
      H002-P1-Obs2	VAR_5c3c88eb			2023-10-18
      H002-P1-Obs2	VAR_74213e92		1	
      H002-P1-Obs2	VAR_7d2a26d7		172	
      H002-P2-Obs1	VAR_0789e56a		74	
      H002-P2-Obs1	VAR_156a6e1b		19.83	
      H002-P2-Obs1	VAR_5c3c88eb			2023-02-28
      H002-P2-Obs1	VAR_74213e92		2	
      H002-P2-Obs1	VAR_7d2a26d7		160	
      H003-P1-Obs1	VAR_0789e56a		40	
      H003-P1-Obs1	VAR_156a6e1b		31.84	
      H003-P1-Obs1	VAR_5c3c88eb			2023-05-02
      H003-P1-Obs1	VAR_74213e92		1	
      H003-P1-Obs1	VAR_7d2a26d7		150	
      H003-P2-Obs1	VAR_0789e56a		38	
      H003-P2-Obs1	VAR_156a6e1b		17.5	
      H003-P2-Obs1	VAR_5c3c88eb			2023-01-30
      H003-P2-Obs1	VAR_74213e92		0	
      H003-P2-Obs1	VAR_7d2a26d7		140	
      H003-P3-Obs1	VAR_0789e56a		45	
      H003-P3-Obs1	VAR_5c3c88eb			2023-01-05
      H003-P3-Obs1	VAR_7d2a26d7		130	
      H003-P3-Obs2	VAR_0789e56a		91	
      H003-P3-Obs2	VAR_5c3c88eb			2023-08-07
      H003-P3-Obs2	VAR_74213e92		1	
      H003-P3-Obs2	VAR_7d2a26d7		127	
      
      ##  attributevalue_sde41bbea90_partcpnt.cache 
      H001-P1	VAR_709a2322	Alice		
      H001-P1	VAR_c4dab5fd	Relative		
      H001-P1	VAR_ce2bd99c	Di		
      H001-P1	VAR_e301dd60	Female		
      H001-P2	VAR_709a2322	Bob		
      H001-P2	VAR_c4dab5fd	Child		
      H001-P2	VAR_ce2bd99c	Bobby		
      H001-P2	VAR_e301dd60	Male		
      H001-P3	VAR_709a2322	Diana		
      H001-P3	VAR_c4dab5fd	Parent		
      H001-P3	VAR_e301dd60	Female		
      H002-P1	VAR_709a2322	Charlie		
      H002-P1	VAR_c4dab5fd	Child		
      H002-P1	VAR_ce2bd99c	Chuck		
      H002-P1	VAR_e301dd60	Male		
      H002-P2	VAR_709a2322	Anna		
      H002-P2	VAR_c4dab5fd	Relative		
      H002-P2	VAR_e301dd60	Female		
      H003-P1	VAR_709a2322	Helen		
      H003-P1	VAR_c4dab5fd	Parent		
      H003-P1	VAR_ce2bd99c	Hells		
      H003-P1	VAR_e301dd60	Female		
      H003-P2	VAR_709a2322	Eve		
      H003-P2	VAR_c4dab5fd	Child		
      H003-P2	VAR_e301dd60	Female		
      H003-P3	VAR_709a2322	Michael		
      H003-P3	VAR_c4dab5fd	Child		
      H003-P3	VAR_ce2bd99c	Micky		
      H003-P3	VAR_e301dd60	Male		
      
      ##  collection_sde41bbea90_observtn.cache 
      COL_60b0b28d	integer-based anatomical measures	2			32	175		integer	continuous		0	0	0		measurement	measurements
      
      ##  collectionattribute_sde41bbea90_observtn.cache 
      COL_60b0b28d	VAR_0789e56a
      COL_60b0b28d	VAR_7d2a26d7
      
      ##  entitytypegraph.cache 
      ENT_6a7855ea	STUDY_de41bbea90		househld		household	households	0	0	3
      ENT_7a211045	STUDY_de41bbea90	ENT_6a7855ea	partcpnt		participant	participants	0	1	8
      ENT_7c02c7e8	STUDY_de41bbea90	ENT_7a211045	observtn		observation	observations	1	1	11
      
      ##  study.cache 
      @USER_DATASET_ID@	STUDY_de41bbea90	sde41bbea90	@MODIFICATION_DATE@

