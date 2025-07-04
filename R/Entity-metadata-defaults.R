# These are the columns needed in the `Entity@variables` tibble.
# It also has `variable` column which is the "primary key" column containing
# the column's name in the `Entity@data` tibble.
variable_metadata_defaults <- tibble(
  # wrangler-specific
  entity_name = NA_character_,
  entity_level = as.integer(0),
  # EDA variable metadata
  provider_label = list(list()),
  display_name = NA_character_,
  definition = NA_character_,
  stable_id = NA_character_,
  data_type = factor(NA, levels = c("id", "string", "number", "date", "longitude", "integer", "category")),
  data_shape = factor(NA, levels = c("continuous", "categorical", "ordinal", "binary")),
  # for ordinals only
  ordinal_levels = list(list()),
  # remember when dumping metadata for factor columns (data_shape != "continuous")
  # that we need to dump the levels of the factor as "vocabulary"
  display_order = NA_integer_, # for some reason this is NUMBER(3,0) on the database side, so integers -999 to 999
  display_type = factor("default", levels = c("default", "multifilter", "geoaggregator", "latitude", "longitude")),
  scale = factor(NA, levels = c("log", "log2", "ln")),
  # the following are character type so they can handle dates and numbers :-(
  # dates should be ISO-8601 strings YYYY-MM-DD only
  bin_width_override = NA_character_, # for dates this should be "week", "month" or "year"
  display_range_max = NA_character_,
  display_range_min = NA_character_,
  # hydrated, read-only # precision = NA_integer_,
  has_values = TRUE, # FALSE for display_type == 'category', TRUE otherwise 
  hidden = list(factor(NA, levels = c("download", "variableTree", "map", "everywhere"))),
  is_featured = FALSE,
  is_merge_key = FALSE,
  is_multi_valued = FALSE,
  multi_value_delimiter = NA_character_,
  is_repeated = FALSE,
  is_temporal = FALSE,
  parent_variable = NA_character_,
  has_study_dependent_vocabulary = NA,
  impute_zero = NA,
  weighting_variable_spec = NA_character_,
  variable_spec_to_impute_zeroes_for = NA_character_,
  unit = NA_character_
)

empty_variable_metadata <- variable_metadata_defaults %>% slice(0) %>% mutate(variable = NA_character_)
