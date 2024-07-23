test_date <- as.Date("2020-01-01")
raw_df_chr <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = "a"),
  dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = "d")
)
ungrouped_chr <- as_epi_df(raw_df_chr, as_of = test_date + 6)
grouped_chr <- ungrouped_chr %>%
  group_by(geo_value)

raw_df_num <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15),
  dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = 1:5)
)
ungrouped_num <- as_epi_df(raw_df_num, as_of = test_date + 6)
grouped_num <- ungrouped_num %>%
  group_by(geo_value)

test_that("autoplot fails if no non-key columns are numeric", {
  expect_error(autoplot(ungrouped_chr),
    class = "epiprocess__no_numeric_vars_available"
  )

  # Multiple non-numeric columns
  testdf <- mutate(ungrouped_chr, value2 = "d")
  expect_error(autoplot(testdf),
    class = "epiprocess__no_numeric_vars_available"
  )

  expect_error(autoplot(grouped_chr),
    class = "epiprocess__no_numeric_vars_available"
  )

  # A numeric column is available, but is a key not a value.
  testdf <- mutate(raw_df_chr, key1 = c(1:5, 5:9)) %>%
    as_tsibble(index = time_value, key = c(geo_value, key1)) %>%
    as_epi_df(as_of = test_date + 6)
  expect_error(autoplot(testdf),
    class = "epiprocess__no_numeric_vars_available"
  )
})

test_that("autoplot warns when a variable is not specified, and lists the auto-selected column", {
  expect_warning(autoplot(ungrouped_num),
    regexp = ".*selecting `value`[.]",
    class = "epiprocess__unspecified_plot_var"
  )

  expect_warning(autoplot(grouped_num),
    regexp = ".*selecting `value`[.]",
    class = "epiprocess__unspecified_plot_var"
  )
})

test_that("autoplot errors when all specified columns are not numeric, and lists column names", {
  expect_error(autoplot(ungrouped_chr, value),
    regexp = ".*value.*",
    class = "epiprocess__all_requested_vars_not_numeric"
  )

  testdf <- mutate(ungrouped_chr, value2 = "d")
  expect_error(autoplot(testdf, value, value2),
    regexp = ".*variables `value` and `value2` are.*",
    class = "epiprocess__all_requested_vars_not_numeric"
  )

  expect_error(autoplot(grouped_chr, value),
    regexp = ".*variables `value` are.*",
    class = "epiprocess__all_requested_vars_not_numeric"
  )
})

test_that("autoplot warns when some specified columns are not numeric, and lists column names", {
  testdf <- mutate(ungrouped_num, value2 = "d")
  expect_warning(autoplot(testdf, value, value2),
    regexp = ".*`value` are numeric.*cannot display `value2`.*",
    class = "epiprocess__some_requested_vars_not_numeric"
  )

  testdf <- mutate(grouped_num, value2 = "d")
  expect_warning(autoplot(testdf, value, value2),
    regexp = ".*`value` are numeric.*cannot display `value2`.*",
    class = "epiprocess__some_requested_vars_not_numeric"
  )
})
