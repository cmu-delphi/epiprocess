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

test_that("autoplot_subsample_keys warning", {
  set.seed(42)
  df <- expand.grid(
    geo_value = letters[1:20],
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(cases = rnorm(dplyr::n())) %>%
    as_epi_df()

  # Test with default .max_keys = 10
  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df, "geo_value",
      .max_keys = 10, interactive = FALSE, .facet_used = FALSE
    ),
    class = "epiprocess__autoplot_max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$geo_value)), 10)
})

test_that("autoplot_subsample_keys hint logic respects .facet_used", {
  set.seed(42)
  df <- expand.grid(
    geo_value = letters[1:20],
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(cases = rnorm(dplyr::n())) %>%
    as_epi_df()

  # Hint should appear if .facet_used = TRUE
  expect_warning(
    epiprocess:::autoplot_subsample_keys(
      df, "geo_value",
      .max_keys = 10, interactive = FALSE, .facet_used = TRUE
    ),
    regexp = "To plot specific keys, use `autoplot\\(..., .facet_filter = ...\\)`"
  )

  # Hint should NOT appear if .facet_used = FALSE
  w <- expect_warning(
    epiprocess:::autoplot_subsample_keys(
      df, "geo_value",
      .max_keys = 10, interactive = FALSE, .facet_used = FALSE
    )
  )
  expect_false(grepl("To plot specific keys", w$message))
})

test_that("autoplot_plotly_dropdown is dispatched correctly for faceted plots", {
  skip_if_not_installed("plotly")

  df <- expand.grid(
    geo_value = c("ak", "al"),
    time_value = as.Date("2023-01-01") + 0:10
  ) %>%
    mutate(cases = 1:22) %>%
    as_epi_df()

  # Facet by geo_value triggers dropdown in interactive mode
  p <- autoplot(df, cases, interactive = TRUE, .facet_by = "geo_value")
  pb <- plotly::plotly_build(p)

  expect_s3_class(p, "plotly")

  # Check if it has the dropdown menu
  expect_true(!is.null(pb$x$layout$updatemenus))
  expect_match(pb$x$layout$title$text, "Key: ak")
})

test_that("autoplot_plotly_dropdown works for epi_archive", {
  skip_if_not_installed("plotly")

  df <- expand.grid(
    geo_value = c("ak", "al"),
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(cases = 1:6, version = time_value) %>%
    as_epi_archive()

  p <- autoplot(df, cases, .versions = "day", interactive = TRUE)
  pb <- plotly::plotly_build(p)

  expect_s3_class(p, "plotly")
  expect_true(!is.null(pb$x$layout$updatemenus))
  expect_match(pb$x$layout$title$text, "Key: ak")
})
