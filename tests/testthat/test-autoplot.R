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

set.seed(42)
df_many_keys <- expand.grid(
  geo_value = letters[1:20],
  time_value = as.Date("2023-01-01") + 0:2
) %>%
  mutate(cases = rnorm(dplyr::n())) %>%
  as_epi_df(as_of = as.Date("2023-01-04"))

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

test_that("autoplot_subsample_keys warning/hints", {
  # Default .max_keys = 10 logic
  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df_many_keys, "geo_value",
      .max_keys = 10, .interactive = FALSE, .facet_used = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$geo_value)), 10)

  # Hint logic respects .facet_used
  expect_warning(
    epiprocess:::autoplot_subsample_keys(
      df_many_keys, "geo_value",
      .max_keys = 10, .facet_used = TRUE, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_snapshot(
    epiprocess:::autoplot_subsample_keys(
      df_many_keys, "geo_value",
      .max_keys = 10, .facet_used = TRUE, .interactive = FALSE
    )
  )

  expect_warning(
    epiprocess:::autoplot_subsample_keys(
      df_many_keys, "geo_value",
      .max_keys = 10, .facet_used = FALSE, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_snapshot(
    epiprocess:::autoplot_subsample_keys(
      df_many_keys, "geo_value",
      .max_keys = 10, .facet_used = FALSE, .interactive = FALSE
    )
  )
})

test_that("autoplot interactive dropdown logic (epi_df and epi_archive)", {
  skip_if_not_installed("plotly")

  df <- expand.grid(
    geo_value = c("ak", "al"),
    time_value = as.Date("2023-01-01") + 0:10
  ) %>%
    mutate(cases = 1:22) %>%
    as_epi_df()

  # Simple epi_df dropdowns
  p1 <- autoplot(df, cases, .interactive = TRUE, .facet_by = "geo_value", .facet_to_dropdown = TRUE)
  expect_snapshot(plotly::plotly_build(p1)$x$layout$updatemenus)

  # Complex multi-key epi_df dropdowns
  df_other <- expand.grid(
    geo_value = c("ak", "al"),
    time_value = as.Date("2023-01-01") + 0:10,
    other = c("x", "y")
  ) %>%
    mutate(cases = rnorm(dplyr::n())) %>%
    as_epi_df(other_keys = "other", as_of = as.Date("2023-01-04"))

  p2 <- autoplot(
    df_other, cases,
    .facet_by = "other_keys", .facet_to_dropdown = TRUE,
    .color_by = "none", .interactive = TRUE
  )
  pb2 <- plotly::plotly_build(p2)
  expect_snapshot(pb2$x$layout$updatemenus)
  expect_snapshot(purrr::map(pb2$x$data, ~ .x$visible))
  expect_snapshot(pb2$x$layout$yaxis)

  # epi_archive dropdowns
  df_arc <- expand.grid(
    geo_value = c("ak", "al"),
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(cases = 1:6, version = time_value) %>%
    as_epi_archive()

  p3 <- autoplot(df_arc, cases, .versions = "day", .interactive = TRUE, .facet_to_dropdown = TRUE)
  pb3 <- plotly::plotly_build(p3)
  expect_snapshot(pb3$x$layout$updatemenus)
  expect_snapshot(pb3$x$layout$title$text)
})

test_that("interactive plot sampling warning", {
  set.seed(42)
  # Create a df with 20 keys
  df <- expand.grid(
    geo_value = letters[1:20],
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(cases = rnorm(dplyr::n())) %>%
    as_epi_df()

  # Non-faceted interactive plot should perform sampling and warn
  expect_message(
    p <- autoplot(df, cases, .interactive = TRUE, .max_keys = 5),
    class = "epiprocess__autoplot_interactive_subsetting"
  )
  expect_snapshot(purrr::map_chr(plotly::plotly_build(p)$x$data, ~ .x$visible %||% "TRUE"))

  # Faceted interactive plot does not perform sampling in interactive_df
  expect_no_message(
    autoplot(df, cases, .facet_by = "geo_value", .interactive = TRUE, .max_keys = 5)
  )
})
