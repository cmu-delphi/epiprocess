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
  # Add .colours to df_many_keys (20 levels)
  df_colours <- df_many_keys %>%
    mutate(.colours = factor(geo_value))

  # Default .max_keys = 10 logic
  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df_colours,
      .max_keys = 10, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$.colours)), 10)

  # Fallback to .rows if .colours is missing
  df_rows <- df_many_keys %>%
    mutate(.rows = factor(geo_value))

  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df_rows,
      .max_keys = 10, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$.rows)), 10)

  # Add .facets to df_many_keys (20 levels)
  df_facets <- df_many_keys %>%
    mutate(.facets = factor(geo_value))

  # Samples facets
  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df_facets,
      .max_keys = 10, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$.facets)), 10)

  # Both .facets and .colours = 20 total
  df_both <- expand.grid(
    geo_value = "ak",
    f = 1:5,
    c = 1:4,
    time_value = as.Date("2023-01-01") + 0:2
  ) %>%
    mutate(
      .facets = factor(f),
      .colours = factor(c),
      cases = 1
    ) %>%
    as_epi_df(other_keys = c("f", "c"))

  # Total shown = 2 * 4 = 8.
  expect_warning(
    sampled <- epiprocess:::autoplot_subsample_keys(
      df_both,
      .max_keys = 10, .interactive = FALSE
    ),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_equal(length(unique(sampled$.facets)), 2)
  expect_equal(length(unique(sampled$.colours)), 4)

  # Snapshots for warning message structure
  expect_snapshot(
    epiprocess:::autoplot_subsample_keys(df_facets, .max_keys = 10, .interactive = FALSE)
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

  # Verify interactive plotly dropdowns in the .json snapshots. We look for:
  # - `active`: index of the default active button.
  # - `buttons`: dropdown options with their `label`, `method`, and `args`.
  # - `args`: `visible` trace flags, `showlegend` flags, and the updated plot `title`.

  expect_equal_custom_rds_snapshot(plotly::plotly_build(p1)$x$layout$updatemenus, "p1_updatemenus")

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
  # Check dropdown logic for complex multi-key epi_df
  expect_equal_custom_rds_snapshot(pb2$x$layout$updatemenus, "p2_updatemenus")
  expect_snapshot(purrr::map_lgl(pb2$x$data, ~ .x$visible))
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
  # Dropdown logic for epi_archive
  expect_equal_custom_rds_snapshot(pb3$x$layout$updatemenus, "p3_updatemenus")
  expect_snapshot(pb3$x$layout$title$text)

  # Test single-trace legend suppression
  df_single <- dplyr::tibble(
    geo_value = rep(c("ak", "al"), each = 5),
    time_value = rep(as.Date("2023-01-01") + 0:4, 2),
    cases = 1:10
  ) %>% as_epi_df()
  p4 <- autoplot(df_single, cases, .interactive = TRUE, .facet_by = "geo_value", .facet_to_dropdown = TRUE)
  pb4 <- plotly::plotly_build(p4)
  # All traces should have showlegend = FALSE because each panel has only 1 trace
  expect_true(all(purrr::map_lgl(pb4$x$data, ~ !.x$showlegend)))
  # Check button args
  expect_true(all(purrr::map_lgl(pb4$x$layout$updatemenus[[1]]$buttons, ~ all(!unlist(.x$args[[1]]$showlegend)))))
})

test_that("autoplot distinguishes indicators when faceting by geo_value", {
  df <- dplyr::tibble(
    geo_value = "ak",
    time_value = as.Date("2023-01-01") + 0:4,
    v1 = 1:5,
    v2 = 6:10
  ) %>% as_epi_df()

  # When faceting by geo_value with 2 responses, colors include .response_name
  # and exclude the geo_value to avoid redundant legends.
  p <- autoplot(df, v1, v2, .facet_by = "geo_value")
  expect_true(".colours" %in% names(p$data))
  expect_setequal(levels(p$data$.colours), c("v1", "v2"))
  # Facets should stay as just geo_value
  expect_setequal(levels(p$data$.facets), "ak")
})

test_that("autoplot drops color when redundant (one line per facet)", {
  df <- dplyr::tibble(
    geo_value = rep(c("ak", "al"), each = 5),
    time_value = rep(as.Date("2023-01-01") + 0:4, 2),
    cases = 1:10
  ) %>% as_epi_df()

  # When faceting by geo_value with only 1 response, each panel has 1 line.
  # .colours should be dropped since it's redundant.
  p <- autoplot(df, cases, .facet_by = "geo_value")
  expect_false(".colours" %in% names(p$data))
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
