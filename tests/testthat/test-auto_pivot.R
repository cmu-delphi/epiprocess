test_that("as_epi_df auto-detects long format and pivots", {
  test_date <- as.Date("2020-01-01")
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = rep(test_date + 1:5, 2),
    signal = rep(c("cases", "deaths"), each = 5),
    value = 1:10
  )

  # Auto detection
  expect_snapshot(
    df <- as_epi_df(raw)
  )
  expect_s3_class(df, "epi_df")
  expect_true(all(c("cases", "deaths") %in% names(df)))
  expect_false("signal" %in% names(df))
  expect_false("value" %in% names(df))
  expect_equal(nrow(df), 5)
})

test_that("as_epi_df supports explicit long format via input_format and signal_var", {
  test_date <- as.Date("2020-01-01")
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = rep(test_date + 1:5, 2),
    custom_signal = rep(c("a", "b"), each = 5),
    value = 1:10
  )

  # Explicit long format
  expect_snapshot(
    df <- as_epi_df(raw, input_format = "long", signal_var = "custom_signal")
  )
  expect_true(all(c("a", "b") %in% names(df)))
})

test_that("as_epi_df errors if signal_var or value column is missing in long format", {
  test_date <- as.Date("2020-01-01")
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = test_date + 1:5,
    signal = "cases",
    not_value = 1:5
  )

  expect_snapshot(
    as_epi_df(raw, input_format = "long", signal_var = "signal"),
    error = TRUE
  )

  expect_snapshot(
    as_epi_df(raw, input_format = "long", signal_var = "nonexistent"),
    error = TRUE
  )
})

test_that("as_epi_archive auto-detects long format and pivots", {
  test_date <- as.Date("2020-01-01")
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = rep(test_date + 1:5, 2),
    version = test_date + 6,
    signal = rep(c("cases", "deaths"), each = 5),
    value = 1:10
  )

  expect_snapshot(
    arch <- as_epi_archive(raw)
  )
  expect_s3_class(arch, "epi_archive")
  expect_true(all(c("cases", "deaths") %in% names(arch$DT)))
  expect_equal(nrow(arch$DT), 5)
})

test_that("as_epi_df with input_format = 'wide' does no pivoting", {
  test_date <- as.Date("2020-01-01")
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = test_date + 1:5,
    value = 1:5,
    signal = "cases" # This would be detected as long if "auto"
  )

  df <- as_epi_df(raw, input_format = "wide")
  expect_true("signal" %in% names(df))
  expect_true("value" %in% names(df))
})

test_that("as_epi_df drops extra metadata columns during auto-pivot", {
  # Data with a 'direction' column that should be dropped
  df <- tibble::tibble(
    geo_value = rep(c("ca", "ny"), each = 2),
    time_value = rep(as.Date("2020-01-01"), 4),
    name = rep(c("cases", "deaths"), 2),
    value = c(10, 1, 20, 2),
    direction = c(1, 0, -1, 0)
  )

  # Should pivot and drop 'direction'
  expect_snapshot(edf <- as_epi_df(df))
  expect_named(edf, c("geo_value", "time_value", "cases", "deaths"))
  expect_false("direction" %in% names(edf))
})

test_that("as_epi_archive handles long-format with LOCF during pivot", {
  # Case provided by user
  tib <- tibble::tibble(
    geo_value = 1,
    time_value = 1,
    version = c(1, 1, 2),
    signal = c("a", "b", "a"),
    value = c(1, 11, 2)
  )

  expect_snapshot(
    arch <- as_epi_archive(tib)
  )

  # Check that version 2 has both 'a' and 'b'
  # 'a' should be updated to 2, 'b' should be carried forward as 11
  expect_equal(nrow(arch$DT), 2)
  expect_equal(arch$DT$a, c(1, 2))
  expect_equal(arch$DT$b, c(11, 11))
})
