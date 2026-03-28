test_date <- as.Date("2020-01-01")

test_that("as_epi_df and as_epi_archive handle auto-detection and pivoting", {
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = rep(test_date + 1:5, 2),
    signal = rep(c("cases", "deaths"), each = 5),
    value = 1:10
  )

  # as_epi_df: auto-detects 'signal' and pivots to wide
  expect_snapshot(df <- as_epi_df(raw))
  expect_s3_class(df, "epi_df")
  expect_setequal(names(df), c("geo_value", "time_value", "cases", "deaths"))
  expect_equal(nrow(df), 5)

  # as_epi_archive: auto-detects and pivots
  raw_arch <- dplyr::mutate(raw, version = test_date + 6)
  expect_snapshot(arch <- as_epi_archive(raw_arch))
  expect_setequal(names(arch$DT), c("geo_value", "time_value", "cases", "deaths", "version"))
  expect_equal(nrow(arch$DT), 5)

  # Drops extra columns during pivot
  raw_extra <- dplyr::mutate(raw, direction = 1)
  expect_snapshot(edf <- as_epi_df(raw_extra))
  expect_setequal(names(edf), c("geo_value", "time_value", "cases", "deaths"))

  # Handles observation carried forward in archive pivot
  tib <- tibble::tibble(
    geo_value = 1, time_value = 1, version = c(1, 1, 2),
    signal = c("a", "b", "a"), value = c(1, 11, 2)
  )
  expect_snapshot(arch_locf <- as_epi_archive(tib))
  expect_equal(arch_locf$DT$a, c(1, 2))
  expect_equal(arch_locf$DT$b, c(11, 11))
})

test_that("Explicit signal formats (long/wide) and guessing", {
  raw <- dplyr::tibble(
    geo_value = "ak",
    time_value = rep(test_date + 1:5, 2),
    custom_signal = rep(c("a", "b"), each = 5),
    value = 1:10
  )

  # Explicit long format
  expect_snapshot(df_long <- as_epi_df(raw, signal_format = "long", signal_var = "custom_signal"))
  expect_true("custom_signal" %in% attr(df_long, "metadata")$other_keys)
  expect_true("value" %in% names(df_long))

  # Guessing long if NULL
  raw_guess <- dplyr::rename(raw, signal = custom_signal)
  expect_snapshot(df_long_guess <- as_epi_df(raw_guess, signal_format = "long"))
  expect_true("signal" %in% attr(df_long_guess, "metadata")$other_keys)

  # Explicit wide format
  expect_snapshot(df_wide <- as_epi_df(raw, signal_format = "wide", signal_var = "custom_signal"))
  expect_named(df_wide, c("geo_value", "time_value", "a", "b"))

  # Guessing wide if NULL
  expect_snapshot(df_wide_guess <- as_epi_df(raw_guess, signal_format = "wide"))
  expect_named(df_wide_guess, c("geo_value", "time_value", "a", "b"))

  # Explicit signal_var activates pivot in auto mode
  expect_message(df_auto <- as_epi_df(raw, signal_var = "custom_signal"))
  expect_named(df_auto, c("geo_value", "time_value", "a", "b"))
})

test_that("Multi-candidate behavior and silence", {
  raw <- dplyr::tibble(
    geo_value = "ca", time_value = test_date,
    signal = 1, value = 2
  )

  # Multiple candidates: silent if not pivoting OR if ambiguous in auto mode
  raw_multi <- dplyr::mutate(raw, signal2 = 1)
  expect_silent(as_epi_df(raw_multi))

  raw_pivot <- dplyr::tibble(
    geo_value = "ca", time_value = test_date + 1:5,
    signal = c(1, 1, 2, 2, 2), value = 1:5,
    variable = 1
  )
  expect_silent(as_epi_df(raw_pivot))
  # Explicit selection resolves ambiguity and pivots
  expect_message(as_epi_df(raw_pivot, signal_var = "signal"), "Pivoting")

  # Silent and no-pivot when multiple candidates found in auto mode (if not pivoting)
  raw_no_pivot <- dplyr::mutate(raw, signal_name = "s")
  expect_silent(df <- as_epi_df(raw_no_pivot))
  expect_true(all(c("signal", "signal_name") %in% names(df)))

  # Archive silence
  raw_arch <- dplyr::mutate(raw_no_pivot, version = test_date + 1)
  expect_silent(arch <- as_epi_archive(raw_arch))
  expect_true(all(c("signal", "signal_name") %in% names(arch$DT)))
})

test_that("Error handling and non-scalar validations", {
  raw_error <- dplyr::tibble(
    geo_value = "ca", time_value = test_date,
    custom = "cases", value = 1
  )

  # Cannot guess
  expect_error(
    as_epi_df(raw_error, signal_format = "long"),
    class = "epiprocess__unspecified_signal_var"
  )

  # Multiple candidates in non-auto mode
  raw_multi <- dplyr::mutate(raw_error, signal = "s", name = "n")
  expect_error(
    as_epi_df(raw_multi, signal_format = "long"),
    class = "epiprocess__multiple_signal_candidates"
  )
  expect_error(
    as_epi_archive(dplyr::mutate(raw_multi, version = test_date), signal_format = "wide"),
    class = "epiprocess__multiple_signal_candidates"
  )

  # Non-scalar signal_var
  raw_ns <- dplyr::mutate(raw_error, s1 = 1, s2 = 2)
  expect_error(
    as_epi_df(raw_ns, signal_var = c("s1", "s2")),
    class = "epiprocess__signal_var_not_scalar"
  )
  expect_error(
    as_epi_archive(dplyr::mutate(raw_ns, version = test_date), signal_var = c("s1", "s2")),
    class = "epiprocess__signal_var_not_scalar"
  )

  # signal_var cannot be a primary key
  expect_error(
    as_epi_df(raw_error, signal_var = "geo_value"),
    class = "epiprocess__signal_var_is_key"
  )
  expect_error(
    as_epi_archive(dplyr::mutate(raw_error, version = test_date), signal_var = "version"),
    class = "epiprocess__signal_var_is_key"
  )

  # signal_var in other_keys prevents it from being guessed (stays long)
  x_ok <- tibble::tibble(
    geo_value = rep("pa", 2), time_value = test_date,
    signal = c("s1", "s2"), value = c(1, 2)
  )
  # auto-pivot skipped because "signal" is in other_keys
  res_ok <- as_epi_df(x_ok, other_keys = "signal")
  expect_false("s1" %in% names(res_ok))
  expect_true("signal" %in% attr(res_ok, "metadata")$other_keys)

  # explicit conflict still aborts
  expect_error(
    as_epi_df(x_ok, signal_var = "signal", other_keys = "signal"),
    class = "epiprocess__signal_var_is_key"
  )

  # wide pivot requires value column
  expect_error(
    as_epi_df(dplyr::select(raw_error, -value), signal_var = "custom", signal_format = "wide"),
    class = "epiprocess__wide_pivot_requires_value_col"
  )
})
