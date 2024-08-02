test_that("guess_geo_type tests for different types of geo_value's", {
  # California, New York
  states <- c("ca", "ny")

  # Canada, USA, United Kingdom
  nations <- c("ca", "us", "uk")

  # Note: These are just five-number names that may not necessarily be existent
  # counties
  counties <- c("12345", "67890")

  # HHS regions
  hhs <- c(1:3)

  # HRR regions
  hrr <- c(100, 200)

  # Long numbers should be custom
  long_nums <- c(123456789, 111222333)

  # Health regions in British Columbia
  bc <- c(
    "Vancouver Coastal", "Interior", "Fraser",
    "Northern", "Vancouver Island"
  )

  # Long numbers as strings should also be custom
  long_num_strings <- c("123456789", "111222333")

  expect_equal(guess_geo_type(states), "state")
  expect_equal(guess_geo_type(nations), "nation")
  expect_equal(guess_geo_type(counties), "county")
  expect_equal(guess_geo_type(hhs), "hhs")
  expect_equal(guess_geo_type(hrr), "hrr")
  expect_equal(guess_geo_type(long_num_strings), "custom")
  expect_equal(guess_geo_type(bc), "custom")
  expect_equal(guess_geo_type(long_nums), "custom")
})

test_that("guess_time_type works for different types", {
  days <- as.Date("2022-01-01") + 0:6
  weeks <- as.Date("2022-01-01") + 7 * 0:6
  yearmonths <- tsibble::yearmonth(10)
  integers <- c(1999, 2000)

  # YYYY-MM-DD is the accepted format
  not_ymd1 <- "January 1, 2022"
  not_ymd2 <- "1 January 2022"
  not_ymd3 <- "1 Jan 2022"
  not_a_date <- "asdf"

  expect_equal(guess_time_type(days), "day")
  expect_equal(guess_time_type(weeks), "week")
  expect_equal(guess_time_type(yearmonths), "yearmonth")
  expect_equal(guess_time_type(integers), "integer")

  expect_warning(guess_time_type(not_ymd1), "Unsupported time type in column")
  expect_warning(guess_time_type(not_ymd2), "Unsupported time type in column")
  expect_warning(guess_time_type(not_ymd3), "Unsupported time type in column")
  expect_warning(guess_time_type(not_a_date), "Unsupported time type in column")
})

test_that("guess_time_type works with gaps", {
  gaps <- c(1:6, 8, 9, 11, 8 + 7)
  expect_equal(guess_time_type(as.Date("2022-01-01") + gaps), "day")
  expect_equal(guess_time_type(as.Date("2022-01-01") + 7 * gaps), "week")
})

test_that("enlist works", {
  my_list <- enlist(x = 1, y = 2, z = 3)
  expect_equal(my_list$x, 1)
  expect_true(inherits(my_list, "list"))
})

test_that("assert_sufficient_f_args alerts if the provided f doesn't take enough args", {
  f_xgt <- function(x, g, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xgt_dots <- function(x, g, t, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))

  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_no_error(assert_sufficient_f_args(f_xgt, .ref_time_value_label = "reference time value"))
  expect_no_warning(assert_sufficient_f_args(f_xgt, .ref_time_value_label = "reference time value"))
  expect_no_error(assert_sufficient_f_args(f_xgt_dots, .ref_time_value_label = "reference time value"))
  expect_no_warning(assert_sufficient_f_args(f_xgt_dots, .ref_time_value_label = "reference time value"))

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_dots <- function(...) dplyr::tibble(value = c(5), count = c(2))
  f_x <- function(x) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f <- function() dplyr::tibble(value = c(5), count = c(2))

  expect_warning(assert_sufficient_f_args(f_x_dots, .ref_time_value_label = "reference time value"),
    regexp = ", the group key and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_warning(assert_sufficient_f_args(f_dots, .ref_time_value_label = "reference time value"),
    regexp = ", the window data, group key, and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_error(assert_sufficient_f_args(f_x, .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args"
  )
  expect_error(assert_sufficient_f_args(f, .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args"
  )

  # Make sure we generate the same sort of conditions on some external functions
  # that have caused surprises in the past:
  expect_warning(assert_sufficient_f_args(mean, .ref_time_value_label = "reference time value"),
    regexp = ", the group key and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_warning(assert_sufficient_f_args(sum, .ref_time_value_label = "reference time value"),
    regexp = ", the window data, group key, and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_warning(assert_sufficient_f_args(dplyr::slice, .ref_time_value_label = "reference time value"),
    regexp = ", the group key and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )

  f_xs_dots <- function(x, setting = "a", ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xs <- function(x, setting = "a") dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  expect_warning(assert_sufficient_f_args(f_xs_dots, setting = "b", .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_error(assert_sufficient_f_args(f_xs, setting = "b", .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args_plus_forwarded"
  )

  expect_error(assert_sufficient_f_args(f_xgt, "b", .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args_plus_forwarded"
  )
})

test_that("assert_sufficient_f_args alerts if the provided f has defaults for the required args", {
  f_xgt <- function(x, g = 1, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xgt_dots <- function(x = 1, g, t, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_x_dots <- function(x = 1, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))

  expect_error(assert_sufficient_f_args(f_xgt, .ref_time_value_label = "reference time value"),
    regexp = "pass the group key to `f`'s g argument,",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(assert_sufficient_f_args(f_xgt_dots, .ref_time_value_label = "reference time value"),
    regexp = "pass the window data to `f`'s x argument,",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(suppressWarnings(assert_sufficient_f_args(f_x_dots, .ref_time_value_label = "reference time value")),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  f_xsgt <- function(x, setting = "a", g, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xsgt_dots <- function(x, setting = "a", g, t, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xs_dots <- function(x = 1, setting = "a", ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))

  # forwarding named dots should prevent some complaints:
  expect_no_error(assert_sufficient_f_args(f_xsgt, setting = "b", .ref_time_value_label = "reference time value"))
  expect_no_error(assert_sufficient_f_args(f_xsgt_dots, setting = "b", .ref_time_value_label = "reference time value"))
  expect_error(suppressWarnings(assert_sufficient_f_args(f_xs_dots, setting = "b", .ref_time_value_label = "reference time value")),
    regexp = "pass the window data to `f`'s x argument",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  # forwarding unnamed dots should not:
  expect_error(assert_sufficient_f_args(f_xsgt, "b", .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(assert_sufficient_f_args(f_xsgt_dots, "b", .ref_time_value_label = "reference time value"),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(
    expect_warning(
      assert_sufficient_f_args(f_xs_dots, "b", .ref_time_value_label = "reference time value"),
      class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
    ),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  # forwarding no dots should produce a different error message in some cases:
  expect_error(
    expect_warning(
      assert_sufficient_f_args(f_xs_dots, .ref_time_value_label = "reference time value"),
      class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
    ),
    regexp = "window data and group key to `f`'s x and setting argument",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
})

test_that("computation formula-derived functions take all argument types", {
  # positional
  expect_identical(as_time_slide_computation(~ ..2 + ..3)(1, 2, 3), 5)
  expect_identical(as_time_slide_computation(~..1)(1, 2, 3), 1)
  # Matching rlang, purr, dplyr usage
  expect_identical(as_time_slide_computation(~ .x + .z)(1, 2, 3), 4)
  expect_identical(as_time_slide_computation(~ .x + .y)(1, 2, 3), 3)
  # named
  expect_identical(as_time_slide_computation(~ . + .ref_time_value)(1, 2, 3), 4)
  expect_identical(as_time_slide_computation(~.group_key)(1, 2, 3), 2)
})

test_that("as_slide_computation passes functions unaltered", {
  f <- function(a, b, c) {
    a * b * c + 5
  }
  expect_identical(as_time_slide_computation(f), f)
})

test_that("as_slide_computation raises errors as expected", {
  # Formulas must be one-sided
  expect_error(as_time_slide_computation(y ~ ..1),
    class = "epiprocess__as_slide_computation__formula_is_twosided"
  )

  # Formulas can't be paired with ...
  expect_error(as_time_slide_computation(~..1, method = "fn"),
    class = "epiprocess__as_slide_computation__formula_with_dots"
  )

  # `f_env` must be an environment
  formula_without_env <- stats::as.formula(~..1)
  rlang::f_env(formula_without_env) <- 5
  expect_error(as_time_slide_computation(formula_without_env),
    class = "epiprocess__as_slide_computation__formula_has_no_env"
  )

  # `f` must be a function, formula, or string
  expect_error(as_time_slide_computation(5),
    class = "epiprocess__as_slide_computation__cant_convert_catchall"
  )
})

test_that("guess_period works", {
  # Error cases:
  expect_error(guess_period(numeric(0L)), class = "epiprocess__guess_period__not_enough_times")
  expect_error(guess_period(c(1)), class = "epiprocess__guess_period__not_enough_times")
  # Different numeric classes and cases:
  expect_identical(guess_period(c(1, 8)), 7)
  expect_identical(guess_period(c(1, 8, 15)), 7)
  expect_identical(guess_period(c(1L, 8L, 15L)), 7L)
  expect_identical(guess_period(c(0, 7, 14, 15)), 1)
  # We currently allow the guessed frequency to not appear in the diffs, but
  # this might not be a good idea as it likely indicates an issue with the data
  # (#485).
  expect_identical(guess_period(c(0, 2, 5)), 1)
  expect_identical(guess_period(c(0, 4, 10)), 2)
  # On Dates:
  daily_dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-15"), by = "day")
  weekly_dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-15"), by = "week")
  expect_identical(
    daily_dates[[1L]] + guess_period(daily_dates) * (seq_along(daily_dates) - 1L),
    daily_dates
  )
  expect_identical(
    weekly_dates[[1L]] + guess_period(weekly_dates) * (seq_along(weekly_dates) - 1L),
    weekly_dates
  )
  # On POSIXcts:
  daily_posixcts <- as.POSIXct(daily_dates, tz = "US/Aleutian") + 3600
  weekly_posixcts <- as.POSIXct(weekly_dates, tz = "US/Aleutian") + 3600
  expect_identical(
    daily_posixcts[[1L]] + guess_period(daily_posixcts) * (seq_along(daily_posixcts) - 1L),
    daily_posixcts
  )
  expect_identical(
    weekly_posixcts[[1L]] + guess_period(weekly_posixcts) * (seq_along(weekly_posixcts) - 1L),
    weekly_posixcts
  )
  # On POSIXlts:
  daily_posixlts <- as.POSIXlt(daily_dates, tz = "UTC") + 3600
  weekly_posixlts <- as.POSIXlt(weekly_dates, tz = "UTC") + 3600
  expect_identical(
    daily_posixlts[[1L]] + guess_period(daily_posixlts) * (seq_along(daily_posixlts) - 1L),
    daily_posixlts
  )
  expect_identical(
    weekly_posixlts[[1L]] + guess_period(weekly_posixlts) * (seq_along(weekly_posixlts) - 1L),
    weekly_posixlts
  )
})


test_that("validate_slide_window_arg works", {
  for (time_type in c("day", "week", "integer", "yearmonth")) {
    expect_no_error(validate_slide_window_arg(Inf, time_type))
  }
  expect_no_error(validate_slide_window_arg(as.difftime(1, units = "days"), "day"))
  expect_no_error(validate_slide_window_arg(1, "day"))
  expect_no_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "day"))

  expect_no_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "week"))
  expect_error(validate_slide_window_arg(1, "week"))

  expect_no_error(validate_slide_window_arg(1, "integer"))
  expect_error(validate_slide_window_arg(as.difftime(1, units = "days"), "integer"))
  expect_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "integer"))

  expect_no_error(validate_slide_window_arg(1, "yearmonth"))
  expect_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "yearmonth"))
})
