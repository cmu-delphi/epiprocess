test_that("new summarizing functions work", {
  x <- c(3, 4, 5, 9, NA)
  expect_equal(min_na_rm(x), 3)
})

test_that("Other capital letter functions work", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(extend_r(x), c(1, 2, 3, 4, 5, 5))
})

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

  yearweeks <- tsibble::yearweek(10)
  yearmonths <- tsibble::yearmonth(10)
  yearquarters <- tsibble::yearquarter(10)

  years <- c(1999, 2000)

  # YYYY-MM-DD is the accepted format
  not_ymd1 <- "January 1, 2022"
  not_ymd2 <- "1 January 2022"
  not_ymd3 <- "1 Jan 2022"

  not_a_date <- "asdf"

  expect_equal(guess_time_type(days), "day")
  expect_equal(guess_time_type(weeks), "week")

  expect_equal(guess_time_type(yearweeks), "yearweek")
  expect_equal(guess_time_type(yearmonths), "yearmonth")
  expect_equal(guess_time_type(yearquarters), "yearquarter")

  expect_equal(guess_time_type(years), "year")

  expect_equal(guess_time_type(not_ymd1), "custom")
  expect_equal(guess_time_type(not_ymd2), "custom")
  expect_equal(guess_time_type(not_ymd3), "custom")
  expect_equal(guess_time_type(not_a_date), "custom")
})
3
test_that("guess_time_type works with gaps", {
  days_gaps <- as.Date("2022-01-01") + c(0, 1, 3, 4, 8, 8 + 7)
  weeks_gaps <- as.Date("2022-01-01") + 7 * c(0, 1, 3, 4, 8, 8 + 7)
  expect_equal(guess_time_type(days_gaps), "day")
  expect_equal(guess_time_type(weeks_gaps), "week")
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
  expect_error(assert_sufficient_f_args(f_xgt), regexp = NA)
  expect_warning(assert_sufficient_f_args(f_xgt), regexp = NA)
  expect_error(assert_sufficient_f_args(f_xgt_dots), regexp = NA)
  expect_warning(assert_sufficient_f_args(f_xgt_dots), regexp = NA)

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_dots <- function(...) dplyr::tibble(value = c(5), count = c(2))
  f_x <- function(x) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f <- function() dplyr::tibble(value = c(5), count = c(2))

  expect_warning(assert_sufficient_f_args(f_x_dots),
    regexp = ", the group key and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_warning(assert_sufficient_f_args(f_dots),
    regexp = ", the window data, group key, and reference time value will be included",
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_error(assert_sufficient_f_args(f_x),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args"
  )
  expect_error(assert_sufficient_f_args(f),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args"
  )

  f_xs_dots <- function(x, setting = "a", ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xs <- function(x, setting = "a") dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  expect_warning(assert_sufficient_f_args(f_xs_dots, setting = "b"),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
  expect_error(assert_sufficient_f_args(f_xs, setting = "b"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args_plus_forwarded"
  )

  expect_error(assert_sufficient_f_args(f_xgt, "b"),
    class = "epiprocess__assert_sufficient_f_args__f_needs_min_args_plus_forwarded"
  )
})

test_that("assert_sufficient_f_args alerts if the provided f has defaults for the required args", {
  f_xgt <- function(x, g = 1, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xgt_dots <- function(x = 1, g, t, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_x_dots <- function(x = 1, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))

  expect_error(assert_sufficient_f_args(f_xgt),
    regexp = "pass the group key to `f`'s g argument,",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(assert_sufficient_f_args(f_xgt_dots),
    regexp = "pass the window data to `f`'s x argument,",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(suppressWarnings(assert_sufficient_f_args(f_x_dots)),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  f_xsgt <- function(x, setting = "a", g, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xsgt_dots <- function(x, setting = "a", g, t, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  f_xs_dots <- function(x = 1, setting = "a", ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))

  # forwarding named dots should prevent some complaints:
  expect_no_error(assert_sufficient_f_args(f_xsgt, setting = "b"))
  expect_no_error(assert_sufficient_f_args(f_xsgt_dots, setting = "b"))
  expect_error(suppressWarnings(assert_sufficient_f_args(f_xs_dots, setting = "b")),
    regexp = "pass the window data to `f`'s x argument",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  # forwarding unnamed dots should not:
  expect_error(assert_sufficient_f_args(f_xsgt, "b"),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(assert_sufficient_f_args(f_xsgt_dots, "b"),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
  expect_error(
    expect_warning(
      assert_sufficient_f_args(f_xs_dots, "b"),
      class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
    ),
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )

  # forwarding no dots should produce a different error message in some cases:
  expect_error(
    expect_warning(
      assert_sufficient_f_args(f_xs_dots),
      class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
    ),
    regexp = "window data and group key to `f`'s x and setting argument",
    class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults"
  )
})

test_that("computation formula-derived functions take all argument types", {
  # positional
  expect_identical(as_slide_computation(~ ..2 + ..3)(1, 2, 3), 5)
  expect_identical(as_slide_computation(~..1)(1, 2, 3), 1)
  # Matching rlang, purr, dplyr usage
  expect_identical(as_slide_computation(~ .x + .z)(1, 2, 3), 4)
  expect_identical(as_slide_computation(~ .x + .y)(1, 2, 3), 3)
  # named
  expect_identical(as_slide_computation(~ . + .ref_time_value)(1, 2, 3), 4)
  expect_identical(as_slide_computation(~.group_key)(1, 2, 3), 2)
})

test_that("as_slide_computation passes functions unaltered", {
  f <- function(a, b, c) {
    a * b * c + 5
  }
  expect_identical(as_slide_computation(f), f)
})

test_that("as_slide_computation raises errors as expected", {
  # Formulas must be one-sided
  expect_error(as_slide_computation(y ~ ..1),
    class = "epiprocess__as_slide_computation__formula_is_twosided"
  )

  # Formulas can't be paired with ...
  expect_error(as_slide_computation(~..1, method = "fn"),
    class = "epiprocess__as_slide_computation__formula_with_dots"
  )

  # `f_env` must be an environment
  formula_without_env <- stats::as.formula(~..1)
  rlang::f_env(formula_without_env) <- 5
  expect_error(as_slide_computation(formula_without_env),
    class = "epiprocess__as_slide_computation__formula_has_no_env"
  )

  # `f` must be a function, formula, or string
  expect_error(as_slide_computation(5),
    class = "epiprocess__as_slide_computation__cant_convert_catchall"
  )
})
