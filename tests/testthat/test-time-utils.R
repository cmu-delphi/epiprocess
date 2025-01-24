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
  expect_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "day"),
    class = "epiprocess__validate_slide_window_arg"
  )
  expect_error(validate_slide_window_arg(as.difftime(1, units = "secs"), "day"),
    class = "epiprocess__validate_slide_window_arg"
  )

  expect_no_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "week"))
  expect_error(validate_slide_window_arg(1, "week"),
    class = "epiprocess__validate_slide_window_arg"
  )

  expect_no_error(validate_slide_window_arg(1, "integer"))
  expect_error(validate_slide_window_arg(as.difftime(1, units = "days"), "integer"),
    class = "epiprocess__validate_slide_window_arg"
  )
  expect_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "integer"),
    class = "epiprocess__validate_slide_window_arg"
  )

  expect_no_error(validate_slide_window_arg(1, "yearmonth"))
  expect_error(validate_slide_window_arg(as.difftime(1, units = "weeks"), "yearmonth"),
    class = "epiprocess__validate_slide_window_arg"
  )
})

test_that("unit_time_delta works", {
  for (format in c("friendly", "fast")) {
    expect_equal(
      as.Date("2020-01-01") + 5 * unit_time_delta("day", format = format),
      as.Date("2020-01-06")
    )
    expect_equal(
      as.Date("2020-01-01") + 2 * unit_time_delta("week", format = format),
      as.Date("2020-01-15")
    )
    expect_equal(
      tsibble::make_yearmonth(2000, 1) + 5 * unit_time_delta("yearmonth", format = format),
      tsibble::make_yearmonth(2000, 6)
    )
    expect_equal(
      1L + 5L * unit_time_delta("integer", format = format),
      6L
    )
    #
    expect_equal(
      as.Date("2020-01-01") +
        time_delta_to_n_steps(as.Date("2020-01-06") - as.Date("2020-01-01"), "day") *
          unit_time_delta("day", format = format),
      as.Date("2020-01-06")
    )
    expect_equal(
      as.Date("2020-01-01") +
        time_delta_to_n_steps(as.integer(as.Date("2020-01-06") - as.Date("2020-01-01")), "day") *
          unit_time_delta("day", format = format),
      as.Date("2020-01-06")
    )
    expect_equal(
      as.Date("2020-01-01") +
        time_delta_to_n_steps(as.Date("2020-01-15") - as.Date("2020-01-01"), "week") *
          unit_time_delta("week", format = format),
      as.Date("2020-01-15")
    )
    expect_equal(
      as.Date("2020-01-01") +
        time_delta_to_n_steps(as.difftime(2, units = "weeks"), "week") *
          unit_time_delta("week", format = format),
      as.Date("2020-01-15")
    )
    expect_equal(
      tsibble::make_yearmonth(2000, 1) +
        time_delta_to_n_steps(5, "yearmonth") *
          unit_time_delta("yearmonth", format = format),
      tsibble::make_yearmonth(2000, 6)
    )
    expect_equal(
      1L +
        time_delta_to_n_steps(5, "integer") *
          unit_time_delta("integer", format = format),
      6L
    )
  }
})

test_that("time_delta_to_approx_difftime works as expected", {
  expect_equal(
    time_delta_to_approx_difftime(as.difftime(3, units = "days"), "day"),
    as.difftime(3, units = "days")
  )
  expect_equal(
    time_delta_to_approx_difftime(3, "day"),
    as.difftime(3, units = "days")
  )
  expect_equal(
    time_delta_to_approx_difftime(3, "week"),
    as.difftime(3, units = "weeks")
  )
  expect_true(time_delta_to_approx_difftime(3, "yearmonth") %>%
    `units<-`("days") %>% # nolint: indentation_linter
    as.numeric() %>%
    `-`(90) %>%
    abs() %>%
    `<=`(5))
  expect_error(time_delta_to_approx_difftime(3, "integer"))
})

test_that("format_time_delta works as expected", {
  # time_type "day":
  expect_equal(
    format_time_delta(as.difftime(1, units = "days"), "day"),
    "1 day"
  )
  expect_equal(
    format_time_delta(as.difftime(2, units = "days"), "day"),
    "2 days"
  )
  expect_equal(
    format_time_delta(1, "day"),
    "1 day"
  )
  expect_equal(
    format_time_delta(2, "day"),
    "2 days"
  )
  # time_type "week":
  expect_equal(
    format_time_delta(as.difftime(1, units = "weeks"), "week"),
    "1 week"
  )
  expect_equal(
    format_time_delta(as.difftime(7, units = "days"), "week"),
    "1 week"
  )
  expect_equal(
    format_time_delta(1, "week"),
    "1 week"
  )
  expect_equal(
    format_time_delta(as.difftime(2, units = "weeks"), "week"),
    "2 weeks"
  )
  # time_type "yearmonth":
  expect_equal(
    format_time_delta(1, "yearmonth"),
    "1 month"
  )
  expect_equal(
    format_time_delta(2, "yearmonth"),
    "2 months"
  )
  # time_type "integer":
  expect_equal(
    format_time_delta(1, "integer"),
    "1 time step"
  )
  expect_equal(
    format_time_delta(2, "integer"),
    "2 time steps"
  )
  # we don't handle length != 1; pluralize will raise error for us:
  expect_error(format_time_delta(numeric(0), "day")) # we don't handle length != 0
  expect_error(format_time_delta(1:5, "day")) # we don't handle length != 0
})

test_that("difftime_approx_ceiling_time_delta works as expected", {
  # At time of writing, docs don't guarantee difftime_approx_ceiling_time_delta
  # will output friendly time_deltas, so we'll include a standardization step in
  # these tests. Prevent eye-glazing repetitition by testing a bunch of cases
  # simultaneously with dplyr:
  comparisons <- tibble::tribble(
    ~x_amount, ~x_units, ~time_type, ~expected_wrapped_friendly_result,
    # days x day:
    0, "days", "day", list(as.difftime(0.0, units = "days")),
    1.5, "days", "day", list(as.difftime(2.0, units = "days")),
    2.0, "days", "day", list(as.difftime(2.0, units = "days")),
    # days x week:
    2.0, "days", "week", list(as.difftime(1.0, units = "weeks")),
    7.0, "days", "week", list(as.difftime(1.0, units = "weeks")),
    8.0, "days", "week", list(as.difftime(2.0, units = "weeks")),
    # weeks x week:
    1.0, "weeks", "week", list(as.difftime(1.0, units = "weeks")),
    1.1, "weeks", "week", list(as.difftime(2.0, units = "weeks")),
    # days x yearmonth:
    2.0, "days", "yearmonth", list(1.0),
    32.0, "days", "yearmonth", list(2.0),
  ) %>%
    mutate(across(expected_wrapped_friendly_result, purrr::list_flatten)) %>%
    rowwise() %>%
    mutate(
      wrapped_friendly_result = as.difftime(x_amount, units = x_units) %>%
        difftime_approx_ceiling_time_delta(time_type) %>%
        time_delta_standardize(time_type, format = "friendly") %>%
        list()
    ) %>%
    ungroup()

  expect_equal(
    comparisons$wrapped_friendly_result,
    comparisons$expected_wrapped_friendly_result
  )

  # days x integer:
  expect_error(difftime_approx_ceiling_time_delta(as.difftime(1, units = "days"), "integer"),
    regexp = "Unsupported time_type"
  )
})
