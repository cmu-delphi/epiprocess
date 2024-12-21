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
    `units<-`("days") %>%
    as.numeric() %>%
    `-`(90) %>%
    abs() %>%
    `<=`(5))
  expect_error(time_delta_to_approx_difftime(3, "integer"))
})
