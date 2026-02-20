date0 <- as.Date("2020-01-01") - 1L

test_that("extract2_horizon works on daily-daily archive", {
  daily_daily_archive <- as_epi_archive(tibble(
    geo_value = 1,
    time_value = date0 + c(1, 1, 2, 2, 2),
    version = date0 + c(2, 4, 3:5),
    value = as.numeric(1:5)
  ))
  daily_ekvs <- tibble(
    geo_value = 1,
    version = date0 + 2:6
  )
  expect_equal(
    extract2_horizon(daily_daily_archive, daily_ekvs, "value", -1),
    c(1, 3, NA, NA, NA)
  )
  expect_equal(
    extract2_horizon(daily_daily_archive, daily_ekvs, "value", as.difftime(-2, units = "days")),
    c(NA, 1, 4, NA, NA)
  )
  expect_equal(
    extract2_horizon(daily_daily_archive, daily_ekvs, "value", -3),
    c(NA, NA, 2, 5, NA)
  )
  expect_equal(
    extract2_horizon(daily_daily_archive, daily_ekvs, "value", -4),
    c(NA, NA, NA, 2, NA) # last NA because beyond version end for time 2
  )
})

test_that("extract2_horizon works on weekly-weekly-with-same-wday archive", {
  nice_weekly_weekly_archive <- as_epi_archive(tibble(
    geo_value = 1,
    time_value = date0 + 7 * c(1, 1, 2, 2, 2),
    version = date0 + 7 * c(2, 4, 3:5),
    value = as.numeric(1:5)
  ))
  weekly_ekvs <- tibble(
    geo_value = 1,
    version = date0 + 7 * 2:6
  )
  expect_equal(
    extract2_horizon(nice_weekly_weekly_archive, weekly_ekvs, "value", as.difftime(-1, units = "weeks")),
    c(1, 3, NA, NA, NA)
  )
  expect_equal(
    extract2_horizon(nice_weekly_weekly_archive, weekly_ekvs, "value", as.difftime(-2, units = "weeks")),
    c(NA, 1, 4, NA, NA)
  )
  expect_equal(
    extract2_horizon(nice_weekly_weekly_archive, weekly_ekvs, "value", as.difftime(-3, units = "weeks")),
    c(NA, NA, 2, 5, NA)
  )
  expect_equal(
    extract2_horizon(nice_weekly_weekly_archive, weekly_ekvs, "value", as.difftime(-4, units = "weeks")),
    c(NA, NA, NA, 2, NA) # last NA because beyond version end for time 2
  )
})

test_that("extract2_horizon works on weekly-weekly-with-diff-wday archive", {

  mixed_weekly_weekly_archive <- as_epi_archive(tibble(
    geo_value = 1,
    time_value = date0 + 7 * c(1, 1, 2, 2, 2),
    version = date0 + 7 * c(2, 4, 3:5) + 3,
    value = as.numeric(1:5)
  ))
  vtype_weekly_ekvs <- tibble(
    geo_value = 1,
    version = date0 + 7 * 2:6 + 3
  )

  expect_error(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(-1, units = "weeks")),
    class = "epiprocess__extract2_horizon__horizon_weeks_misaligned"
  )

  expect_error(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(0 + 3, units = "days")),
    class = "epiprocess__extract2_horizon__horizon_days_misaligned"
  )

  expect_equal(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(-1*7 - 3, units = "days")),
    c(1, 3, NA, NA, NA)
  )
  expect_equal(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(-2*7 - 3, units = "days")),
    c(NA, 1, 4, NA, NA)
  )
  expect_equal(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(-3*7 - 3, units = "days")),
    c(NA, NA, 2, 5, NA)
  )
  expect_equal(
    extract2_horizon(mixed_weekly_weekly_archive, vtype_weekly_ekvs, "value", as.difftime(-4*7 - 3, units = "days")),
    c(NA, NA, NA, 2, NA) # last NA because beyond version end for time 2
  )

})
