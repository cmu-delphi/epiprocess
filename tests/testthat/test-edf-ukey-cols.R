suppressPackageStartupMessages({
  library(dplyr)
})
date_edf <- tibble(geo_value = 1, time_value = as.Date("2020-01-01") - 1 + 1:5, value = 1:5) %>%
  as_epi_df(as_of = as.Date("2020-01-10"))

test_that("ungrouped Date filter works", {
  expect_identical(
    date_edf %>%
      filter(
        {if (is_ukey_col_heavyprefix(time_value)) stop("time_value was still ukeyed"); TRUE},
        time_value == as.Date("2020-01-03")
      ),
    tibble(geo_value = 1, time_value = as.Date("2020-01-03"), value = 3L) %>%
      as_epi_df(as_of = as.Date("2020-01-10"))
  )
})

test_that("grouped Date filter works", {
  expect_identical(
    date_edf %>%
      group_by(geo_value) %>%
      filter(
        {if (is_ukey_col_heavyprefix(time_value)) stop("time_value was still ukeyed"); TRUE},
        time_value == as.Date("2020-01-03")
      ),
    tibble(geo_value = 1, time_value = as.Date("2020-01-03"), value = 3L) %>%
      as_epi_df(as_of = as.Date("2020-01-10")) %>%
      group_by(geo_value)
  )
})

test_that("grouped Date filter works", {
  expect_identical(
    date_edf %>%
      group_epi_df(exclude = "time_value") %>%
      filter(
        {if (is_ukey_col_heavyprefix(time_value)) stop("time_value was still ukeyed"); TRUE},
        time_value == as.Date("2020-01-03")
      ),
    tibble(geo_value = 1, time_value = as.Date("2020-01-03"), value = 3L) %>%
      as_epi_df(as_of = as.Date("2020-01-10")) %>%
      group_by(geo_value)
  )
})


# TODO filter, group_by, and/or mutate have some differences based on
# whether it thinks it needs to run mutate or not; remember which, and
# trigger both cases.

test_that("ungrouped mutate ukey<Date> replacement works", {
  expect_identical(
    date_edf %>%
      mutate(time_value = time_value + 1,
             ukey_classed = is_ukey_col_heavyprefix(time_value)),
    tibble(geo_value = 1, time_value = as.Date("2020-01-01") - 1 + 1:5 + 1, value = 1:5, ukey_classed = FALSE) %>%
      as_epi_df(as_of = as.Date("2020-01-10"))
  )
})

test_that("grouped mutate ukey<Date> replacement works", {
  expect_identical(
    date_edf %>%
      group_by(geo_value) %>%
      mutate(time_value = time_value + 1,
             ukey_classed = is_ukey_col_heavyprefix(time_value)),
    tibble(geo_value = 1, time_value = as.Date("2020-01-01") - 1 + 1:5 + 1, value = 1:5, ukey_classed = FALSE) %>%
      as_epi_df(as_of = as.Date("2020-01-10")) %>%
      group_by(geo_value)
  )
})

test_that("grouped mutate ukey<Date> replacement works", {
  expect_identical(
    date_edf %>%
      group_epi_df(exclude = "time_value") %>%
      mutate(time_value = time_value + 1,
             ukey_classed = is_ukey_col_heavyprefix(time_value)),
    tibble(geo_value = 1, time_value = as.Date("2020-01-01") - 1 + 1:5 + 1, value = 1:5, ukey_classed = FALSE) %>%
      as_epi_df(as_of = as.Date("2020-01-10")) %>%
      group_by(geo_value)
  )
})
