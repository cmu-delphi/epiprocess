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

# FIXME TODO turn these into tests in another file

# local({
#   on.exit(if (exists("con")) DBI::dbDisconnect(con))
#   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#   mtcars_with_id <- mtcars %>% mutate(id = as_ukey_col_heavyprefix(seq_len(nrow(.))))
#   copy_to(con, mtcars_with_id)
#   mtcars_with_id_dbtbl <- tbl(con, "mtcars_with_id")
#   mtcars_with_id_dbtbl %>% pull(id) %>% class()
# })

# data.table allows ukey cols as keys and keeps their classes, though
# it wouldn't perform further management

# tsibble(t = as_ukey_col_heavyprefix(as.Date("2020-01-01") + 1:5 - 1), index = t)

# This one might also be solved by the cast but might require extra handling
#
# tibble(geo_value = as_ukey_col_heavyprefix("ak"), time_value = as_ukey_col_heavyprefix(as.Date("2020-01-01") + 1:5), value = 1:5) %>% as_epi_df()
