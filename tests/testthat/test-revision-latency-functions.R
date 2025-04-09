dummy_ex <- tibble::tribble(
  ~geo_value, ~time_value, ~version, ~value,
  # ak 1 has 4 revisions w/out NAs, but 6 with NAs
  # a min lag of 2, and a change of 101
  "ak", as.Date("2020-01-01"), as.Date("2020-01-03"), 1,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-05"), NA,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-06"), 1,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-07"), 5,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-08"), 6,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-09"), 7,
  "ak", as.Date("2020-01-01"), as.Date("2020-01-20"), 102,
  # ak 2 has 1 revision a min lag of 4, a change of 9, and a rel change of 9%
  "ak", as.Date("2020-01-02"), as.Date("2020-01-06"), 100,
  "ak", as.Date("2020-01-02"), as.Date("2020-01-07"), 91,
  # ak 3 has 0 revisions, and a value of zero, and thus a rel_spread of NaN
  "ak", as.Date("2020-01-03"), as.Date("2020-01-06"), 0,
  "ak", as.Date("2020-01-03"), as.Date("2020-01-07"), 0,
  # al 1 has 1 real revision, a lag of 0, and changes by 99
  "al", as.Date("2020-01-01"), as.Date("2020-01-01"), 1,
  "al", as.Date("2020-01-01"), as.Date("2020-01-10"), 1,
  "al", as.Date("2020-01-01"), as.Date("2020-01-20"), 100,
  # al 2 has no revision, a min lag of 0, and a rel_spread of 0
  "al", as.Date("2020-01-02"), as.Date("2020-01-02"), 1,
  # al 3 has 1 revision and a min lag of 1, and a change of 3
  "al", as.Date("2020-01-03"), as.Date("2020-01-04"), 1,
  "al", as.Date("2020-01-03"), as.Date("2020-01-05"), 4,
  # al 4 has 1 revision including NA's none if not, a lag of 0/1 and changes of 0
  "al", as.Date("2020-01-04"), as.Date("2020-01-04"), NA,
  "al", as.Date("2020-01-04"), as.Date("2020-01-05"), 9,
) %>%
  as_epi_archive(versions_end = as.Date("2022-01-01"), compactify = FALSE)

dummy_ex_weekly <- dummy_ex$DT %>%
  mutate(across(
    c(time_value, version),
    ~ as.Date("2020-01-01") + 7 * as.numeric(.x - as.Date("2020-01-01"))
  )) %>%
  as_epi_archive(
    versions_end = as.Date("2022-01-01") + as.numeric(as.Date("2022-01-01") - as.Date("2020-01-01")) %% 7,
    compactify = FALSE
  )
stopifnot(dummy_ex_weekly$time_type == "week")

dummy_ex_yearmonthly <- dummy_ex$DT %>%
  mutate(across(
    c(time_value, version),
    ~ tsibble::make_yearmonth(2020, 1) + as.numeric(.x - as.Date("2020-01-01"))
  )) %>%
  as_epi_archive(
    versions_end = tsibble::make_yearmonth(2020, 1) + as.numeric(as.Date("2022-01-01") - as.Date("2020-01-01")),
    compactify = FALSE
  )
stopifnot(dummy_ex_yearmonthly$time_type == "yearmonth")

dummy_ex_integerly <- dummy_ex$DT %>%
  mutate(across(
    c(time_value, version),
    ~ 1 + as.numeric(.x - as.Date("2020-01-01"))
  )) %>%
  as_epi_archive(
    versions_end = 1 + as.numeric(as.Date("2022-01-01") - as.Date("2020-01-01")),
    compactify = FALSE
  )
stopifnot(dummy_ex_integerly$time_type == "integer")

test_that("revision_summary works for dummy datasets", {
  rs1 <- dummy_ex %>% revision_summary()
  rs2 <- dummy_ex %>% revision_summary(drop_nas = FALSE)
  expect_snapshot(rs1)
  expect_snapshot(rs1$revision_behavior %>% print(n = 10, width = 300))
  expect_snapshot(rs2)
  expect_snapshot(rs2$revision_behavior %>% print(n = 10, width = 300))

  # Weekly dummy is mostly just "day" -> "week", but quick-revision summary changes:
  rs3 <- dummy_ex_weekly %>% revision_summary(drop_nas = FALSE)
  expect_snapshot(rs3)
  expect_snapshot(rs3$revision_behavior %>% print(n = 10, width = 300))
  # Yearmonthly has the same story. It would have been close to encountering
  # min_waiting_period-based filtering but we actually set its versions_end to
  # sometime in 2080 rather than 2022:
  rs4 <- dummy_ex_yearmonthly %>% revision_summary(drop_nas = FALSE)
  expect_snapshot(rs4)
  expect_snapshot(rs4$revision_behavior %>% print(n = 10, width = 300))
  # Integer is very much like daily. We have to provide some of the
  # configuration arguments since we have no idea about what the integers
  # represent. If the possible integers being used have large jumps like
  # YYYYww-as-integer epiweek labels (e.g., 200053 jumps to 200101) or are
  # regularly spaced apart but by more than 1, we'll still be producing
  # something nonsensical, but we tried.
  rs5 <- dummy_ex_integerly %>%
    revision_summary(
      min_waiting_period = 60, quick_revision = 3,
      drop_nas = FALSE
    )
  expect_snapshot(rs5)
  expect_snapshot(rs5$revision_behavior %>% print(n = 10, width = 300))
})

test_that("tidyselect is functional", {
  expect_no_error(quiet(revision_summary(dummy_ex, value)))
  expect_no_error(quiet(revision_summary(dummy_ex, starts_with("val"))))
  # column order shouldn't matter
  with_later_key_col <- dummy_ex$DT %>%
    select(geo_value, time_value, value, version) %>%
    as_epi_archive(versions_end = dummy_ex$versions_end, compactify = FALSE)
  expect_equal(
    quiet(revision_summary(with_later_key_col)),
    quiet(revision_summary(dummy_ex))
  )
  # extra column shouldn't interfere
  with_later_val_col <- dummy_ex$DT %>%
    mutate(value2 = 0) %>%
    as_epi_archive(versions_end = dummy_ex$versions_end, compactify = FALSE)
  expect_equal(
    quiet(revision_summary(with_later_val_col, value)),
    quiet(revision_summary(dummy_ex, value))
  )
  # error when which column we're summarizing is ambiguous
  expect_error(
    dummy_ex$DT %>%
      copy() %>%
      mutate(value2 = value) %>%
      as_epi_archive(
        versions_end = dummy_ex$versions_end,
        compactify = FALSE
      ) %>%
      revision_summary(),
    class = "epiprocess__revision_summary_cannot_determine_default_selection"
  )
  expect_error(revision_summary(with_later_val_col, !everything()),
    class = "epiprocess__revision_summary__selected_zero_columns"
  )
})

test_that("revision_summary default min_waiting_period works as expected", {
  # just outside the window for daily data
  expect_equal(
    tibble(
      geo_value = 1,
      time_value = as.Date("2020-01-01") + 0:1,
      version = time_value + 1,
      value = 1:2
    ) %>%
      as_epi_archive(versions_end = as.Date("2020-01-01") + 1 + 59) %>%
      revision_summary(return_only_tibble = TRUE) %>%
      pull(time_value),
    as.Date("2020-01-01")
  )
  # just outside the window for weekly data
  expect_equal(
    tibble(
      geo_value = 1,
      time_value = as.Date("2020-01-01") + 7 * (0:1),
      version = time_value + 35,
      value = 1:2
    ) %>%
      as_epi_archive(versions_end = as.Date("2020-01-01") + 7 + 56) %>%
      revision_summary(return_only_tibble = TRUE) %>%
      pull(time_value),
    as.Date("2020-01-01")
  )
  # just outside the window for monthly data
  expect_equal(
    tibble(
      geo_value = 1,
      time_value = tsibble::make_yearmonth(2000, 1:2),
      version = time_value + 1,
      value = 1:2
    ) %>%
      as_epi_archive(versions_end = tsibble::make_yearmonth(2000, 3)) %>%
      revision_summary(return_only_tibble = TRUE) %>%
      pull(time_value),
    tsibble::make_yearmonth(2000, 1)
  )
  # we don't know how to interpret the default in terms of "integer" time_type
  expect_error(
    tibble(
      geo_value = 1,
      time_value = 1:2 + 0,
      version = time_value + 1,
      value = 1:2
    ) %>%
      as_epi_archive(versions_end = 1 + 1 + 59) %>%
      revision_summary(return_only_tibble = TRUE),
    regexp = "Unsupported time_type"
  )
})
