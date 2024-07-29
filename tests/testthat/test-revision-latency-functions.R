dummy_ex <- tibble::tribble(
  ~geo_value, ~time_value, ~version, ~value,
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
) %>%
  as_epi_archive(compactify = FALSE)

test_that("revision_summary works for a dummy dataset", {
  expect_snapshot(dummy_ex %>% revision_summary() %>% print(n = 10, width = 300))
  expect_snapshot(dummy_ex %>% revision_summary(drop_nas = FALSE) %>% print(n = 10, width = 300))
})
test_that("tidyselect is functional", {
  expect_no_error(revision_summary(dummy_ex, value))
})
test_that("revision_summary works for various timetypes", {})
