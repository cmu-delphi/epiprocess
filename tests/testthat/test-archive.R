library(dplyr)

test_that("first input must be a data.frame",{
  expect_error(as_epi_archive(c(1,2,3)),
               regexp="`x` must be a data frame.")
})

dt <- archive_cases_dv_subset$DT

test_that("data.frame must contain geo_value, time_value and version columns",{
  expect_error(as_epi_archive(select(dt,-geo_value)),
               regexp="`x` must contain a `geo_value` column.")
  expect_error(as_epi_archive(select(dt,-time_value)),
               regexp="`x` must contain a `time_value` column.")
  expect_error(as_epi_archive(select(dt,-version)),
               regexp="`x` must contain a `version` column.")
})

test_that("other_keys can only contain names of the data.frame columns",{
  expect_error(as_epi_archive(dt,other_keys = "xyz"),
               regexp="`other_keys` must be contained in the column names of `x`.")
  expect_error(as_epi_archive(dt,other_keys = "percent_cli"),NA)
})

test_that("other_keys cannot contain names geo_value, time_value or version",{
  expect_error(as_epi_archive(dt,other_keys = "geo_value"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  expect_error(as_epi_archive(dt,other_keys = "time_value"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  expect_error(as_epi_archive(dt,other_keys = "version"),
               regexp="`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
})