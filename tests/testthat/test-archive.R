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

test_that("Warning thrown when other_metadata contains overlapping names with
          geo_type or time_type fields",{
  expect_warning(as_epi_archive(dt,additional_metadata = list(geo_type = 1)),
                 regexp="`additional_metadata` names overlap with existing metadata fields\n\"geo_type\", \"time_type\".")
  expect_warning(as_epi_archive(dt,additional_metadata = list(time_type = 1)),
                 regexp="`additional_metadata` names overlap with existing metadata fields\n\"geo_type\", \"time_type\".")
})