library(dplyr)

ea <- archive_cases_dv_subset$clone()

# epix_as_of tests
test_that("epix_as_of behaves identically to as_of method",{
  expect_identical(epix_as_of(ea,max_version = min(ea$DT$version)),
                   ea$as_of(max_version = min(ea$DT$version)))
})

test_that("Errors are thrown due to bad as_of inputs",{
  # max_version cannot be of string class rather than date class
  expect_error(ea$as_of("2020-01-01"))
  # max_version cannot be later than latest version
  expect_error(ea$as_of(as.Date("2025-01-01")))
  # max_version cannot be a vector
  expect_error(ea$as_of(c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
})

test_that("Warning against max_version being clobberable",{
  # none by default
  expect_warning(regexp = NA, ea$as_of(max_version = max(ea$DT$version)))
  expect_warning(regexp = NA, ea$as_of(max_version = min(ea$DT$version)))
  # but with `clobberable_versions_start` non-`NA`, yes
  ea_with_clobberable = ea$clone()
  ea_with_clobberable$clobberable_versions_start = max(ea_with_clobberable$DT$version)
  expect_warning(ea_with_clobberable$as_of(max_version = max(ea$DT$version)))
  expect_warning(regexp = NA, ea_with_clobberable$as_of(max_version = min(ea$DT$version)))
})

test_that("as_of properly grabs the data and doesn't mutate key",{

  d <- as.Date("2020-06-01")

  ea2 = tibble::tribble(
    ~geo_value, ~time_value,      ~version, ~cases,
          "ca", "2020-06-01", "2020-06-01",      1,
          "ca", "2020-06-01", "2020-06-02",      2,
    #
          "ca", "2020-06-02", "2020-06-02",      0,
          "ca", "2020-06-02", "2020-06-03",      1,
          "ca", "2020-06-02", "2020-06-04",      2,
    #
          "ca", "2020-06-03", "2020-06-03",      1,
    #
          "ca", "2020-06-04", "2020-06-04",      4,
    ) %>%
    dplyr::mutate(dplyr::across(c(time_value, version), as.Date)) %>%
    as_epi_archive()

  old_key = data.table::key(ea2$DT)

  edf_as_of <- ea2 %>%
    epix_as_of(max_version = as.Date("2020-06-03"))

  edf_expected <- as_epi_df(tibble(
    geo_value = "ca",
    time_value = d + 0:2,
    cases = c(2,1,1)
  ), as_of = as.Date("2020-06-03"))

  expect_equal(edf_as_of, edf_expected, ignore_attr=c(".internal.selfref", "sorted"))
  expect_equal(data.table::key(ea2$DT), old_key)
})
