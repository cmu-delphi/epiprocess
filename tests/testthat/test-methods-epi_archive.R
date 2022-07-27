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

test_that("Warning against max_version being same as edf's max version",{
  expect_warning(ea$as_of(max_version = max(ea$DT$version)))
  expect_warning(ea$as_of(max_version = min(ea$DT$version)),NA)
})

test_that("as_of properly grabs the data",{
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
  
  df_as_of <- ea2 %>%
    epix_as_of(max_version = as.Date("2020-06-03")) %>%
    as_tibble()
  
  df_expected <- tibble(
    geo_value = "ca",
    time_value = d + 0:2,
    cases = c(2,1,1)
  )
  
  expect_identical(df_as_of[[1]],df_expected[[1]])
  expect_identical(df_as_of[[2]],df_expected[[2]])
  expect_identical(df_as_of[[3]],df_expected[[3]])
})

# epix_merge tests
test_that("epix_merge requires second argument to be a data.table or
          epi_archive",{
  expect_error(epix_merge(ea,data.frame(x=1)))
})

test_that("data.table merging is utilized if second argument is a data.table",{
  dt1 <- select(ea$DT , -case_rate_7d_av)
  ea1 <- as_epi_archive(dt1)
  dt2 <- select(ea$DT , -percent_cli)
  
  expect_identical(
    epix_merge(ea1,dt2),
    merge(dt1,dt2,all=TRUE)
  )
})

test_that("data.table merging works as intended",{
  ea <- archive_cases_dv_subset$clone()
  dt1 <- select(ea$DT , -case_rate_7d_av)
  ea1 <- as_epi_archive(dt1)
  dt2 <- select(ea$DT , -percent_cli)
  
  expect_identical(
    as_epi_archive(ea$DT),
    as_epi_archive(merge(dt1,dt2,all=TRUE))
  )
})
