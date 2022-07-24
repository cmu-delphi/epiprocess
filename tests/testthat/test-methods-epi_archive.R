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

test_that("as_of properly grabs the data and doesn't mutate key",{
  old_key = data.table::key(ea$DT)

  df_as_of <- ea %>%
    epix_as_of(max_version = as.Date("2020-07-01")) %>%
    na.omit() %>%
    as.data.frame()
    
  df_filter <- ea$DT %>%
    filter(version == as.Date("2020-07-01")) %>%
    na.omit() %>%
    select(-version) %>%
    as.data.frame()
  
  expect_equal(df_as_of[1:4],df_filter)
  expect_equal(data.table::key(ea$DT), old_key)
})
