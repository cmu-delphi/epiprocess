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
    merge(dt1,dt2)
  )
})

test_that("data.table merging works as intended",{
  ea <- archive_cases_dv_subset$clone()
  dt1 <- select(ea$DT , -case_rate_7d_av)
  ea1 <- as_epi_archive(dt1)
  dt2 <- select(ea$DT , -percent_cli)
  
  expect_identical(
    as_epi_archive(ea$DT),
    as_epi_archive(merge(dt1,dt2))
  )
})

# (epi_archive) slide tests
test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

test_that("epix_slide works as intended",{
  x2 <- ea$clone()$DT %>%
    filter(geo_value == "ca", version <= as.Date("2020-06-09")) %>%
    select(-percent_cli,-case_rate_7d_av) %>%
    mutate(binary = 2^(row_number()-1)) %>%
    as_epi_archive()
  
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-09"),
                     by = "1 day")
  xx <- epix_slide(x = x2,
            f = ~ sum(.x$binary),
            n = 3,
            group_by = geo_value,
            ref_time_values = time_values,
            new_col_name = '3d_sum_binary')
})