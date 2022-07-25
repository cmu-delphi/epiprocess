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

test_that("quosure passing issue in epix_slide is resolved + other potential issues", {
  # (First part adapted from @examples)
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-02"),
                     by = "1 day")
  reference = epix_slide(x = archive_cases_dv_subset,
                         f = ~ mean(.x$case_rate_7d_av),
                         n = 3,
                         group_by = geo_value,
                         ref_time_values = time_values,
                         new_col_name = 'case_rate_3d_av')
  # test the passing-something-that-must-be-enquosed behavior:
  expect_identical(
    archive_cases_dv_subset$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      group_by = geo_value,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference
  )
  # test the passing-string-literal behavior:
  expect_identical(
    epix_slide(x = archive_cases_dv_subset,
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               group_by = "geo_value",
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference
  )
  expect_identical(
    archive_cases_dv_subset$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      group_by = "geo_value",
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference
  )
  # Might also want to test the passing-string-var-without-all_of behavior, but
  # make sure to set, trigger, then reset (or restore to old value) the
  # tidyselect once-per-session message about the ambiguity
  #
  # test the passing-all-of-string-var behavior:
  my_group_by = "geo_value"
  expect_identical(
    epix_slide(x = archive_cases_dv_subset,
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               group_by = tidyselect::all_of(my_group_by),
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference
  )
  expect_identical(
    archive_cases_dv_subset$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      group_by = tidyselect::all_of(my_group_by),
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference
  )
  # test the default behavior (default in this case should just be "geo_value"):
  expect_identical(
    epix_slide(x = archive_cases_dv_subset,
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference
  )
  expect_identical(
    archive_cases_dv_subset$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference
  )
})
