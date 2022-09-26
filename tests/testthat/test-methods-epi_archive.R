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

test_that("quosure passing issue in epix_slide is resolved + other potential issues", {
  # (First part adapted from @examples)
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-02"),
                     by = "1 day")
  # We only have one non-version, non-time key in the example archive. Add
  # another so that we don't accidentally pass tests due to accidentally
  # matching the default grouping.
  ea = as_epi_archive(archive_cases_dv_subset$DT %>%
                        dplyr::mutate(modulus = seq_len(nrow(.)) %% 5L),
                      other_keys = "modulus",
                      compactify = TRUE)
  reference_by_modulus = ea %>%
    group_by(modulus) %>%
    epix_slide(f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av')
  reference_by_neither = ea %>%
    group_by() %>%
    epix_slide(f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av')
  # test the passing-something-that-must-be-enquosed behavior:
  #
  # (S3 group_by behavior for this case is the `reference_by_modulus`)
  expect_identical(
    ea$group_by(modulus)$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the .data pronoun behavior:
  expect_identical(
    epix_slide(x = ea %>% group_by(.data$modulus),
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(.data$modulus)$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the passing across-all-of-string-literal behavior:
  expect_identical(
    epix_slide(x = ea %>% group_by(dplyr::across(all_of("modulus"))),
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(across(all_of("modulus")))$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the passing-across-all-of-string-var behavior:
  my_group_by = "modulus"
  expect_identical(
    epix_slide(x = ea %>% group_by(dplyr::across(tidyselect::all_of(my_group_by))),
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(dplyr::across(tidyselect::all_of(my_group_by)))$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the default behavior (default in this case should just be grouping by neither):
  expect_identical(
    epix_slide(x = ea,
               f = ~ mean(.x$case_rate_7d_av),
               n = 3,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_neither
  )
  expect_identical(
    ea$slide(
      f = ~ mean(.x$case_rate_7d_av),
      n = 3,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_neither
  )
})
