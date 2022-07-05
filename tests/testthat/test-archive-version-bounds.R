test_that("`validate_version_bound` allows/catches `NULL` as requested", {
  my_version_bound = NULL
  validate_version_bound(my_version_bound, null_ok=TRUE)
  expect_error(validate_version_bound(my_version_bound, null_ok=FALSE),
               class="epiprocess__my_version_bound_is_null")
  # Note that if the error class name changes, this test may produce some
  # confusing output along the following lines:
  #
  # > Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("testthat::expect_error(...)",  :
  # >   replacement has 4 rows, data has 3
})

test_that("`validate_version_bound` catches bounds that are the wrong length", {
  # Even if NULL is allowed, we should disallow other length-0 things:
  my_version_bound1 = integer(0L)
  expect_error(validate_version_bound(my_version_bound1, null_ok=TRUE),
               class="epiprocess__my_version_bound1_is_not_length_1")
  # And length > 1 things:
  my_version_bound2 = c(2, 10)
  expect_error(validate_version_bound(my_version_bound2, null_ok=TRUE),
               class="epiprocess__my_version_bound2_is_not_length_1")
})

test_that("`validate_version_bound` catches NA and outputs a sensible error message", {
  my_version_bound1 = as.Date(NA)
  expect_error(validate_version_bound(my_version_bound1, null_ok=FALSE),
               regexp = "satisfy `is.na`$",
               class = "epiprocess__my_version_bound1_is_na")
  my_version_bound2 = NA_integer_
  expect_error(validate_version_bound(my_version_bound2, null_ok=FALSE),
               regexp = "satisfy `is.na`$",
               class = "epiprocess__my_version_bound2_is_na")
})

test_that("`validate_version_bound` validate and class checks together allow and catch as intended", {
  my_int = 5L
  my_dbl = 5
  my_list = list(5L)
  my_date = as.Date("2000-01-01")
  my_datetime = vctrs::vec_cast(my_date, as.POSIXct(as.Date("1900-01-01")))
  # When first drafted, this validate function was a (validate+)cast function,
  # which used vctrs::vec_cast inside. However, the initial implementation
  # didn't actually allow casting to occur, and it was easier to change to the
  # current stringent validation than to think about what exactly casts to
  # allow. Some of the tests here were motivated based on that setup and have
  # been kept around. For example, we wouldn't want to allow casts between dates
  # and POSIXct's, because there are tz gotchas; these first couple of checks
  # detect that we have a validate-compatible date and datetime to make sure we
  # can properly help ward off the gotchas if switching to a cast rather than a
  # validate.
  expect_identical(vctrs::vec_cast(my_datetime, my_date), my_date)
  expect_identical(vctrs::vec_cast(my_date, my_datetime), my_datetime)
  #
  x_int = tibble::tibble(version = my_int)
  x_dbl = tibble::tibble(version = my_dbl)
  x_list = tibble::tibble(version = my_list)
  x_date = tibble::tibble(version = my_date)
  x_datetime = tibble::tibble(version = my_datetime)
  # Custom classes matter (test vectors and non-vctrs-specialized lists separately):
  my_version_bound1 = `class<-`(24, "c1")
  expect_error(validate_version_bound(my_version_bound1, x_int, null_ok=FALSE),
               class="epiprocess__my_version_bound1_has_invalid_class_or_typeof")
  my_version_bound2 = `class<-`(list(12), c("c2a","c2b","c2c"))
  expect_error(validate_version_bound(my_version_bound2, x_list, null_ok=FALSE),
               class="epiprocess__my_version_bound2_has_invalid_class_or_typeof")
  # Want no error matching date to date or datetime to datetime, but no interop due to tz issues:
  validate_version_bound(my_date, x_date, version_bound_arg="vb")
  validate_version_bound(my_datetime, x_datetime, version_bound_arg="vb")
  expect_error(validate_version_bound(my_datetime, x_date, null_ok=TRUE, version_bound_arg="vb"),
               class="epiprocess__vb_has_invalid_class_or_typeof")
  expect_error(validate_version_bound(my_date, x_datetime, null_ok=TRUE, version_bound_arg="vb"),
               class="epiprocess__vb_has_invalid_class_or_typeof")
  # Bad:
  expect_error(validate_version_bound(3.5, x_int, TRUE, "vb"))
  expect_error(validate_version_bound(.Machine$integer.max, x_dbl, TRUE, "vb"))
  expect_error(validate_version_bound(`class<-`(list(2), "clazz"),
                                      tibble::tibble(version=`class<-`(5L, "clazz")), TRUE, "vb"))
  # Maybe questionable:
  expect_error(validate_version_bound(3, x_int, TRUE, "vb"))
  expect_error(validate_version_bound(3L, x_dbl, TRUE, "vb"))
  # Good:
  validate_version_bound(my_int, x_int, TRUE, "vb")
  validate_version_bound(my_dbl, x_dbl, TRUE, "vb")
  validate_version_bound(my_list, x_list, TRUE, "vb")
  validate_version_bound(my_date, x_date, TRUE, "vb")
  validate_version_bound(my_datetime, x_datetime, TRUE, "vb")
})

test_that("archive version bounds args work as intended", {
  measurement_date = as.Date("2000-01-01")
  update_tbl = tibble::tibble(
    geo_value = "g1",
    time_value = measurement_date,
    version = measurement_date + 1:5,
    value = 1:5
  )
  expect_error(as_epi_archive(update_tbl,
                              clobberable_versions_start = 1241,
                              observed_versions_end = measurement_date),
               class="epiprocess__clobberable_versions_start_has_invalid_class_or_typeof")
  expect_error(as_epi_archive(update_tbl[integer(0L),]),
               class="epiprocess__max_version_cannot_be_used")
  expect_error(as_epi_archive(update_tbl,
                              clobberable_versions_start = NULL,
                              observed_versions_end = measurement_date),
               class="epiprocess__observed_versions_end_earlier_than_updates")
  expect_error(as_epi_archive(update_tbl,
                              clobberable_versions_start=measurement_date+6L,
                              observed_versions_end = measurement_date+5L),
               class="epiprocess__observed_versions_end_earlier_than_clobberable_versions_start")
  expect_error(as_epi_archive(update_tbl, observed_versions_end = NULL),
               regexp="observed_versions_end.*must not be NULL")
  ea_default = as_epi_archive(update_tbl)
  ea_default$as_of(measurement_date+4L)
  expect_warning(ea_default$as_of(measurement_date+5L),
                 class = "epiprocess__snapshot_as_of_clobberable_version")
  expect_error(ea_default$as_of(measurement_date+6L),
               regexp = "max_version.*at most.*observed_versions_end")
})
