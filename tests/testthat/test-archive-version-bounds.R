test_that("`validate_version_bound` allows/catches `NA` as requested", {
  my_version_bound <- NA
  version_class <- class(5L)
  validate_version_bound(my_version_bound, version_class, na_ok = TRUE)
  expect_error(validate_version_bound(my_version_bound, version_class, na_ok = FALSE),
    class = "epiprocess__version_bound_na_with_na_not_okay"
  )
})

test_that("`validate_version_bound` catches bounds that are the wrong length", {
  version_class <- class(5L)
  my_version_bound1a <- NULL
  expect_error(validate_version_bound(my_version_bound1a, version_class, na_ok = TRUE),
    class = "epiprocess__version_bound_null"
  )
  my_version_bound1b <- integer(0L)
  expect_error(validate_version_bound(my_version_bound1b, version_class, na_ok = TRUE),
    class = "epiprocess__version_bound_wrong_length"
  )
  my_version_bound2 <- c(2, 10)
  expect_error(validate_version_bound(my_version_bound2, version_class, na_ok = TRUE),
    class = "epiprocess__version_bound_wrong_length"
  )
})

test_that("`validate_version_bound` validate and class checks together allow and catch as intended", {
  my_int <- 5L
  my_dbl <- 5
  my_list <- list(5L)
  my_date <- as.Date("2000-01-01")
  my_datetime <- vctrs::vec_cast(my_date, as.POSIXct(as.Date("1900-01-01")))
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
  int_class <- class(my_int)
  dbl_class <- class(my_dbl)
  list_class <- class(my_list)
  date_class <- class(my_date)
  datetime_class <- class(my_datetime)
  # Custom classes matter (test vectors and non-vctrs-specialized lists separately):
  my_version_bound1 <- `class<-`(24, "c1")
  expect_error(
    validate_version_bound(my_version_bound1, int_class, na_ok = FALSE),
    regexp = "must have the same `class` vector as"
  )
  my_version_bound2 <- `class<-`(list(12), c("c2a", "c2b", "c2c"))
  expect_error(validate_version_bound(my_version_bound2, list_class, na_ok = FALSE), regexp = "must have the same `class`")
  # Want no error matching date to date or datetime to datetime, but no interop due to tz issues:
  validate_version_bound(my_date, date_class, version_bound_arg = "vb")
  validate_version_bound(my_datetime, datetime_class, version_bound_arg = "vb")
  expect_error(
    validate_version_bound(my_datetime, date_class, na_ok = TRUE, version_bound_arg = "vb"),
    regexp = "must have the same `class`",
    class = "epiprocess__version_bound_mismatched_class"
  )
  expect_error(
    validate_version_bound(my_date, datetime_class, na_ok = TRUE, version_bound_arg = "vb"),
    regexp = "must have the same `class`",
    class = "epiprocess__version_bound_mismatched_class"
  )
  # Bad:
  expect_error(validate_version_bound(3.5, int_class, TRUE, "vb"), regexp = "must have the same `class`")
  expect_error(validate_version_bound(.Machine$integer.max, dbl_class, TRUE, "vb"), regexp = "must have the same `class`")
  # Maybe questionable:
  expect_error(validate_version_bound(3, int_class, TRUE, "vb"))
  expect_error(validate_version_bound(3L, dbl_class, TRUE, "vb"))
  # Maybe questionable, but accept to relax things a bit, as this is happening
  # with Dates in some R(?) versions. Might need to turn some things into
  # vec_cast_common, but idea is just make Date stuff work for now:
  validate_version_bound(
    `class<-`(list(2), "clazz"),
    "clazz", TRUE, "vb"
  )
  # Good:
  validate_version_bound(
    `class<-`(2, "Date"),
    "Date", TRUE, "vb"
  )
  validate_version_bound(my_int, int_class, TRUE, "vb")
  validate_version_bound(my_dbl, dbl_class, TRUE, "vb")
  validate_version_bound(my_list, list_class, TRUE, "vb")
  validate_version_bound(my_date, date_class, TRUE, "vb")
  validate_version_bound(my_datetime, datetime_class, TRUE, "vb")
})

test_that("archive version bounds args work as intended", {
  measurement_date <- as.Date("2000-01-01")
  update_tbl <- tibble::tibble(
    geo_value = "ak",
    time_value = measurement_date,
    version = measurement_date + 1:5,
    value = 1:5
  )
  expect_error(
    as_epi_archive(update_tbl,
      clobberable_versions_start = 1241,
      versions_end = measurement_date
    ),
    regexp = "must have the same `class`"
  )
  expect_error(
    as_epi_archive(update_tbl[integer(0L), ]),
    regexp = "don't have a sensible guess at what version that is"
  )
  expect_error(
    as_epi_archive(update_tbl,
      clobberable_versions_start = NA,
      versions_end = measurement_date
    ),
    regexp = "the archive contained updates for a later version"
  )
  expect_error(
    as_epi_archive(update_tbl,
      clobberable_versions_start = measurement_date + 6L,
      versions_end = measurement_date + 5L
    ),
    regexp = "`x\\$clobberable_versions_start`.*indicating that there were later observed versions"
  )
  expect_error(as_epi_archive(update_tbl, versions_end = NA),
    class = "epiprocess__version_bound_na_with_na_not_okay"
  )
  ea_default <- as_epi_archive(update_tbl)
  ea_default %>% epix_as_of(measurement_date + 4L)
  expect_warning(
    regexp = NA,
    ea_default %>% epix_as_of(measurement_date + 5L),
    class = "epiprocess__snapshot_epix_as_of_clobberable_version"
  )
  ea_default %>% epix_as_of(measurement_date + 5L)
  expect_error(ea_default %>% epix_as_of(measurement_date + 6L))
})
