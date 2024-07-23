test_date <- as.Date("2020-01-01")

test_that("epix_fill_through_version mirrors input when it is sufficiently up to date", {
  ea_orig <- as_epi_archive(data.table::data.table(
    geo_value = "ak", time_value = test_date,
    version = test_date + 1:5, value = 1:5
  ))
  some_earlier_observed_version <- test_date + 2L
  ea_trivial_fill_na1 <- epix_fill_through_version(ea_orig, some_earlier_observed_version, "na")
  ea_trivial_fill_na2 <- epix_fill_through_version(ea_orig, ea_orig$versions_end, "na")
  ea_trivial_fill_locf <- epix_fill_through_version(ea_orig, some_earlier_observed_version, "locf")

  expect_identical(ea_orig, ea_trivial_fill_na1)
  expect_identical(ea_orig, ea_trivial_fill_na2)
  expect_identical(ea_orig, ea_trivial_fill_locf)
})

test_that("epix_fill_through_version can extend observed versions, gives expected `as_of`s", {
  ea_orig <- as_epi_archive(data.table::data.table(
    geo_value = "ak",
    time_value = test_date + c(rep(0L, 5L), 1L),
    version = test_date + c(1:5, 2L),
    value = 1:6
  ))
  first_unobserved_version <- test_date + 6L
  later_unobserved_version <- test_date + 10L
  ea_fill_na <- epix_fill_through_version(ea_orig, later_unobserved_version, "na")
  ea_fill_locf <- epix_fill_through_version(ea_orig, later_unobserved_version, "locf")

  withCallingHandlers(
    {
      expect_identical(ea_fill_na$versions_end, later_unobserved_version)
      expect_identical(tibble::as_tibble(epix_as_of(ea_fill_na, first_unobserved_version)),
        tibble::tibble(geo_value = "ak", time_value = test_date + 0:1, value = rep(NA_integer_, 2L)),
        ignore_attr = TRUE
      )
      expect_identical(ea_fill_locf$versions_end, later_unobserved_version)
      expect_identical(
        epix_as_of(ea_fill_locf, first_unobserved_version),
        epix_as_of(ea_fill_locf, ea_orig$versions_end) %>%
          {
            attr(., "metadata")$as_of <- first_unobserved_version
            .
          }
      )
    },
    epiprocess__snapshot_as_of_clobberable_version = function(wrn) invokeRestart("muffleWarning")
  )
})

test_that("epix_fill_through_version does not mutate x", {
  for (ea_orig in list(
    # vanilla case
    as_epi_archive(data.table::data.table(
      geo_value = "ak", time_value = test_date,
      version = test_date + 1:5, value = 1:5
    )),
    # data.table unique yielding original DT by reference special case (maybe
    # having only 1 row is the trigger? having no revisions of initial values
    # doesn't seem sufficient to trigger)
    as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date + 1, version = test_date + 1, value = 10L))
  )) {
    ea_orig_before <- clone(ea_orig)

    ea_fill_na <- epix_fill_through_version(ea_orig, test_date + 8, "na")
    expect_identical(ea_orig_before, ea_orig)

    ea_fill_locf <- epix_fill_through_version(ea_orig, test_date + 8, "locf")
    expect_identical(ea_orig_before, ea_orig)
  }
})

test_that("epix_fill_through_version return with expected visibility", {
  ea <- as_epi_archive(data.table::data.table(
    geo_value = "ak", time_value = test_date,
    version = test_date + 1:5, value = 1:5
  ))
  expect_true(withVisible(epix_fill_through_version(ea, test_date + 10L, "na"))[["visible"]])
})

test_that("epix_fill_through_version returns same key & doesn't mutate old DT or its key", {
  ea <- as_epi_archive(
    tibble::tibble(geo_value = "ak", time_value = test_date + 1, version = test_date + 1, value = 10L)
  )
  old_dt_copy <- data.table::copy(ea$DT)
  old_key <- data.table::key(ea$DT)
  expect_identical(data.table::key(epix_fill_through_version(ea, test_date + 5L, "na")$DT), old_key)
  expect_identical(data.table::key(epix_fill_through_version(ea, test_date + 5L, "locf")$DT), old_key)
  expect_identical(data.table::key(ea$DT), old_key)
})
