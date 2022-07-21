
test_that("epix_fill_through_version mirrors input when it is sufficiently up to date", {
  ea_orig = as_epi_archive(data.table::data.table(geo_value = "g1", time_value = as.Date("2020-01-01"),
                                                  version = 1:5, value = 1:5))
  some_earlier_observed_version = 2L
  ea_trivial_fill_na1 = epix_fill_through_version(ea_orig, some_earlier_observed_version, "na")
  ea_trivial_fill_na2 = epix_fill_through_version(ea_orig, ea_orig$observed_versions_end, "na")
  ea_trivial_fill_lvcf = epix_fill_through_version(ea_orig, some_earlier_observed_version, "lvcf")
  # Below, we want R6 objects to be compared based on contents rather than
  # addresses. We appear to get this with `expect_identical` in `testthat`
  # edition 3, which is based on `waldo::compare` rather than `base::identical`;
  # `waldo::compare` in waldo >=0.3.1 appears (as of 0.4.0) to compare R6
  # objects by contents rather than address (in a way that is tested but maybe
  # not guaranteed via user docs). Use `local_edition` to ensure we use edition
  # 3 here.
  local_edition(3)
  expect_identical(ea_orig, ea_trivial_fill_na1)
  expect_identical(ea_orig, ea_trivial_fill_na2)
  expect_identical(ea_orig, ea_trivial_fill_lvcf)
})

test_that("epix_fill_through_version can extend observed versions, gives expected `as_of`s", {
  ea_orig = as_epi_archive(data.table::data.table(
    geo_value = "g1",
    time_value = as.Date("2020-01-01") + c(rep(0L,5L), 1L),
    version = c(1:5, 2L),
    value = 1:6))
  first_unobserved_version = 6L
  later_unobserved_version = 10L
  ea_fill_na = epix_fill_through_version(ea_orig, later_unobserved_version, "na")
  ea_fill_lvcf = epix_fill_through_version(ea_orig, later_unobserved_version, "lvcf")

  # We use edition 3 features here, passing `ignore_attr` to `waldo::compare`.
  # Ensure we are using edition 3:
  local_edition(3)
  withCallingHandlers({
    expect_identical(ea_fill_na$observed_versions_end, later_unobserved_version)
    expect_identical(tibble::as_tibble(ea_fill_na$as_of(first_unobserved_version)),
                     tibble::tibble(geo_value="g1", time_value=as.Date("2020-01-01")+0:1, value=rep(NA_integer_, 2L)),
                     ignore_attr = TRUE)
    expect_identical(ea_fill_lvcf$observed_versions_end, later_unobserved_version)
    expect_identical(ea_fill_lvcf$as_of(first_unobserved_version),
                     ea_fill_lvcf$as_of(ea_orig$observed_versions_end) %>%
                       {attr(., "metadata")$as_of <- first_unobserved_version; .})
  }, epiprocess__snapshot_as_of_clobberable_version = function(wrn) invokeRestart("muffleWarning"))
})

test_that("epix_fill_through_version does not mutate x", {
  ea_orig = as_epi_archive(data.table::data.table(geo_value = "g1", time_value = as.Date("2020-01-01"),
                                                  version = 1:5, value = 1:5))
  # We want to perform a strict comparison of the contents of `ea_orig` before
  # and `ea_orig` after. `clone` + `expect_identical` based on waldo would sort
  # of work, but we might want something stricter. `as.list` + `identical` seems
  # to do the trick
  ea_orig_before_as_list = as.list(ea_orig)
  some_unobserved_version = 8L
  ea_fill_na = epix_fill_through_version(ea_orig, some_unobserved_version, "na")
  ea_orig_after_as_list = as.list(ea_orig)
  # use identical, not expect_identical; latter isn't as strict
  expect_true(identical(ea_orig_before_as_list, ea_orig_after_as_list))
})

test_that("x$fill_through_version mutates x (if needed)", {
  ea = as_epi_archive(data.table::data.table(geo_value = "g1", time_value = as.Date("2020-01-01"),
                                                  version = 1:5, value = 1:5))
  # We want the contents to change in a substantial way that makes waldo compare
  # different (if the contents need to change).
  ea_before_copies_as_list = lapply(ea, data.table::copy)
  some_unobserved_version = 8L
  ea$fill_through_version(some_unobserved_version, "na")
  ea_after_copies_as_list = lapply(ea, data.table::copy)
  expect_failure(expect_identical(ea_before_copies_as_list, ea_after_copies_as_list))
})

test_that("{epix_,$}fill_through_version return with expected visibility", {
  ea = as_epi_archive(data.table::data.table(geo_value = "g1", time_value = as.Date("2020-01-01"),
                                             version = 1:5, value = 1:5))
  expect_true(withVisible(epix_fill_through_version(ea, 10L, "na"))[["visible"]])
  expect_false(withVisible(ea$fill_through_version(15L, "na"))[["visible"]])
})
