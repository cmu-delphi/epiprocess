
test_that("epix_merge requires stops on invalid `y`",{
  ea = archive_cases_dv_subset$clone()
  expect_error(epix_merge(ea, data.frame(x=1)))
})

test_that("epix_merge merges and carries forward updates properly", {
  x = as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(~geo_value, ~time_value, ~version, ~x_value,
                      # same version set for x and y
                      "g1", 1L, 1:3, paste0("XA", 1:3),
                      # versions of x surround those of y + this measurement has
                      # max update version beyond some others
                      "g1", 2L, 1:5, paste0("XB", 1:5),
                      # mirror case
                      "g1", 3L, 2L, paste0("XC", 2L),
                      # x has 1 version, y has 0
                      "g1", 4L, 1L, paste0("XD", 1L),
                      # non-NA values that should be carried forward
                      # (version-wise LOCF) in other versions, plus NAs that
                      # should (similarly) be carried forward as NA (latter
                      # wouldn't work with an ordinary merge + post-processing
                      # with `data.table::nafill`)
                      "g1", 6L, c(1L,3L,5L), paste0("XE", c(1L, NA, 5L))
                      ) %>%
        tidyr::unchop(c(version, x_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  y = as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(~geo_value, ~time_value, ~version, ~y_value,
                      "g1", 1L, 1:3, paste0("YA", 1:3),
                      "g1", 2L, 2L, paste0("YB", 2L),
                      "g1", 3L, 1:5, paste0("YC", 1:5),
                      "g1", 5L, 1L, paste0("YD", 1L),
                      "g1", 6L, 1:5, paste0("YE", 1:5),
                      ) %>%
        tidyr::unchop(c(version, y_value)) %>%
        dplyr::mutate(dplyr::across(c(y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  xy = epix_merge(x, y)
  xy_expected = as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(~geo_value, ~time_value, ~version, ~x_value, ~y_value,
                      "g1", 1L, 1:3, paste0("XA", 1:3), paste0("YA", 1:3),
                      "g1", 2L, 1:5, paste0("XB", 1:5), paste0("YB", c(NA,2L,2L,2L,2L)),
                      "g1", 3L, 1:5, paste0("XC", c(NA,2L,2L,2L,2L)), paste0("YC", 1:5),
                      "g1", 4L, 1L, paste0("XD", 1L), paste0("YD", NA),
                      "g1", 5L, 1L, paste0("XD", NA), paste0("YD", 1L),
                      "g1", 6L, 1:5, paste0("XE", c(1L,1L,NA,NA,5L)), paste0("YE", 1:5),
                      ) %>%
        tidyr::unchop(c(version, x_value, y_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value, y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  # We rely on testthat edition 3 expect_identical using waldo, not identical. See
  # test-epix_fill_through_version.R comments for details.
  local_edition(3)
  expect_identical(xy, xy_expected)
})

test_that('epix_merge stops and warns on metadata and naming issues', {
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value="tx", time_value=1L, version=1L, x_value=1L)),
      as_epi_archive(tibble::tibble(geo_value="us", time_value=1L, version=5L, y_value=2L))
    ),
    regexp = "must have the same.*geo_type"
  )
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value="pa", time_value=1L, version=1L, x_value=1L)),
      as_epi_archive(tibble::tibble(geo_value="pa", time_value=as.Date("2020-01-01"), version=5L, y_value=2L))
    ),
    regexp = "must have the same.*time_type"
  )
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, value=1L)),
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, value=2L))
    ),
    regexp = "overlapping.*names"
  )
  expect_warning(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, x_value=1L),
                     additional_metadata=list("updates_fetched"=lubridate::ymd_hms("2022-05-01 16:00:00", tz="UTC"))),
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, y_value=2L))
    ),
    regexp = "x\\$additional_metadata",
    class = "epiprocess__epix_merge_ignores_additional_metadata"
  )
  expect_warning(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, x_value=1L)),
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, y_value=2L),
                     additional_metadata=list("updates_fetched"=lubridate::ymd_hms("2022-05-01 16:00:00", tz="UTC")))
    ),
    regexp = "y\\$additional_metadata",
    class = "epiprocess__epix_merge_ignores_additional_metadata"
  )
})

# use `local` to prevent accidentally using the x, y, xy bindings here
# elsewhere, while allowing reuse across a couple tests
local({
  x = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, x_value=1L),
                     clobberable_versions_start=1L, versions_end = 10L)
  y = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, y_value=2L),
                     clobberable_versions_start=3L, versions_end = 10L)
  xy = epix_merge(x,y)
  test_that('epix_merge considers partially-clobberable row to be clobberable', {
    expect_identical(xy$clobberable_versions_start, 1L)
  })
  test_that('epix_merge result uses versions_end metadata not max version val', {
    expect_identical(xy$versions_end, 10L)
  })
})

local({
  x = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, x_value=10L))
  y = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=5L, y_value=20L))
  print(epix_merge(x,y, versions_end_conflict = "na"))
  test_that('epix_merge stops on versions_end_conflict default or "stop"', {
    expect_error(epix_merge(x,y),
                 class="epiprocess__epix_merge_unresolved_versions_end_conflict")
    expect_error(epix_merge(x,y, versions_end_conflict = "stop"),
                 class="epiprocess__epix_merge_unresolved_versions_end_conflict")
  })
  test_that('epix_merge versions_end_conflict="na" works', {
    expect_equal(
      epix_merge(x,y, versions_end_conflict = "na"),
      as_epi_archive(tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        1L, 1L, 1L, 10L, NA_integer_,         # x updated, y not observed yet
        1L, 1L, 2L, NA_integer_, NA_integer_, # NA-ing out x, y not observed yet
        1L, 1L, 5L, NA_integer_, 20L,         # x still NA, y updated
        ), clobberable_versions_start=1L)
    )
  })
  test_that('epix_merge versions_end_conflict="locf" works', {
    expect_equal(
      epix_merge(x,y, versions_end_conflict = "locf"),
      as_epi_archive(tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        1L, 1L, 1L, 10L, NA_integer_,  # x updated, y not observed yet
        1L, 1L, 5L, 10L, 20L, # x LOCF'd, y updated
        ), clobberable_versions_start=1L)
    )
  })
  x_no_conflict = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, x_value=10L))
  y_no_conflict = as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=1L, y_value=20L))
  xy_no_conflict_expected = as_epi_archive(tibble::tribble(
    ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
    1L, 1L, 1L, 10L, 20L,         # x updated, y not observed yet
    ))
  test_that('epix_merge versions_end_conflict="stop" on no-conflict works', {
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, versions_end_conflict = "stop"),
      xy_no_conflict_expected
    )
  })
  test_that('epix_merge versions_end_conflict="na" on no-conflict works', {
    # This test is the main reason for these no-conflict tests. We want to make
    # sure that we don't add an unnecessary NA-ing-out version beyond a common
    # versions_end.
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, versions_end_conflict = "na"),
      xy_no_conflict_expected
    )
  })
  test_that('epix_merge versions_end_conflict="locf" on no-conflict works', {
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, versions_end_conflict = "locf"),
      xy_no_conflict_expected
    )
  })
})


test_that('epix_merge versions_end_conflict="na" balks if do not know next_after', {
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=as.POSIXct(as.Date("2020-01-01")), x_value=10L)),
      as_epi_archive(tibble::tibble(geo_value=1L, time_value=1L, version=as.POSIXct(as.Date("2020-01-02")), y_value=20L)),
      versions_end_conflict = "na"
    ),
    regexp = "no applicable method.*next_after"
  )
})
