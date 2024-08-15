test_date <- as.Date("2020-01-01")

test_that("epix_merge requires forbids on invalid `y`", {
  ea <- archive_cases_dv_subset
  expect_error(epix_merge(ea, data.frame(x = 1)))
})

test_that("epix_merge merges and carries forward updates properly", {
  x <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value,
        # same version set for x and y
        "ak", test_date + 1, test_date + 1:3, paste0("XA", 1:3),
        # versions of x surround those of y + this measurement has
        # max update version beyond some others
        "ak", test_date + 2, test_date + 1:5, paste0("XB", 1:5),
        # mirror case
        "ak", test_date + 3, test_date + 2L, paste0("XC", 2L),
        # x has 1 version, y has 0
        "ak", test_date + 4, test_date + 1L, paste0("XD", 1L),
        # non-NA values that should be carried forward
        # (version-wise LOCF) in other versions, plus NAs that
        # should (similarly) be carried forward as NA (latter
        # wouldn't work with an ordinary merge + post-processing
        # with `data.table::nafill`)
        "ak", test_date + 6, test_date + c(1L, 3L, 5L), paste0("XE", c(1L, NA, 5L))
      ) %>%
        tidyr::unchop(c(version, x_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  y <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~y_value,
        "ak", test_date + 1, test_date + 1:3, paste0("YA", 1:3),
        "ak", test_date + 2, test_date + 2L, paste0("YB", 2L),
        "ak", test_date + 3, test_date + 1:5, paste0("YC", 1:5),
        "ak", test_date + 5, test_date + 1L, paste0("YD", 1L),
        "ak", test_date + 6, test_date + 1:5, paste0("YE", 1:5),
      ) %>%
        tidyr::unchop(c(version, y_value)) %>%
        dplyr::mutate(dplyr::across(c(y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  xy <- epix_merge(x, y)
  xy_expected <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        "ak", test_date + 1, test_date + 1:3, paste0("XA", 1:3), paste0("YA", 1:3),
        "ak", test_date + 2, test_date + 1:5, paste0("XB", 1:5), paste0("YB", c(NA, 2L, 2L, 2L, 2L)),
        "ak", test_date + 3, test_date + 1:5, paste0("XC", c(NA, 2L, 2L, 2L, 2L)), paste0("YC", 1:5),
        "ak", test_date + 4, test_date + 1L, paste0("XD", 1L), paste0("YD", NA),
        "ak", test_date + 5, test_date + 1L, paste0("XD", NA), paste0("YD", 1L),
        "ak", test_date + 6, test_date + 1:5, paste0("XE", c(1L, 1L, NA, NA, 5L)), paste0("YE", 1:5),
      ) %>%
        tidyr::unchop(c(version, x_value, y_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value, y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )

  expect_identical(xy, xy_expected)


  s1 <- tibble(
    geo_value = c("ca", "ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
    version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-02")),
    signal1 = c("XA", "XB", "XC")
  )

  s2 <- tibble(
    geo_value = c("ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-02")),
    version = as.Date(c("2024-08-03", "2024-08-03")),
    signal2 = c("YA", "YB")
  )

  s1 <- s1 %>% as_epi_archive()
  s2 <- s2 %>% as_epi_archive()

  merge1_expected <- tibble(
    geo_value = rep("ca", 5),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-01", "2024-08-02", "2024-08-02")),
    version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03", "2024-08-02", "2024-08-03")),
    signal1 = c("XA", "XB", "XB", "XC", "XC"),
    signal2 = c(NA, NA, "YA", NA, "YB")
  ) %>% as_epi_archive()

  merged1 <- epix_merge(s1, s2, sync = "locf")

  expect_identical(merged1, merge1_expected)

  s1 <- tibble(
    geo_value = c("ca", "ca", "ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-03")),
    version = as.Date(c("2024-08-01", "2024-08-03", "2024-08-03", "2024-08-03")),
    signal1 = c("XA", "XB", "XC", "XD")
  )

  s2 <- tibble(
    geo_value = c("ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-02")),
    version = as.Date(c("2024-08-02", "2024-08-02")),
    signal2 = c("YA", "YB"),
  )


  s1 <- s1 %>% as_epi_archive()
  s2 <- s2 %>% as_epi_archive()

  merge2_expected <- tibble(
    geo_value = rep("ca", 6),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-01", "2024-08-02", "2024-08-02", "2024-08-03")),
    version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03", "2024-08-02", "2024-08-03", "2024-08-03")),
    signal1 = c("XA", "XA", "XB", NA, "XC", "XD"),
    signal2 = c(NA, "YA", "YA", "YB", "YB", NA)
  ) %>% as_epi_archive()

  merged2 <- epix_merge(s1, s2, sync = "locf")

  expect_identical(merged2, merge2_expected)

  s1 <- tibble(
    geo_value = c("ca", "ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
    version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
    signal1 = c("XA", "XB", "XC")
  )

  s2 <- tibble(
    geo_value = c("ca", "ca", "ca"),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
    version = as.Date(c("2024-08-02", "2024-08-03", "2024-08-03")),
    signal2 = c("YA", "YB", "YC"),
  )

  s1 <- s1 %>% as_epi_archive()
  s2 <- s2 %>% as_epi_archive()

  merge3_expected <- tibble(
    geo_value = rep("ca", 6),
    time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-01", "2024-08-02", "2024-08-02", "2024-08-03")),
    version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03", "2024-08-02", "2024-08-03", "2024-08-03")),
    signal1 = c("XA", "XA", "XA", "XB", "XB", "XC"),
    signal2 = c(NA, "YA", "YB", NA, "YC", NA)
  ) %>% as_epi_archive()

  merged3 <- epix_merge(s1, s2, sync = "locf")

  expect_identical(merged3, merge3_expected)
})

test_that("epix_merge forbids and warns on metadata and naming issues", {
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value = "tx", time_value = test_date, version = test_date + 1L, x_value = 1L)),
      as_epi_archive(tibble::tibble(geo_value = "us", time_value = test_date, version = test_date + 5L, y_value = 2L))
    ),
    regexp = "must have the same.*geo_type"
  )
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value = "pa", time_value = test_date, version = test_date + 1L, x_value = 1L)),
      as_epi_archive(
        tibble::tibble(geo_value = "pa", time_value = 1L, version = 2L, y_value = 2L)
      )
    ),
    regexp = "must share data type on their `time_value` column."
  )
  expect_error(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, value = 1L)),
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, value = 2L))
    ),
    regexp = "overlapping.*names"
  )
  
  skip("Additional metadata is no longer used, not tested.")
  expect_warning(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, x_value = 1L),
        additional_metadata = list("updates_fetched" = lubridate::ymd_hms("2022-05-01 16:00:00", tz = "UTC"))
      ),
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, y_value = 2L))
    ),
    regexp = "x\\$additional_metadata",
    class = "epiprocess__epix_merge_ignores_additional_metadata"
  )
  expect_warning(
    epix_merge(
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, x_value = 1L)),
      as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, y_value = 2L),
        additional_metadata = list("updates_fetched" = lubridate::ymd_hms("2022-05-01 16:00:00", tz = "UTC"))
      )
    ),
    regexp = "y\\$additional_metadata",
    class = "epiprocess__epix_merge_ignores_additional_metadata"
  )
})

# use `local` to prevent accidentally using the x, y, xy bindings here
# elsewhere, while allowing reuse across a couple tests
local({
  x <- as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, x_value = 1L),
    clobberable_versions_start = test_date + 1L, versions_end = test_date + 10L
  )
  y <- as_epi_archive(tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, y_value = 2L),
    clobberable_versions_start = test_date + 3L, versions_end = test_date + 10L
  )
  xy <- epix_merge(x, y)
  test_that("epix_merge considers partially-clobberable row to be clobberable", {
    expect_identical(xy$clobberable_versions_start, test_date + 1L)
  })
  test_that("epix_merge result uses versions_end metadata not max version val", {
    expect_identical(xy$versions_end, test_date + 10L)
  })
})

local({
  x <- as_epi_archive(
    tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, x_value = 10L),
    clobberable_versions_start = test_date + 1L,
    versions_end = test_date + 3L
  )
  y <- as_epi_archive(
    tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 5L, y_value = 20L),
    clobberable_versions_start = test_date + 1L
  )
  test_that('epix_merge forbids on sync default or "forbid"', {
    expect_error(epix_merge(x, y),
      class = "epiprocess__epix_merge_unresolved_sync"
    )
    expect_error(epix_merge(x, y, sync = "forbid"),
      class = "epiprocess__epix_merge_unresolved_sync"
    )
  })
  test_that('epix_merge sync="na" works', {
    expect_equal(
      epix_merge(x, y, sync = "na"),
      as_epi_archive(tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        "ak", test_date, test_date + 1L, 10L, NA_integer_, # x updated, y not observed yet
        "ak", test_date, test_date + 4L, NA_integer_, NA_integer_, # NA-ing out x, y not observed yet
        "ak", test_date, test_date + 5L, NA_integer_, 20L, # x still NA, y updated
        # (we should not have a y vals -> NA update here; version 5 should be
        # the `versions_end` of the result)
      ), clobberable_versions_start = test_date + 1L)
    )
  })
  test_that('epix_merge sync="locf" works', {
    expect_equal(
      epix_merge(x, y, sync = "locf"),
      as_epi_archive(tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        "ak", test_date, test_date + 1L, 10L, NA_integer_, # x updated, y not observed yet
        "ak", test_date, test_date + 5L, 10L, 20L, # x LOCF'd, y updated
      ), clobberable_versions_start = test_date + 1L)
    )
  })
  test_that('epix_merge sync="truncate" works', {
    expect_equal(
      epix_merge(x, y, sync = "truncate"),
      as_epi_archive(tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        "ak", test_date, test_date + 1L, 10L, NA_integer_, # x updated, y not observed yet
        # y's update beyond x's last update has been truncated
      ), clobberable_versions_start = test_date + 1L, versions_end = test_date + 3L)
    )
  })
  x_no_conflict <- as_epi_archive(
    tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, x_value = 10L)
  )
  y_no_conflict <- as_epi_archive(
    tibble::tibble(geo_value = "ak", time_value = test_date, version = test_date + 1L, y_value = 20L)
  )
  xy_no_conflict_expected <- as_epi_archive(tibble::tribble(
    ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
    "ak", test_date, test_date + 1L, 10L, 20L, # x updated, y not observed yet
  ))
  test_that('epix_merge sync="forbid" on no-conflict works', {
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, sync = "forbid"),
      xy_no_conflict_expected
    )
  })
  test_that('epix_merge sync="na" on no-conflict works', {
    # This test is the main reason for these no-conflict tests. We want to make
    # sure that we don't add an unnecessary NA-ing-out version beyond a common
    # versions_end.
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, sync = "na"),
      xy_no_conflict_expected
    )
  })
  test_that('epix_merge sync="locf" on no-conflict works', {
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, sync = "locf"),
      xy_no_conflict_expected
    )
  })
  test_that('epix_merge sync="truncate" on no-conflict works', {
    expect_equal(
      epix_merge(x_no_conflict, y_no_conflict, sync = "truncate"),
      xy_no_conflict_expected
    )
  })
})
