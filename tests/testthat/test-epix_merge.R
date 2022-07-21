
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
                      # values that should be LVCF'd + NAs that should be LVCF's as NA
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

# TODO test other behaviors
