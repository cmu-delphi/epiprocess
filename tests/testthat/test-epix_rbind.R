test_that("epix_rbind merges and carries forward updates properly", {
  x1 <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value,
        # X has initial versions defined
        "g1", 1L, 1:3, paste0("XA", 1:3),
      ) %>%
        tidyr::unchop(c(version, x_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  x2 <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value,
        # X has later versions defined as well
        "g1", 1L, 4:6, paste0("XA", 4:6),
        "g1", 2L, 4:6, paste0("XB", 4:6),
      ) %>%
        tidyr::unchop(c(version, x_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  y1 <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~y_value,
        # y also has earlier version defined
        "g1", 1L, 1:3, paste0("YA", 1:3),
      ) %>%
        tidyr::unchop(c(version, y_value)) %>%
        dplyr::mutate(dplyr::across(c(y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  y2 <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~y_value,
        # but y's data is "correct", and gets no more updates
        "g1", 2L, 4:6, paste0("YB", 4:6),
      ) %>%
        tidyr::unchop(c(version, y_value)) %>%
        dplyr::mutate(dplyr::across(c(y_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  first_merge <- epix_merge(x1, y1)
  second_merge <- epix_merge(x2, y2)
  # We rely on testthat edition 3 expect_identical using waldo, not identical. See
  # test-epix_fill_through_version.R comments for details.
  testthat::local_edition(3)
  # throw an error without a setting chosen when there's conflicts
  expect_error(epix_rbind(first_merge, second_merge), class = "epiprocess__epix_rbind_unresolved_sync")

  # the sync = "na" case
  canonical_no_overwrite <- as_epi_archive(
    data.table::as.data.table(
      tibble::tribble(
        ~geo_value, ~time_value, ~version, ~x_value, ~y_value,
        # X has initial versions defined
        "g1", 1L, 1:3, paste0("XA", 1:3), paste0("YA", 1:3),
        "g1", 1L, 4:6, paste0("XA", 4:6), NA,
        "g1", 2L, 4:6, paste0("XB", 4:6), paste0("YB", 4:6),
      ) %>%
        tidyr::unchop(c(version, x_value, y_value)) %>%
        dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
    )
  )
  # produce exactly the above result when NA's are unmodified
  rbinded_no_overwrite <- epix_rbind(first_merge, second_merge, sync = "na")
  expect_identical(rbinded_no_overwrite, canonical_no_overwrite)

  # filling forward is equivalent to doing columns rbinds first, and then merging
  x <- as_epi_archive(rbind(x1$DT, x2$DT))
  y <- as_epi_archive(rbind(y1$DT, y2$DT))
  canonical_locf <- epix_merge(x, y, sync = "locf")
  rbinded_locf <- epix_rbind(second_merge, first_merge, sync = "locf")
  expect_identical(canonical_locf$DT, rbinded_locf$DT)
  # y should be the same as epix_rbind(y1,y2), since each has distinct dates
  expect_identical(epix_rbind(y1,y2, sync = "forbid"), y)
  expect_identical(epix_rbind(y1,y2, sync = "na"), y)
  expect_identical(epix_rbind(y1,y2, sync = "locf"), y)
})
