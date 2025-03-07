library(dplyr)

test_that("epi_slide_opt_archive_one_epikey works as expected", {
  start_date <- as.Date("2020-01-01")

  updates <- bind_rows(
    tibble(version = 10, time_value = 0:20, value = 0:20),
    tibble(version = 12, time_value = 4:5, value = 5:4),
    tibble(version = 13, time_value = 8, value = 9),
    tibble(version = 14, time_value = 11, value = NA),
    tibble(version = 15, time_value = -10, value = -10),
    tibble(version = 16, time_value = 50, value = 50)
  ) %>%
    mutate(across(c(version, time_value), ~ start_date - 1 + .x)) %>%
    tidyr::nest(.by = version, .key = "subtbl")

  expected <- list(
    vctrs::vec_cbind(
      tibble(version = 10),
      updates$subtbl[[1L]] %>%
        mutate(time_value = as.numeric(time_value - start_date) + 1) %>%
        mutate(slide_value = frollmean(value, 3, algo = "exact"))
    ),
    tibble(
      version = 12,
      time_value = c(4, 5, 7), # time 6 unchanged, compactified away
      # time 7 `value` unchanged, but here because `slide_value` changed:
      value = c(5, 4, 7),
      slide_value = c(
        mean(c(2, 3, 5)),
        # time 5 `slide_value` unchanged, but here because `value` changed:
        mean(c(3, 5, 4)),
        mean(c(4, 6, 7))
      )
    ),
    tibble(
      version = 13, time_value = 8:10, value = c(9, 9, 10),
      slide_value = frollmean(c(6, 7, 9, 9, 10),  3,  algo = "exact")[-(1:2)]
    ),
    tibble(
      version = 14, time_value = 11:13, value = c(NA, 12, 13), slide_value = rep(NA_real_, 3L)
    ),
    tibble(
      version = 15, time_value = -10, value = -10, slide_value = NA_real_
    ),
    tibble(
      version = 16, time_value = 50, value = 50, slide_value = NA_real_
    )
  ) %>%
    lapply(function(x) {
      x %>%
        mutate(across(c(version, time_value), ~ start_date - 1 + .x))
    })

  f <- purrr::partial(data.table::frollmean, algo = "exact")

  result <- updates %>%
    epi_slide_opt_archive_one_epikey("value", f, "data.table", 2L, 0L, "day", "slide_value") %>%
    lapply(function(x) {
      x %>%
        arrange(time_value) %>%
        select(version, time_value, everything())
    })

  expect_equal(result, expected)
})


test_that("epi_slide_opt.epi_archive is not confused by unique(DT$version) unsorted", {
  start_date <- as.Date("2020-01-01")
  tibble(
    geo_value = 1,
    time_value = start_date - 1 + 1:4,
    version = start_date - 1 + c(5, 5, 4, 4),
    value = c(1, 2, 3, 4)
  ) %>%
    as_epi_archive() %>%
    epi_slide_opt(value, frollmean, .window_size = 2L) %>%
    expect_equal(
      tibble(
        geo_value = 1,
        time_value = start_date - 1 + c(1, 2, 3, 3, 4),
        version = start_date - 1 + c(5, 5, 4, 5, 4),
        value = c(1, 2, 3, 3, 4),
        value_2dav = c(NA, 1.5, NA, 2.5, 3.5)
      ) %>%
        as_epi_archive()
    )
})

test_that("epi_slide_opt.epi_archive is not confused by unique(DT$time_value) unsorted", {

  start_date <- as.Date("2020-01-01")
  tibble(
    geo_value = c(1, 1, 2, 2),
    time_value = start_date - 1 + c(2, 3, 1, 2),
    version = start_date - 1 + c(1, 2, 2, 2),
    value = c(1, 2, 3, 4)
  ) %>%
    as_epi_archive() %>%
    epi_slide_opt(value, frollmean, .window_size = 2L) %>%
    expect_equal(
      tibble(
        geo_value = c(1, 1, 2, 2),
        time_value = start_date - 1 + c(2, 3, 1, 2),
        version = start_date - 1 + c(1, 2, 2, 2),
        value = c(1, 2, 3, 4),
        value_2dav = c(NA, 1.5, NA, 3.5)
      ) %>%
        as_epi_archive()
    )

})

test_that("epi_slide_opt.epi_archive is equivalent to epix_slide reconversion on example data", {

  case_death_rate_archive %>%
    epi_slide_opt(case_rate, frollmean, .window_size = 7
                  # , algo = "exact"
                  ) %>%
    .$DT %>%
    as.data.frame() %>%
    as_tibble() %>%
    filter(!approx_equal(case_rate_7dav, case_rate_7d_av, 1e-6, TRUE)) %>%
    dplyr::transmute(version, geo_value, time_value, case_rate_7dav, case_rate_7d_av,
                     abs_diff = abs(case_rate_7dav - case_rate_7d_av)) %>%
    {}

    # TODO finish tests on example data sets

  })


# TODO grouped behavior checks
