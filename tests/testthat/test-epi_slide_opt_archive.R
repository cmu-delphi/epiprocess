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
    epi_slide_opt_archive_one_epikey("value", f, "data.table", 2L, 0L, "day", "slide_value")

  expect_equal(
    result %>% lapply(function(x) {
      x %>%
        arrange(time_value) %>%
        select(version, time_value, everything())
    })
   ,
    expected
  )

  # TODO check about version nesting ordering

})

# TODO tests on example data sets
