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
      slide_value = frollmean(c(6, 7, 9, 9, 10), 3, algo = "exact")[-(1:2)]
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

test_that("epi_slide_opt.epi_archive gives expected results on example data; also grouped behavior", {
  # vs. built-in case_rate_7d_av column.
  #
  # If we were to compare the keyset vs.
  # the original, it changes, as the original contains some tiny deviations in
  # values that don't seem achievable with available sliding functions. E.g., in
  # the recomputed result, geo "ak" version "2020-11-01" changes time 2020-03-13
  # from 0 to 0.138 and time 2020-03-14 from a slightly different value of 0.138
  # to 0, while nearby times remained stable; in the original, this resulted in
  # a tiny update to the 7d_av for 2020-03-14 but not following times somehow,
  # while in the recomputation there are also minute updates to 2020-03-15 and
  # 2020-03-16; 2020-03-17 onward have other case_rate changes factoring in.
  # Compactifying and comparing with tolerances would help account for some of
  # these differences, but only through writing this was it realized that both
  # archives would need the recompactification with tolerance; it's not just
  # epi_slide_opt.epi_archive's very rigid compactification that's the cause.
  # (Side note: allowing configurable compactification tolerance in
  # epi_slide_opt.epi_archive wasn't included due to either feeling strange
  # applying the compactification tolerance to all columns rather than just
  # computed columns, and a slowdown when using one approach to compactify just
  # the new columns + also awkward not matching what's possible with just
  # construction functions.)
  #
  # --> just compare essentially an epix_merge of the original & the recomputation:
  case_death_rate_archive_time <- system.time(
    case_death_rate_archive_result <- case_death_rate_archive %>%
      epi_slide_opt(case_rate, frollmean, algo = "exact", .window_size = 7)
  )
  expect_equal(
    case_death_rate_archive_result$DT$case_rate_7dav,
    case_death_rate_archive_result$DT$case_rate_7d_av
  )

  # vs. computing via epix_slide:

  mini_case_death_rate_archive <- case_death_rate_archive %>%
    {
      as_tibble(as.data.frame(.$DT))
    } %>%
    filter(geo_value %in% head(unique(geo_value), 4L)) %>%
    as_epi_archive()

  mini_case_death_rate_archive_time_opt <- system.time(
    mini_case_death_rate_archive_result <- mini_case_death_rate_archive %>%
      epi_slide_opt(case_rate, frollmean, .window_size = 7)
  )

  mini_case_death_rate_archive_time_gen <- system.time(
    mini_case_death_rate_archive_expected <- mini_case_death_rate_archive %>%
      epix_slide(
        ~ .x %>% epi_slide_opt(case_rate, frollmean, .window_size = 7)
      ) %>%
      select(names(mini_case_death_rate_archive$DT), everything()) %>%
      as_epi_archive()
  )

  expect_equal(mini_case_death_rate_archive_result, mini_case_death_rate_archive_expected)

  mini_case_death_rate_archive_result2 <- mini_case_death_rate_archive %>%
    group_by(geo_value) %>%
    epi_slide_opt(case_rate, frollmean, .window_size = 7)

  expect_equal(
    mini_case_death_rate_archive_result2,
    mini_case_death_rate_archive_result %>%
      group_by(geo_value)
  )

  mini_case_death_rate_archive_b <- mini_case_death_rate_archive %>%
    {
      as_tibble(as.data.frame(.$DT))
    } %>%
    mutate(age_group = "overall") %>%
    as_epi_archive(other_keys = "age_group")

  expect_equal(
    mini_case_death_rate_archive_b %>%
      group_by(geo_value, age_group) %>%
      epi_slide_opt(case_rate, frollmean, .window_size = 7),
    mini_case_death_rate_archive_b %>%
      epi_slide_opt(case_rate, frollmean, .window_size = 7) %>%
      group_by(geo_value, age_group)
  )

  expect_error(
    mini_case_death_rate_archive_b %>%
      group_by(age_group) %>%
      epi_slide_opt(case_rate, frollmean, .window_size = 7)
  )

  archive_cases_dv_subset_time_opt <- system.time(
    archive_cases_dv_subset_result <- archive_cases_dv_subset %>%
      epi_slide_opt(percent_cli, frollmean, .window_size = 7)
  )

  archive_cases_dv_subset_time_gen <- system.time(
    archive_cases_dv_subset_expected <- archive_cases_dv_subset %>%
      epix_slide(
        ~ .x %>% epi_slide_opt(percent_cli, frollmean, .window_size = 7)
      ) %>%
      select(geo_value, time_value, version, everything()) %>%
      as_epi_archive()
  )

  expect_equal(archive_cases_dv_subset_result, archive_cases_dv_subset_expected)
})
