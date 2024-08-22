library(cli)

test_date <- as.Date("2020-01-01")
days_dt <- as.difftime(1, units = "days")
weeks_dt <- as.difftime(1, units = "weeks")

n <- 30
# A tibble with two geos on the same time index and one geo with a different but
# overlapping time index
toy_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  "a", test_date + 1:n, 1:n,
  "b", test_date + 1:n, 10 * n + 1:n,
  "c", test_date + floor(n / 2) + 1:n, 100 * n + 1:n
) %>%
  tidyr::unnest_longer(c(time_value, value)) %>%
  as_epi_df(as_of = test_date + 100)
toy_edf_g <- toy_edf %>% group_by(geo_value)
overlap_index <- toy_edf %>%
  group_by(geo_value) %>%
  summarize(time_values = list(time_value)) %>%
  pull(time_values) %>%
  Reduce(intersect, .) %>%
  as.Date()

# Utility functions for computing expected slide_sum output
compute_slide_external <- function(before, overlap = FALSE) {
  if (overlap) {
    toy_edf <- toy_edf %>%
      filter(time_value %in% overlap_index)
    toy_edf_g <- toy_edf_g %>%
      filter(time_value %in% overlap_index)
  }
  slide_value <- toy_edf %>%
    group_by(time_value) %>%
    summarize(value = sum(value)) %>%
    pull(value) %>%
    slider::slide_sum(before = before)
  toy_edf_g %>%
    mutate(slide_value = slide_value) %>%
    ungroup()
}
compute_slide_external_g <- function(before) {
  toy_edf_g %>%
    mutate(slide_value = slider::slide_sum(value, before = before)) %>%
    dplyr::arrange(geo_value, time_value) %>%
    as_epi_df(as_of = test_date + 100)
}

f_tib_avg_count <- function(x, g, t) dplyr::tibble(avg = mean(x$value), count = length(x$value))


# Argument validation tests
bad_values <- list(
  "a", 0.5, -1L, -1.5, 1.5, NA, c(0, 1)
)
purrr::walk(bad_values, function(bad_value) {
  test_that(
    format_inline("`before` and `after` in epi_slide fail on {bad_value}"),
    {
      expect_error(
        epi_slide(toy_edf_g, before = bad_value, ref_time_values = test_date + 2),
        class = "epiprocess__validate_slide_window_arg"
      )
      expect_error(
        epi_slide(toy_edf_g, after = bad_value, ref_time_values = test_date + 2),
        class = "epiprocess__validate_slide_window_arg"
      )
    }
  )
})
purrr::walk(bad_values, function(bad_value) {
  test_that(format_inline("`before` and `after` in epi_slide_mean fail on {bad_value}"), {
    expect_error(
      epi_slide_mean(toy_edf_g, col_names = value, before = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
    expect_error(
      epi_slide_mean(toy_edf_g, col_names = value, after = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
  })
})

bad_values <- c(min(toy_edf_g$time_value) - 1, max(toy_edf_g$time_value) + 1)
purrr::walk(bad_values, function(bad_value) {
  test_that(format_inline("epi_slide[_mean]: `ref_time_values` out of range for all groups {bad_value}"), {
    expect_error(
      epi_slide(toy_edf_g, f_tib_avg_count, before = 2 * days_dt, ref_time_values = bad_value),
      class = "epi_slide__invalid_ref_time_values"
    )
    expect_error(
      epi_slide_mean(toy_edf_g, col_names = value, before = 2 * days_dt, ref_time_values = bad_value),
      class = "epi_slide_opt__invalid_ref_time_values"
    )
  })
})

test_that(
  "epi_slide or epi_slide_mean: `ref_time_values` in range for at least one group generate no error",
  {
    expect_equal(
      epi_slide(toy_edf_g, ~ sum(.x$value), before = 2 * days_dt, ref_time_values = test_date + 5) %>% ungroup(),
      compute_slide_external_g(before = 2) %>% ungroup() %>% filter(time_value == test_date + 5)
    )
    expect_equal(
      epi_slide_sum(toy_edf_g, value, before = 2 * days_dt, ref_time_values = test_date + 5, na.rm = TRUE) %>%
        ungroup() %>%
        rename(slide_value = slide_value_value),
      compute_slide_external_g(before = 2) %>% ungroup() %>% filter(time_value == test_date + 5)
    )
  }
)

test_that("epi_slide_mean errors when `as_list_col` non-NULL", {
  expect_error(
    toy_edf %>%
      filter(
        geo_value == "a"
      ) %>%
      epi_slide_mean(
        value,
        before = 6 * days_dt, as_list_col = TRUE, na.rm = TRUE
      ),
    class = "lifecycle_error_deprecated"
  )
})

test_that("epi_slide alerts if the provided f doesn't take enough args", {
  expect_no_error(
    epi_slide(toy_edf_g, f_tib_avg_count, before = days_dt, ref_time_values = test_date + 1),
  )
  expect_no_warning(
    epi_slide(toy_edf_g, f_tib_avg_count, before = days_dt, ref_time_values = test_date + 1),
  )

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$value), count = length(x$value))
  expect_warning(epi_slide(toy_edf_g, f_x_dots, before = days_dt, ref_time_values = test_date + 1),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
})


# Common example tests: epi_slide over grouped epi_dfs on common ref_time_values
# TODO: doesn't work on non-overlapping ref_time_values
for (all_rows in list(FALSE, TRUE)) {
  for (rtv in list(NULL, overlap_index[1:3])) {
    test_that(
      format_inline(
        "epi_slide works with formulas, lists, and data.frame outputs with ref_time_value={rtv}
        and all_rows={all_rows}"
      ),
      {
        simpler_slide_call <- function(f) {
          toy_edf_g %>%
            epi_slide(
              before = 6 * days_dt, f,
              ref_time_values = rtv, all_rows = all_rows
            )
        }
        filter_expected <- function(x) {
          if (all_rows && !is.null(rtv)) {
            dplyr::mutate(x, slide_value = dplyr::if_else(time_value %in% rtv, slide_value, NA))
          } else if (!is.null(rtv)) {
            dplyr::filter(x, time_value %in% rtv)
          } else {
            x
          }
        }

        expect_equal(
          simpler_slide_call(~ sum(.x$value)),
          compute_slide_external_g(before = 6) %>% filter_expected()
        )

        expect_equal(
          simpler_slide_call(~ list(rep(sum(.x$value), 2L))),
          compute_slide_external_g(before = 6) %>%
            mutate(slide_value = lapply(slide_value, rep, 2L)) %>%
            filter_expected()
        )

        expect_equal(
          simpler_slide_call(~ data.frame(slide_value = sum(.x$value))),
          compute_slide_external_g(before = 6) %>% filter_expected()
        )

        expect_equal(
          simpler_slide_call(~ list(data.frame(slide_value = sum(.x$value)))),
          compute_slide_external_g(before = 6) %>%
            mutate(slide_value = purrr::map(slide_value, ~ data.frame(slide_value = .x))) %>%
            filter_expected()
        )

        expect_identical(
          simpler_slide_call(~ tibble(slide_value = list(sum(.x$value)))),
          compute_slide_external_g(before = 6) %>% mutate(slide_value = as.list(slide_value)) %>% filter_expected()
        )

        # unnamed data-masking expression producing data frame:
        # unfortunately, we can't pass this directly as `f` and need an extra comma
        slide_unnamed_df <- toy_edf_g %>%
          epi_slide(
            before = 6L, , data.frame(slide_value = sum(.x$value)),
            ref_time_values = rtv, all_rows = all_rows
          )
        expect_identical(
          slide_unnamed_df,
          compute_slide_external_g(before = 6) %>% filter_expected()
        )
      }
    )
  }
}

# Common example tests: epi_slide_sum over grouped epi_dfs on common ref_time_values
# TODO: doesn't work on non-overlapping ref_time_values for most of these
for (all_rows in list(FALSE, TRUE)) {
  for (rtv in list(NULL, overlap_index)) {
    test_that(
      format_inline(
        "epi_slide_sum works with formulas, lists, and data.frame outputs with ref_time_value={rtv}
        and all_rows={all_rows}"
      ),
      {
        filter_expected <- function(x) {
          if (all_rows && !is.null(rtv)) {
            dplyr::mutate(x, slide_value = dplyr::if_else(time_value %in% rtv, slide_value, NA))
          } else if (!is.null(rtv)) {
            dplyr::filter(x, time_value %in% rtv)
          } else {
            x
          }
        }

        expect_equal(
          toy_edf_g %>%
            epi_slide_sum(
              value,
              before = 6 * days_dt,
              ref_time_values = rtv, all_rows = all_rows, na.rm = TRUE
            ) %>%
            rename(slide_value = slide_value_value),
          compute_slide_external_g(before = 6) %>% filter_expected()
        )
      }
    )
  }
}

possible_f <- list(~.ref_time_value, ~.z, ~..3, f = function(x, g, t) t)
purrr::walk(possible_f, function(f) {
  test_that("epi_slide computation can use ref_time_value", {
    # Grouped with multiple geos
    expect_equal(
      toy_edf_g %>% epi_slide(f = f, before = 50 * days_dt),
      toy_edf_g %>% mutate(slide_value = time_value)
    )

    # Ungrouped with multiple geos
    expect_equal(
      toy_edf %>% epi_slide(f = f, before = 50 * days_dt),
      toy_edf %>% mutate(slide_value = time_value) %>% arrange(time_value)
    )
  })
})

test_that("epi_slide computation via dots can use ref_time_value and group", {
  # Use ref_time_value
  expect_equal(
    toy_edf_g %>% epi_slide(before = 50 * days_dt, slide_value = .ref_time_value),
    toy_edf_g %>% mutate(slide_value = time_value)
  )

  # `.{x,group_key,ref_time_value}` should be inaccessible from `.data` and
  # `.env`.
  expect_error(toy_edf_g %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = .env$.ref_time_value
    ))

  # Grouped and use group key as value
  expect_equal(
    toy_edf_g %>% epi_slide(before = 2 * days_dt, slide_value = .group_key$geo_value),
    toy_edf_g %>% mutate(slide_value = geo_value)
  )

  # Use entire group_key object
  expect_equal(
    toy_edf_g %>% epi_slide(before = 2 * days_dt, slide_value = nrow(.group_key)),
    toy_edf_g %>% mutate(slide_value = 1L)
  )

  # Ungrouped with multiple geos
  expect_equal(
    toy_edf %>% epi_slide(before = 50 * days_dt, slide_value = .ref_time_value),
    toy_edf %>% mutate(slide_value = time_value) %>% arrange(time_value)
  )
})

test_that("epi_slide computation via dots outputs the same result using col names and the data var", {
  expected_output <- toy_edf %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = max(time_value)
    ) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- toy_edf %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = max(.x$time_value)
    )

  expect_equal(result1, expected_output)

  result2 <- toy_edf %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = max(.data$time_value)
    )

  expect_equal(result2, expected_output)
})

test_that("`epi_slide` can access objects inside of helper functions", {
  helper <- function(archive_haystack, time_value_needle) {
    archive_haystack %>% epi_slide(
      has_needle = time_value_needle %in% time_value, before = 365000L * days_dt
    )
  }
  expect_error(
    helper(toy_edf, as.Date("2021-01-01")),
    NA
  )
})

# TODO: Only works with overlapping ref_time_values
test_that("basic ungrouped epi_slide computation produces expected output", {
  # Single geo
  expect_equal(
    toy_edf %>% filter(geo_value == "a") %>% epi_slide(before = 50 * days_dt, slide_value = sum(.x$value)),
    compute_slide_external_g(before = 50) %>% ungroup() %>% filter(geo_value == "a") %>% arrange(time_value)
  )
  # Multiple geos
  expect_equal(
    toy_edf %>% filter(time_value %in% overlap_index) %>% epi_slide(before = 50 * days_dt, slide_value = sum(.x$value)),
    compute_slide_external(before = 50, overlap = TRUE) %>% arrange(time_value)
  )
})

test_that("basic ungrouped epi_slide_mean computation produces expected output", {
  # Single geo
  expect_equal(
    toy_edf %>%
      filter(geo_value == "a") %>%
      epi_slide_sum(value, before = 50 * days_dt, na.rm = TRUE) %>%
      rename(slide_value = slide_value_value),
    compute_slide_external_g(before = 50) %>% ungroup() %>% filter(geo_value == "a") %>% arrange(time_value)
  )

  # Multiple geos
  # epi_slide_sum fails when input data groups contain duplicate time_values,
  # e.g. aggregating across geos
  expect_error(
    toy_edf %>% epi_slide_sum(value, before = 6 * days_dt),
    class = "epiprocess__epi_slide_opt__duplicate_time_values"
  )
})


# Other example tests
test_that("epi_slide can use sequential data masking expressions including NULL", {
  edf_a <- tibble::tibble(
    geo_value = 1,
    time_value = 1:10,
    value = 1:10
  ) %>%
    as_epi_df(as_of = 12L)

  noisiness_a1 <- edf_a %>%
    group_by(geo_value) %>%
    epi_slide(
      before = 1L, after = 2L,
      valid = nrow(.x) == 4L, # not the best approach...
      m = mean(.x$value[1:2]),
      noisiness = sqrt(mean((value[3:4] - m)^2)),
      m = NULL
    ) %>%
    ungroup() %>%
    filter(valid) %>%
    select(-valid)

  noisiness_a0 <- edf_a %>%
    filter(
      time_value >= min(time_value) + 1L,
      time_value <= max(time_value) - 2L
    ) %>%
    mutate(noisiness = sqrt((3 - 1.5)^2 + (4 - 1.5)^2) / sqrt(2))

  expect_identical(noisiness_a1, noisiness_a0)

  edf_b <- tibble::tibble(
    geo_value = 1,
    time_value = 1:10,
    value = rep(1:2, 5L)
  ) %>%
    as_epi_df(as_of = 12L)

  noisiness_b1 <- edf_b %>%
    group_by(geo_value) %>%
    epi_slide(
      before = 1L, after = 2L,
      valid = nrow(.x) == 4L, # not the best approach...
      model = list(lm(value ~ time_value, .x[1:2, ])),
      pred = list(predict(model[[1L]], newdata = .x[3:4, "time_value"])),
      model = NULL,
      noisiness = sqrt(mean((.data$value[3:4] - .data$pred[[1L]])^2)),
      pred = NULL
    ) %>%
    ungroup() %>%
    filter(valid) %>%
    select(-valid)

  noisiness_b0 <- edf_b %>%
    filter(
      time_value >= min(time_value) + 1L,
      time_value <= max(time_value) - 2L
    ) %>%
    mutate(noisiness = sqrt((1 - 3)^2 + (2 - 4)^2) / sqrt(2))

  expect_equal(noisiness_b1, noisiness_b0)
})

test_that("epi_slide complains on invalid computation outputs", {
  expect_error(
    toy_edf %>% epi_slide(before = 6L, ~ lm(value ~ time_value, .x)),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_no_error(
    toy_edf %>% epi_slide(before = 6L, ~ list(lm(value ~ time_value, .x))),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_error(
    toy_edf %>% epi_slide(before = 6L, model = lm(value ~ time_value, .x)),
    class = "epiprocess__invalid_slide_comp_tidyeval_output"
  )
  expect_no_error(
    toy_edf %>% epi_slide(before = 6L, model = list(lm(value ~ time_value, .x))),
    class = "epiprocess__invalid_slide_comp_tidyeval_output"
  )
})

test_that("epi_slide can use {nm} :=", {
  nm <- "slide_value"
  expect_identical(
    # unfortunately, we can't pass this directly as `f` and need an extra comma
    toy_edf_g %>% epi_slide(before = 6L, , !!nm := sum(value)),
    compute_slide_external_g(before = 6)
  )
})

test_that("epi_slide can produce packed outputs", {
  packed_basic_result <- compute_slide_external_g(before = 6) %>%
    tidyr::pack(container = c(slide_value)) %>%
    dplyr_reconstruct(compute_slide_external_g(before = 6))
  expect_identical(
    toy_edf_g %>% epi_slide(before = 6L, ~ tibble::tibble(slide_value = sum(.x$value)), new_col_name = "container"),
    packed_basic_result
  )
  expect_identical(
    toy_edf_g %>% epi_slide(before = 6L, container = tibble::tibble(slide_value = sum(.x$value))),
    packed_basic_result
  )
  expect_identical(
    toy_edf_g %>% epi_slide(before = 6L, , tibble::tibble(slide_value = sum(.x$value)), new_col_name = "container"),
    packed_basic_result
  )
})

test_that("nested dataframe output names are controllable", {
  expect_equal(
    toy_edf_g %>% epi_slide(before = 6 * days_dt, ~ data.frame(result = sum(.x$value))),
    compute_slide_external_g(before = 6) %>% rename(result = slide_value)
  )
  expect_equal(
    toy_edf_g %>% epi_slide(before = 6 * days_dt, ~ data.frame(value_sum = sum(.x$value))),
    compute_slide_external_g(before = 6) %>% rename(value_sum = slide_value)
  )
})

# TODO: This seems really strange and counter-intuitive. Deprecate?4
test_that("non-size-1 f outputs are no-op recycled", {
  expect_equal(
    toy_edf %>% filter(time_value %in% overlap_index) %>% epi_slide(before = 6 * days_dt, ~ sum(.x$value) + c(0, 0, 0)),
    compute_slide_external(before = 6, overlap = TRUE) %>% arrange(time_value)
  )
  expect_equal(
    toy_edf %>%
      filter(time_value %in% overlap_index) %>%
      epi_slide(before = 6 * days_dt, ~ as.list(sum(.x$value) + c(0, 0, 0))),
    compute_slide_external(before = 6, overlap = TRUE) %>%
      dplyr::mutate(slide_value = as.list(slide_value)) %>%
      arrange(time_value)
  )
  expect_equal(
    toy_edf %>%
      filter(time_value %in% overlap_index) %>%
      epi_slide(before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value) + c(0, 0, 0))),
    compute_slide_external(before = 6, overlap = TRUE) %>% arrange(time_value)
  )
  # size-1 list is recycled:
  expect_equal(
    toy_edf %>%
      filter(time_value %in% overlap_index) %>%
      epi_slide(before = 6 * days_dt, ~ list(tibble(value = sum(.x$value) + c(0, 0, 0)))),
    compute_slide_external(before = 6, overlap = TRUE) %>%
      group_by(time_value) %>%
      mutate(slide_value = rep(list(tibble(value = slide_value)), 3L)) %>%
      ungroup() %>%
      arrange(time_value)
  )
})

test_that("`epi_slide` doesn't lose Date class output", {
  expect_true(
    toy_edf %>%
      epi_slide(before = 5 * days_dt, ~ as.Date("2020-01-01")) %>%
      `[[`("slide_value") %>%
      inherits("Date")
  )
})

time_types <- c("days", "weeks", "yearmonths", "integers")
for (time_type in time_types) {
  test_that(format_inline("epi_slide and epi_slide_mean: different before/after match for {time_type}"), {
    set.seed(0)
    n <- 16
    epi_data_no_missing <- rbind(
      tibble(geo_value = "al", a = 1:n, b = rnorm(n)),
      tibble(geo_value = "ca", a = n:1, b = rnorm(n) + 10),
      tibble(geo_value = "fl", a = n:1, b = rnorm(n) * 2)
    ) %>%
      mutate(
        time_value = rep(
          switch(time_type,
            days = as.Date("2022-01-01") + 1:n,
            weeks = as.Date("2022-01-01") + 7L * 1:n,
            yearmonths = tsibble::yearmonth(10L + 1:n),
            integers = 2000L + 1:n,
          ), 3
        )
      ) %>%
      as_epi_df() %>%
      group_by(geo_value)
    # Remove rows 12, 13, and 14 from every group
    epi_data_missing <- epi_data_no_missing %>% slice(1:11, 15:16)

    test_time_type_mean <- function(epi_data, before = NULL, after = NULL, ...) {
      result1 <- epi_slide(epi_data, ~ data.frame(
        slide_value_a = mean(.x$a, rm.na = TRUE),
        slide_value_b = mean(.x$b, rm.na = TRUE)
      ),
      before = before, after = after, ...
      )
      result2 <- epi_slide_mean(epi_data, col_names = c(a, b), na.rm = TRUE, before = before, after = after, ...)
      expect_equal(result1, result2)
    }

    units <- switch(time_type,
      days = days_dt,
      weeks = weeks_dt,
      yearmonths = 1,
      integers = 1
    )

    test_time_type_mean(epi_data_missing, before = 6 * units)
    test_time_type_mean(epi_data_missing, before = 6 * units, after = 1 * units)
    test_time_type_mean(epi_data_missing, before = 6 * units, after = 6 * units)
    test_time_type_mean(epi_data_missing, before = 1 * units, after = 6 * units)
    test_time_type_mean(epi_data_missing, after = 6 * units)
    test_time_type_mean(epi_data_missing, after = 1 * units)

    test_time_type_mean(epi_data_no_missing, before = 6 * units)
    test_time_type_mean(epi_data_no_missing, before = 6 * units, after = 1 * units)
    test_time_type_mean(epi_data_no_missing, before = 6 * units, after = 6 * units)
    test_time_type_mean(epi_data_no_missing, before = 1 * units, after = 6 * units)
    test_time_type_mean(epi_data_no_missing, after = 6 * units)
    test_time_type_mean(epi_data_no_missing, after = 1 * units)
  })
}

test_that("helper `full_date_seq` returns expected date values", {
  set.seed(0)
  n <- 7
  epi_data_missing <- rbind(
    tibble(geo_value = "al", a = 1:n, b = rnorm(n)),
    tibble(geo_value = "ca", a = n:1, b = rnorm(n) + 10),
    tibble(geo_value = "fl", a = n:1, b = rnorm(n) * 2)
  ) %>%
    mutate(
      days = rep(as.Date("2022-01-01") - 1 + 1:n, 3),
      weeks = rep(as.Date("2022-01-01") - 7 + 7L * 1:n, 3),
      yearmonths = rep(tsibble::yearmonth(10L - 1 + 1:n), 3),
      integers = rep(2000L - 1 + 1:n, 3)
    ) %>%
    slice(1:4, 6:7)

  before <- 2L
  after <- 1L

  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = days) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before * days_dt, after = after * days_dt, time_type = "day"
    ),
    list(
      all_dates = as.Date(c(
        "2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04",
        "2022-01-05", "2022-01-06", "2022-01-07"
      )),
      pad_early_dates = as.Date(c("2021-12-30", "2021-12-31")),
      pad_late_dates = as.Date(c("2022-01-08"))
    )
  )
  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = weeks) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before, after = after, time_type = "week"
    ),
    list(
      all_dates = as.Date(c(
        "2022-01-01", "2022-01-08", "2022-01-15", "2022-01-22",
        "2022-01-29", "2022-02-05", "2022-02-12"
      )),
      pad_early_dates = as.Date(c("2021-12-18", "2021-12-25")),
      pad_late_dates = as.Date(c("2022-02-19"))
    )
  )
  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = yearmonths) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before, after = after, time_type = "yearmonth"
    ),
    list(
      all_dates = tsibble::yearmonth(10:16),
      pad_early_dates = tsibble::yearmonth(8:9),
      pad_late_dates = tsibble::yearmonth(17)
    )
  )
  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = integers) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before, after = after, time_type = "integer"
    ),
    list(
      all_dates = as.double(2000:2006),
      pad_early_dates = as.double(1998:1999),
      pad_late_dates = 2007
    )
  )

  # Other before/after values
  before <- 5L
  after <- 0L

  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = days) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before * days_dt, after = after * days_dt, time_type = "day"
    ),
    list(
      all_dates = as.Date(c(
        "2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04",
        "2022-01-05", "2022-01-06", "2022-01-07"
      )),
      pad_early_dates = as.Date(c(
        "2021-12-27", "2021-12-28", "2021-12-29", "2021-12-30",
        "2021-12-31"
      )),
      pad_late_dates = NULL
    )
  )

  before <- 0L
  after <- 3L

  expect_identical(
    full_date_seq(
      epi_data_missing %>%
        mutate(time_value = days) %>%
        as_epi_df() %>%
        group_by(geo_value),
      before = before * days_dt, after = after * days_dt, time_type = "day"
    ),
    list(
      all_dates = as.Date(c(
        "2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04",
        "2022-01-05", "2022-01-06", "2022-01-07"
      )),
      pad_early_dates = NULL,
      pad_late_dates = as.Date(c(
        "2022-01-08", "2022-01-09", "2022-01-10"
      ))
    )
  )
})

test_that("epi_slide_mean/sum produces same output as epi_slide_opt grouped", {
  expect_equal(
    epi_slide_mean(toy_edf_g, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(toy_edf_g, value, f = data.table::frollmean, before = 50 * days_dt, na.rm = TRUE)
  )
  expect_equal(
    epi_slide_mean(toy_edf_g, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(toy_edf_g, value, f = slider::slide_mean, before = 50 * days_dt, na_rm = TRUE)
  )
  expect_equal(
    epi_slide_sum(toy_edf_g, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(toy_edf_g, value, f = data.table::frollsum, before = 50 * days_dt, na.rm = TRUE)
  )
  expect_equal(
    epi_slide_sum(toy_edf_g, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(toy_edf_g, value, f = slider::slide_sum, before = 50 * days_dt, na_rm = TRUE)
  )
})

test_that("`epi_slide_opt` errors when passed non-`data.table`, non-`slider` functions", {
  reexport_frollmean <- data.table::frollmean
  expect_no_error(
    epi_slide_opt(
      toy_edf_g,
      col_names = value, f = reexport_frollmean,
      before = days_dt,  ref_time_values = test_date + 1
    )
  )
  expect_error(
    epi_slide_opt(
      toy_edf_g,
      col_names = value, f = mean,
      before = days_dt,  ref_time_values = test_date + 1
    ),
    class = "epiprocess__epi_slide_opt__unsupported_slide_function"
  )
})

multi_columns <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = test_date + 1:200, value = 1:200, value2 = -1:-200),
  dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), value2 = 1:5)
) %>%
  as_epi_df() %>%
  group_by(geo_value)

test_that("no dplyr warnings from selecting multiple columns", {
  expect_no_warning(
    multi_slid <- epi_slide_mean(multi_columns, col_names = c("value", "value2"), before = 3L)
  )
  expect_equal(
    names(multi_slid),
    c("geo_value", "time_value", "value", "value2", "slide_value_value", "slide_value_value2")
  )
  expect_no_warning(
    multi_slid_select <- epi_slide_mean(multi_columns, c(value, value2), before = 3L)
  )
  expect_equal(multi_slid_select, multi_slid)
  expect_no_warning(
    multi_slid_select <- epi_slide_mean(multi_columns, starts_with("value"), before = 3L)
  )
  expect_equal(multi_slid_select, multi_slid)
})

test_that("Inf works in before/after in slide and slide_opt", {
  # Daily data
  df <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:200, value = 1:200),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5))
  ) %>%
    as_epi_df()
  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = Inf,
        slide_value = sum(value)
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000,
        slide_value = sum(value)
      )
  )
  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide_opt(
        before = Inf,
        f = data.table::frollsum,
        col_names = value
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000,
        slide_value_value = sum(value)
      )
  )
  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide_opt(
        before = Inf,
        f = slider::slide_sum,
        col_names = value
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000,
        slide_value_value = sum(value)
      )
  )

  # Weekly data
  df <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:200 * 7, value = 1:200),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5 * 7, value = -(1:5))
  ) %>%
    as_epi_df()

  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = Inf,
        slide_value = sum(value)
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000 * weeks_dt,
        slide_value = sum(value)
      )
  )
  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide_opt(
        col_names = value,
        f = data.table::frollsum,
        before = Inf
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000 * weeks_dt,
        slide_value_value = sum(value)
      )
  )
  expect_equal(
    df %>%
      group_by(geo_value) %>%
      epi_slide_opt(
        before = Inf,
        f = slider::slide_sum,
        col_names = value
      ),
    df %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 365000 * weeks_dt,
        slide_value_value = sum(value)
      )
  )
})
