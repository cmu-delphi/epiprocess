# Create an epi_df and a function to test epi_slide with
test_date <- as.Date("2020-01-01")
days_dt <- as.difftime(1, units = "days")
weeks_dt <- as.difftime(1, units = "weeks")

ungrouped <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = test_date + 1:200, value = 1:200),
  dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5))
) %>%
  as_epi_df()
grouped <- ungrouped %>%
  group_by(geo_value)

small_x <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15),
  dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5))
) %>%
  as_epi_df(as_of = test_date + 6) %>%
  group_by(geo_value)

f <- function(x, g, t) dplyr::tibble(avg = mean(x$value), count = length(x$value))

toy_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  "a", test_date + 1:10, 2L^(1:10),
  "b", test_date + 1:10, 2L^(11:20),
) %>%
  tidyr::unchop(c(time_value, value)) %>%
  as_epi_df(as_of = test_date + 100)

# nolint start: line_length_linter.
basic_sum_result <- tibble::tribble(
  ~geo_value, ~time_value, ~value, ~slide_value,
  "a", test_date + 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
  "b", test_date + 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
) %>%
  tidyr::unchop(c(time_value, value, slide_value)) %>%
  dplyr::arrange(time_value) %>%
  as_epi_df(as_of = test_date + 100)

basic_mean_result <- tibble::tribble(
  ~geo_value, ~time_value, ~value, ~slide_value,
  "a", test_date + 1:10, 2L^(1:10), data.table::frollmean(2L^(1:10), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
) %>%
  tidyr::unchop(c(time_value, value, slide_value)) %>%
  dplyr::arrange(time_value) %>%
  as_epi_df(as_of = test_date + 100)
# nolint end: line_length_linter.


# Argument validation tests
bad_values <- list(
  "a", 0.5, -1L, -1.5, 1.5, NA, c(0, 1)
)
purrr::map(bad_values, function(bad_value) {
  test_that("`before` and `after` in epi_slide fail on {x}", {
    expect_error(
      epi_slide(grouped, before = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
    expect_error(
      epi_slide(grouped, after = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
  })
})
purrr::map(bad_values, function(bad_value) {
  test_that("`before` and `after` in epi_slide_mean fail on {x}", {
    expect_error(
      epi_slide_mean(grouped, col_names = value, before = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
    expect_error(
      epi_slide_mean(grouped, col_names = value, after = bad_value, ref_time_values = test_date + 2),
      class = "epiprocess__validate_slide_window_arg"
    )
  })
})

bad_values <- c(min(grouped$time_value) - 1, max(grouped$time_value) + 1)
purrr::map(bad_values, function(bad_value) {
  test_that("epi_slide or epi_slide_mean: `ref_time_values` out of range for all groups generate an error", {
    expect_error(
      epi_slide(grouped, f, before = 2 * days_dt, ref_time_values = bad_value),
      class = "epi_slide__invalid_ref_time_values"
    )
    expect_error(
      epi_slide_mean(grouped, col_names = value, before = 2 * days_dt, ref_time_values = bad_value),
      class = "epi_slide_opt__invalid_ref_time_values"
    )
  })
})

test_that(
  "epi_slide or epi_slide_mean: `ref_time_values` in range for at least group generate no error",
  {
    expect_equal(
      epi_slide(grouped, f, before = 2 * days_dt, ref_time_values = test_date + 200L) %>%
        ungroup() %>%
        dplyr::select("geo_value", "avg"),
      dplyr::tibble(geo_value = "ak", avg = 199)
    )
    expect_equal(
      epi_slide_mean(
        grouped, value,
        before = 2 * days_dt, ref_time_values = test_date + 200L, na.rm = TRUE
      ) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = "ak", slide_value_value = 199)
    )
  }
)


# Computation tests
test_that("epi_slide outputs list columns when desired, and unpacks unnamed computations", {
  # See `toy_edf` and `basic_sum_result` definitions at top of file.
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ sum(.x$value)),
    basic_sum_result
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ list(sum(.x$value))),
    basic_sum_result %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ list(rep(sum(.x$value), 2L))),
    basic_sum_result %>% dplyr::mutate(slide_value = lapply(slide_value, rep, 2L))
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value))),
    basic_sum_result
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value)))),
    basic_sum_result %>%
      mutate(slide_value = purrr::map(slide_value, ~ data.frame(slide_value = .x)))
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ tibble(slide_value = list(sum(.x$value)))),
    basic_sum_result %>% mutate(across(slide_value, as.list))
  )
  # unnamed data-masking expression producing data frame:
  expect_identical(
    # unfortunately, we can't pass this directly as `f` and need an extra comma
    toy_edf %>% epi_slide(before = 6L, , data.frame(slide_value = sum(.x$value))),
    basic_sum_result
  )
})

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
    toy_edf %>% epi_slide(before = 6L, , !!nm := sum(value)),
    basic_sum_result
  )
})

test_that("epi_slide and epi_slide_opt outputs match", {
  expect_equal(
    epi_slide(grouped, f, before = days_dt, after = days_dt, ref_time_values = test_date + 2) %>% select(-count),
    epi_slide_mean(
      grouped,
      col_names = value, before = days_dt, after = days_dt,
      ref_time_values = test_date + 2, na.rm = TRUE
    ) %>% rename(avg = slide_value_value)
  )
})

test_that("epi_slide can produce packed outputs", {
  packed_basic_result <- basic_sum_result %>%
    tidyr::pack(container = c(slide_value)) %>%
    dplyr_reconstruct(basic_sum_result)
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ tibble::tibble(slide_value = sum(.x$value)), new_col_name = "container"),
    packed_basic_result
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, container = tibble::tibble(slide_value = sum(.x$value))),
    packed_basic_result
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, , tibble::tibble(slide_value = sum(.x$value)), new_col_name = "container"),
    packed_basic_result
  )
})

test_that("epi_slide_mean errors when `as_list_col` non-NULL", {
  # See `toy_edf` and `basic_mean_result` definitions at top of file.
  # We'll try 7d avg with a few formats.
  # Warning: not exactly the same naming behavior as `epi_slide`.
  expect_equal(
    toy_edf %>%
      filter(
        geo_value == "a"
      ) %>%
      epi_slide_mean(
        value,
        before = 6 * days_dt, na.rm = TRUE
      ),
    basic_mean_result %>% rename(slide_value_value = slide_value)
  )
  # `epi_slide_mean` doesn't return dataframe columns
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

test_that("nested dataframe output names are controllable", {
  expect_equal(
    toy_edf %>%
      epi_slide(
        before = 6 * days_dt, ~ data.frame(result = sum(.x$value))
      ),
    basic_sum_result %>% rename(result = slide_value)
  )
  expect_equal(
    toy_edf %>%
      epi_slide(
        before = 6 * days_dt, ~ data.frame(value_sum = sum(.x$value))
      ),
    basic_sum_result %>% rename(value_sum = slide_value)
  )
})

test_that("outputs are recycled", {
  # trying with non-size-1 computation outputs:
  # nolint start: line_length_linter.
  basic_result_from_size2 <- tibble::tribble(
    ~geo_value, ~time_value, ~value, ~slide_value,
    "a", test_date + 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
    "b", test_date + 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE) + 1L,
  ) %>%
    tidyr::unchop(c(time_value, value, slide_value)) %>%
    dplyr::arrange(time_value) %>%
    as_epi_df(as_of = test_date + 100)
  # nolint end
  #
  # non-size-1 outputs with appropriate size are no-op "recycled":
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ sum(.x$value) + 0:1),
    basic_result_from_size2
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ as.list(sum(.x$value) + 0:1)),
    basic_result_from_size2 %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value) + 0:1)),
    basic_result_from_size2
  )
  # size-1 list is recycled:
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ list(tibble(value = sum(.x$value) + 0:1))),
    basic_result_from_size2 %>%
      group_by(time_value) %>%
      mutate(slide_value = rep(list(tibble(value = slide_value)), 2L)) %>%
      ungroup()
  )
})

test_that("epi_slide alerts if the provided f doesn't take enough args", {
  f_xgt <- function(x, g, t) dplyr::tibble(value = mean(x$value), count = length(x$value))
  expect_no_error(
    epi_slide(grouped, f_xgt, before = days_dt, ref_time_values = test_date + 1),
  )
  expect_no_warning(
    epi_slide(grouped, f_xgt, before = days_dt, ref_time_values = test_date + 1),
  )

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$value), count = length(x$value))
  expect_warning(epi_slide(grouped, f_x_dots, before = days_dt, ref_time_values = test_date + 1),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
})

test_that("`ref_time_values` + `all_rows = TRUE` works", {
  # See `toy_edf` definition at top of file. We'll do variants of a slide
  # returning the following:
  # nolint start: line_length_linter.
  basic_full_result <- tibble::tribble(
    ~geo_value, ~time_value, ~value, ~slide_value,
    "a", test_date + 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
    "b", test_date + 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
  ) %>%
    tidyr::unchop(c(time_value, value, slide_value)) %>%
    dplyr::arrange(time_value) %>%
    as_epi_df(as_of = test_date + 100)
  # nolint end
  # slide computations returning atomic vecs:
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ sum(.x$value)),
    basic_full_result
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ sum(.x$value),
      ref_time_values = test_date + c(2L, 8L)
    ),
    basic_full_result %>% dplyr::filter(time_value %in% (test_date + c(2L, 8L)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ sum(.x$value),
      ref_time_values = test_date + c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% (test_date + c(2L, 8L)),
        slide_value, NA_integer_
      ))
  )

  expect_equal(
    toy_edf %>% filter(
      geo_value == "a"
    ) %>%
      epi_slide_mean(
        value,
        before = 6 * days_dt, na.rm = TRUE
      ),
    basic_mean_result %>%
      rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% filter(
      geo_value == "a"
    ) %>%
      epi_slide_mean(
        value,
        before = 6 * days_dt, ref_time_values = test_date + c(2L, 8L),
        na.rm = TRUE
      ),
    filter(basic_mean_result, time_value %in% (test_date + c(2L, 8L))) %>%
      rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% filter(
      geo_value == "a"
    ) %>%
      epi_slide_mean(
        value,
        before = 6 * days_dt, ref_time_values = test_date + c(2L, 8L), all_rows = TRUE,
        na.rm = TRUE
      ),
    basic_mean_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% (test_date + c(2L, 8L)),
        slide_value, NA_integer_
      )) %>%
      rename(slide_value_value = slide_value)
  )

  # slide computations returning data frames:
  expect_equal(
    toy_edf %>% epi_slide(before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value))),
    basic_full_result
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value)),
      ref_time_values = test_date + c(2L, 8L)
    ),
    basic_full_result %>%
      dplyr::filter(time_value %in% (test_date + c(2L, 8L)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ data.frame(slide_value = sum(.x$value)),
      ref_time_values = test_date + c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% (test_date + c(2L, 8L)),
        slide_value, NA_integer_
      ))
  )
  # slide computations returning data frames with `as_list_col=TRUE`:
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value)))
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(slide_value = .x)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value))),
      ref_time_values = test_date + c(2L, 8L)
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(slide_value = .x))) %>%
      dplyr::filter(time_value %in% (test_date + c(2L, 8L)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value))),
      ref_time_values = test_date + c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(slide_value = .x))) %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% (test_date + c(2L, 8L)),
        slide_value, list(NULL)
      ))
  )
  # slide computations returning data frames, `as_list_col = TRUE`, `unnest`:
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value)))
    ) %>%
      unnest(slide_value),
    basic_full_result
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value))),
      ref_time_values = test_date + c(2L, 8L)
    ) %>%
      unnest(slide_value),
    basic_full_result %>%
      dplyr::filter(time_value %in% (test_date + c(2L, 8L)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value))),
      ref_time_values = test_date + c(2L, 8L), all_rows = TRUE
    ) %>%
      unnest(slide_value),
    basic_full_result %>%
      # XXX unclear exactly what we want in this case. Current approach is
      # compatible with `vctrs::vec_detect_missing` but breaks `tidyr::unnest`
      # compatibility since the non-ref rows are dropped
      dplyr::filter(time_value %in% (test_date + c(2L, 8L)))
  )
  rework_nulls <- function(slide_values_list) {
    vctrs::vec_assign(
      slide_values_list,
      vctrs::vec_detect_missing(slide_values_list),
      list(vctrs::vec_cast(NA, vctrs::vec_ptype_common(!!!slide_values_list)))
    )
  }
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6 * days_dt, ~ list(data.frame(slide_value = sum(.x$value))),
      ref_time_values = test_date + c(2L, 8L), all_rows = TRUE
    ) %>%
      mutate(slide_value = rework_nulls(slide_value)) %>%
      unnest(slide_value),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% (test_date + c(2L, 8L)),
        slide_value, NA_integer_
      ))
  )
})

test_that("`epi_slide` doesn't decay date output", {
  expect_true(
    ungrouped %>%
      epi_slide(before = 5 * days_dt, ~ as.Date("2020-01-01")) %>%
      `[[`("slide_value") %>%
      inherits("Date")
  )
})

test_that("basic grouped epi_slide computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15)),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = cumsum(-(1:5)))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  # formula
  result1 <- epi_slide(small_x, f = ~ sum(.x$value), before = 50 * days_dt)
  expect_equal(result1, expected_output)

  # function
  result2 <- epi_slide(small_x, f = function(x, g, t) sum(x$value), before = 50 * days_dt)
  expect_equal(result2, expected_output)

  # dots
  result3 <- epi_slide(small_x, slide_value = sum(value), before = 50 * days_dt)
  expect_equal(result3, expected_output)
})

test_that("basic grouped epi_slide_mean computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = cumsum(-(1:5)) / 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- epi_slide_mean(small_x, value, before = 50 * days_dt, na.rm = TRUE)
  expect_equal(result1, expected_output %>% rename(slide_value_value = slide_value))
})

test_that("ungrouped epi_slide computation completes successfully", {
  expect_no_error(
    small_x %>%
      ungroup() %>%
      epi_slide(
        before = 2 * days_dt,
        slide_value = sum(.x$value)
      )
  )
})

test_that("basic ungrouped epi_slide computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15))
  ) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    ungroup() %>%
    filter(geo_value == "ak") %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = sum(.x$value)
    )
  expect_equal(result1, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(
      geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15) + cumsum(-(1:5))
    ),
    dplyr::tibble(
      geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = cumsum(11:15) + cumsum(-(1:5))
    )
  ) %>%
    as_epi_df(as_of = test_date + 6) %>%
    arrange(time_value)

  result2 <- small_x %>%
    ungroup() %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = sum(.x$value)
    )
  expect_equal(result2, expected_output)
})

test_that("basic ungrouped epi_slide_mean computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
  ) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    ungroup() %>%
    filter(geo_value == "ak") %>%
    epi_slide_mean(value, before = 50 * days_dt, na.rm = TRUE)
  expect_equal(result1, expected_output %>% rename(slide_value_value = slide_value))

  # Ungrouped with multiple geos
  # epi_slide_mean fails when input data groups contain duplicate time_values,
  # e.g. aggregating across geos
  expect_error(
    small_x %>% ungroup() %>% epi_slide_mean(value, before = 6 * days_dt),
    class = "epiprocess__epi_slide_opt__duplicate_time_values"
  )
})

test_that("epi_slide computation via formula can use ref_time_value", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = test_date + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    epi_slide(
      f = ~.ref_time_value,
      before = 50 * days_dt
    )

  expect_equal(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(
      f = ~.z,
      before = 50 * days_dt
    )

  expect_equal(result2, expected_output)

  result3 <- small_x %>%
    epi_slide(
      f = ~..3,
      before = 50 * days_dt
    )

  expect_equal(result3, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = test_date + 1:5)
  ) %>%
    as_epi_df(as_of = test_date + 6) %>%
    arrange(time_value)

  result4 <- small_x %>%
    ungroup() %>%
    epi_slide(
      f = ~.ref_time_value,
      before = 50 * days_dt
    )
  expect_equal(result4, expected_output)
})

test_that("epi_slide computation via function can use ref_time_value", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = test_date + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    epi_slide(
      f = function(x, g, t) t,
      before = 2 * days_dt
    )

  expect_equal(result1, expected_output)
})

test_that("epi_slide computation via dots can use ref_time_value and group", {
  # ref_time_value
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = test_date + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = .ref_time_value
    )

  expect_equal(result1, expected_output)

  # `.{x,group_key,ref_time_value}` should be inaccessible from `.data` and
  # `.env`.
  expect_error(small_x %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = .env$.ref_time_value
    ))

  # group_key
  # Use group_key column
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = "ak"),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = "al")
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result3 <- small_x %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = .group_key$geo_value
    )

  expect_equal(result3, expected_output)

  # Use entire group_key object
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = 1L),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = 1L)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result4 <- small_x %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = nrow(.group_key)
    )

  expect_equal(result4, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 1:5, value = -(1:5), slide_value = test_date + 1:5)
  ) %>%
    as_epi_df(as_of = test_date + 6) %>%
    arrange(time_value)

  result5 <- small_x %>%
    ungroup() %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = .ref_time_value
    )
  expect_equal(result5, expected_output)
})

test_that("epi_slide computation via dots outputs the same result using col names and the data var", {
  expected_output <- small_x %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = max(time_value)
    ) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x %>%
    epi_slide(
      before = 2 * days_dt,
      slide_value = max(.x$time_value)
    )

  expect_equal(result1, expected_output)

  result2 <- small_x %>%
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
    helper(small_x, as.Date("2021-01-01")),
    NA
  )
})

test_that("basic slide behavior is correct when groups have non-overlapping date ranges", {
  small_x_misaligned_dates <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15),
    dplyr::tibble(geo_value = "al", time_value = test_date + 151:155, value = -(1:5))
  ) %>%
    as_epi_df(as_of = test_date + 6) %>%
    group_by(geo_value)

  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
    dplyr::tibble(
      geo_value = "al", time_value = test_date + 151:155, value = -(1:5), slide_value = cumsum(-(1:5)) / 1:5
    )
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- epi_slide(small_x_misaligned_dates, f = ~ mean(.x$value), before = 50 * days_dt)
  expect_equal(result1, expected_output)

  result2 <- epi_slide_mean(small_x_misaligned_dates, value, before = 50 * days_dt, na.rm = TRUE)
  expect_equal(result2, expected_output %>% rename(slide_value_value = slide_value))
})


test_that("epi_slide gets correct ref_time_value when groups have non-overlapping date ranges", {
  small_x_misaligned_dates <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15),
    dplyr::tibble(geo_value = "al", time_value = test_date + 151:155, value = -(1:5))
  ) %>%
    as_epi_df(as_of = test_date + 6) %>%
    group_by(geo_value)

  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = test_date + 1:5, value = 11:15, slide_value = test_date + 1:5),
    dplyr::tibble(geo_value = "al", time_value = test_date + 151:155, value = -(1:5), slide_value = test_date + 151:155)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = test_date + 6)

  result1 <- small_x_misaligned_dates %>%
    epi_slide(
      before = 50 * days_dt,
      slide_value = .ref_time_value
    )

  expect_equal(result1, expected_output)
})

time_types <- c("days", "weeks", "yearmonths", "integers")
for (time_type in time_types) {
  test_that("epi_slide and epi_slide_mean: different before/after match for {time_type}", {
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
      epi_data_missing %>% mutate(time_value = days) %>%
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
      epi_data_missing %>% mutate(time_value = weeks) %>%
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
      epi_data_missing %>% mutate(time_value = yearmonths) %>%
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
      epi_data_missing %>% mutate(time_value = integers) %>%
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
      epi_data_missing %>% mutate(time_value = days) %>%
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
      epi_data_missing %>% mutate(time_value = days) %>%
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

test_that("epi_slide_mean/sum produces same output as epi_slide_opt", {
  expect_equal(
    epi_slide_mean(small_x, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(small_x, value, f = data.table::frollmean, before = 50 * days_dt, na.rm = TRUE)
  )
  expect_equal(
    epi_slide_mean(small_x, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(small_x, value, f = slider::slide_mean, before = 50 * days_dt, na_rm = TRUE)
  )
  expect_equal(
    epi_slide_sum(small_x, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(small_x, value, f = data.table::frollsum, before = 50 * days_dt, na.rm = TRUE)
  )
  expect_equal(
    epi_slide_sum(small_x, value, before = 50 * days_dt, na.rm = TRUE),
    epi_slide_opt(small_x, value, f = slider::slide_sum, before = 50 * days_dt, na_rm = TRUE)
  )
})

test_that("`epi_slide_opt` errors when passed non-`data.table`, non-`slider` functions", {
  reexport_frollmean <- data.table::frollmean
  expect_no_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = reexport_frollmean,
      before = days_dt,  ref_time_values = test_date + 1
    )
  )
  expect_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = mean,
      before = days_dt,  ref_time_values = test_date + 1
    ),
    class = "epiprocess__epi_slide_opt__unsupported_slide_function"
  )
})
