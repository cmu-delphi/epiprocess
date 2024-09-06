library(cli)
library(dplyr)
library(purrr)

num_rows_per_group <- 20
get_test_date <- function(time_type = "day") {
  switch(time_type,
    day = as.Date("2020-01-01"),
    week = as.Date("2020-01-01"),
    yearmonth = tsibble::make_yearmonth(year = 2022, month = 1),
    integer = 2022L
  )
}
get_test_units <- function(time_type = "day") {
  switch(time_type,
    day = as.difftime(1, units = "days"),
    week = as.difftime(1, units = "weeks"),
    yearmonth = 1L,
    integer = 1L
  )
}
get_test_dataset <- function(n, time_type = "day", other_keys = character()) {
  checkmate::assert_integerish(n, lower = 1)
  checkmate::assert_character(time_type)
  checkmate::assert_character(other_keys)
  checkmate::assert_subset(other_keys, "x")
  # Do this to actually get n rows per group.
  n_ <- n - 1

  test_date <- get_test_date(time_type)
  units <- get_test_units(time_type)
  # A tibble with two geos on the same time index and one geo with a different
  # but overlapping time index. Each geo has a missing value somewhere in the middle.
  tibble::tribble(
    ~geo_value, ~time_value, ~value, ~x,
    "a", test_date + units * 0:n_, (0:n_)**2, rep(c(1, 2), length.out = n),
    "b", test_date + units * 0:n_, (10 * n + 0:n_)**2, rep(c(1, 2), length.out = n),
    "c", test_date + units * (floor(n / 2) + 0:n_), (100 * n + 0:n_)**2, rep(c(1, 2), length.out = n)
  ) %>%
    tidyr::unnest_longer(c(time_value, value, x)) %>%
    slice(-10) %>%
    as_epi_df(as_of = test_date + n, other_keys = other_keys) %>%
    group_by(geo_value)
}
test_data <- get_test_dataset(num_rows_per_group, "day")

# TODO: Add a test that uses an 'other_key' grouping column.
# TODO: Add a case where the data contains NA values (not just gaps in time_value).

epi_slide_sum_test <- function(
    .x,
    .window_size = 1, .align = "right", .ref_time_values = NULL, .all_rows = FALSE) {
  time_type <- attr(.x, "metadata")$time_type
  window_args <- get_before_after_from_window(.window_size, .align, time_type)
  date_seq_list <- full_date_seq(.x, window_args$before, window_args$after, time_type)
  if (is.null(.ref_time_values)) {
    .ref_time_values <- date_seq_list$all_dates
  }

  .x %>%
    mutate(.real = TRUE) %>%
    group_by(geo_value) %>%
    complete(time_value = vctrs::vec_c(!!!date_seq_list, .name_spec = rlang::zap())) %>%
    arrange(geo_value, time_value) %>%
    mutate(
      slide_value = slider::slide_index_sum(
        .data$value,
        .data$time_value,
        before = window_args$before,
        after = window_args$after
      )
    ) %>%
    # If .all_rows = TRUE, we need to keep all rows and NA out the ones not in
    # the ref_time_values. Otherwise, we need to return only the rows in
    # ref_time_values.
    group_modify(~ {
      available_ref_time_values <- .ref_time_values[.ref_time_values %in% .$time_value]

      if (.all_rows) {
        dplyr::mutate(., slide_value = dplyr::if_else(time_value %in% available_ref_time_values, slide_value, NA))
      } else {
        dplyr::filter(., time_value %in% available_ref_time_values)
      }
    }) %>%
    filter(.real) %>%
    select(-.real)
}
concatenate_list_params <- function(p) {
  paste(paste0(names(p), "=", p), collapse = "\n")
}
is_null_or_na <- function(x) {
  is.null(x) ||
    (is.na(x) && (is.logical(x) || is.double(x))) ||
    identical(x, list(NULL)) ||
    identical(x, list(NA))
}
test_that("is_null_or_na works", {
  x1 <- NULL
  x2 <- NA
  x3 <- NA_real_
  x4 <- 1
  x5 <- "NA"
  x6 <- list(NULL)
  x7 <- list(NA)

  expect_true(is_null_or_na(x1))
  expect_true(is_null_or_na(x2))
  expect_true(is_null_or_na(x3))
  expect_false(is_null_or_na(x4))
  expect_false(is_null_or_na(x5))
  expect_true(is_null_or_na(x6))
  expect_true(is_null_or_na(x7))
})
expect_equal_handle_null <- function(x, y) {
  x_na_mask <- purrr::map_lgl(x, is_null_or_na)
  y_na_mask <- purrr::map_lgl(y, is_null_or_na)
  expect_equal(x_na_mask, y_na_mask)
  expect_equal(x[!x_na_mask], y[!y_na_mask])
}


# Core functionality tests across an exhaustive combination of parameters on
# non-trivial data sets with three geo_groups, with non-identical time indices,
# with missing time values, and with reported NA values.
# .ref_time_values can be:
# - NULL is a special case where we just use all the unique time_values in the
#   data.
# - c(1, 2) correspond to test_date + 1 * units and test_date + 2 * units.
#   This is outside the time_value index for group c and is close to the
#   left edge for a and b, so if window_size = 7, the output should be
#   either empty or NA (depending if .all_rows is TRUE or not).
# - c(8, 9) corresponds to test_date + 8 * units amd test_date + 9 * units.
#   In this case, groups a and b have values, but c does not.
param_combinations <- bind_rows(
  tidyr::expand_grid(
    .time_type = c("day", "week", "yearmonth", "integer"),
    .ref_time_values = list(NULL, c(1, 2), c(8, 9)),
    .all_rows = c(FALSE, TRUE),
    .align = c("right", "center", "left"),
    .window_size = c(1, 7),
  ),
  tidyr::expand_grid(
    .time_type = c("day", "week", "yearmonth", "integer"),
    .ref_time_values = list(NULL, c(1, 2), c(8, 9)),
    .all_rows = c(FALSE, TRUE),
    .align = c("right"),
    .window_size = c(Inf),
  )
)
for (p in (param_combinations %>% transpose())) {
  test_data <- get_test_dataset(num_rows_per_group, p$.time_type)
  units <- get_test_units(p$.time_type)
  test_date <- get_test_date(p$.time_type)
  p$.window_size <- p$.window_size * units
  if (!is.null(p$.ref_time_values)) {
    p$.ref_time_values <- test_date + units * p$.ref_time_values
  }
  as_of <- attr(test_data, "metadata")$as_of
  slide_args <- p[setdiff(names(p), c(".time_type"))]

  test_that(
    format_inline(
      "epi_slide works correctly with formula vector output and params:\n",
      concatenate_list_params(p)
    ),
    {
      out <- rlang::inject(epi_slide(test_data, .f = ~ sum(.x$value), !!!slide_args))
      expected_out <- rlang::inject(epi_slide_sum_test(test_data, !!!slide_args))
      expect_equal(
        out,
        expected_out
      )
    }
  )

  test_that(
    format_inline(
      "epi_slide works correctly with formula data.frame output and params:\n",
      concatenate_list_params(p)
    ),
    {
      out <- rlang::inject(epi_slide(test_data, .f = ~ data.frame(slide_value = sum(.x$value)), !!!slide_args))
      expected_out <- rlang::inject(epi_slide_sum_test(test_data, !!!slide_args))
      expect_equal(
        out,
        expected_out
      )
    }
  )

  test_that(
    format_inline(
      "epi_slide works correctly with formula list output and params:\n",
      concatenate_list_params(p)
    ),
    {
      out <- rlang::inject(epi_slide(test_data, .f = ~ list(sum(.x$value)), !!!slide_args))
      expected_out <- rlang::inject(epi_slide_sum_test(test_data, !!!slide_args)) %>%
        rowwise() %>%
        mutate(slide_value = list(slide_value)) %>%
        ungroup() %>%
        as_epi_df(as_of = as_of) %>%
        group_by(geo_value)

      expect_equal(
        out %>% select(-slide_value),
        expected_out %>% select(-slide_value)
      )
      expect_equal_handle_null(out$slide_value, expected_out$slide_value)
    }
  )

  test_that(
    format_inline(
      "epi_slide works correctly with formula tibble list output and params:\n",
      concatenate_list_params(p)
    ),
    {
      out <- rlang::inject(epi_slide(test_data, .f = ~ tibble(slide_value = list(sum(.x$value))), !!!slide_args))
      expected_out <- rlang::inject(epi_slide_sum_test(test_data, !!!slide_args)) %>%
        rowwise() %>%
        mutate(slide_value = list(slide_value)) %>%
        ungroup() %>%
        as_epi_df(as_of = as_of) %>%
        group_by(geo_value)
      expect_equal(
        out %>% select(-slide_value),
        expected_out %>% select(-slide_value)
      )
      expect_equal_handle_null(out$slide_value, expected_out$slide_value)
    }
  )

  test_that(
    format_inline(
      "epi_slide works with unnamed data-masking data.frame and params:\n",
      concatenate_list_params(p)
    ),
    {
      expected_out <- rlang::inject(epi_slide_sum_test(test_data, !!!slide_args))
      expect_equal_mod(
        rlang::inject(epi_slide(
          test_data, , data.frame(slide_value = sum(.x$value)),
          !!!slide_args
        )),
        expected_out
      )
    }
  )

  test_that(
    format_inline(
      "epi_slide and epi_slide_opt/sum/mean outputs are consistent. Params:\n",
      concatenate_list_params(p)
    ),
    {
      out_sum <- rlang::inject(epi_slide(test_data, ~ sum(.x$value), !!!slide_args)) %>%
        rename(slide_value_value = slide_value)
      out_mean <- rlang::inject(epi_slide(test_data, ~ mean(.x$value), !!!slide_args)) %>%
        rename(slide_value_value = slide_value)

      expect_equal(
        out_sum,
        rlang::inject(epi_slide_opt(test_data, value, .f = data.table::frollsum, !!!slide_args))
      )
      expect_equal(
        out_sum,
        rlang::inject(epi_slide_opt(test_data, value, .f = slider::slide_sum, !!!slide_args))
      )
      expect_equal(
        out_sum,
        rlang::inject(epi_slide_sum(test_data, value, !!!slide_args))
      )
      expect_equal(
        out_mean,
        rlang::inject(epi_slide_opt(test_data, value, .f = data.table::frollmean, !!!slide_args))
      )
      expect_equal(
        out_mean,
        rlang::inject(epi_slide_opt(test_data, value, .f = slider::slide_mean, !!!slide_args))
      )
      expect_equal(
        out_mean,
        rlang::inject(epi_slide_mean(test_data, value, !!!slide_args))
      )
    }
  )
}

# TODO: This.
test_that(".window_size as integer works", {
  expect_equal(
    epi_slide(test_data, ~ sum(.x$value), .window_size = 7),
    epi_slide_sum_test(test_data, .window_size = 7)
  )
})

bad_values <- list(
  "a", 0.5, -1L, -1.5, 1.5, NA, c(0, 1)
)
for (bad_value in bad_values) {
  test_that(
    format_inline("`.window_size` fails on {bad_value}"),
    {
      expect_error(
        epi_slide(test_data, ~ sum(.x), .window_size = bad_value),
        class = "epiprocess__validate_slide_window_arg"
      )
      expect_error(
        epi_slide_mean(test_data, ~ sum(.x), .col_names = value, .window_size = bad_value),
        class = "epiprocess__validate_slide_window_arg"
      )
    }
  )
}

test_that(format_inline("epi_slide should fail when `.ref_time_values` is out of range for all groups "), {
  bad_values <- c(min(test_data$time_value) - 1, max(test_data$time_value) + 1)
  expect_error(
    epi_slide(test_data, ~ sum(.x), .ref_time_values = bad_values),
    class = "epiprocess__epi_slide_invalid_ref_time_values"
  )
  expect_error(
    epi_slide_mean(test_data, .col_names = value, .ref_time_values = bad_values),
    class = "epiprocess__epi_slide_opt_invalid_ref_time_values"
  )
})

test_that("epi_slide alerts if the provided f doesn't take enough args", {
  f_tib_avg_count <- function(x, g, t) dplyr::tibble(avg = mean(x$value), count = length(x$value))
  expect_no_error(
    epi_slide(test_data, f_tib_avg_count),
  )
  expect_no_warning(
    epi_slide(test_data, f_tib_avg_count),
  )

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$value), count = length(x$value))
  expect_warning(epi_slide(test_data, f_x_dots),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
})

test_that("epi_slide computation via f can use ref_time_value", {
  expected_out <- test_data %>% mutate(slide_value = time_value)
  expect_equal(
    test_data %>% epi_slide(~.ref_time_value),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(~.z),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(~..3),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(.f = function(x, g, t) t),
    expected_out
  )
})

test_that("epi_slide computation via f can use group", {
  expected_out <- test_data %>% mutate(slide_value = geo_value)
  expect_equal(
    test_data %>% epi_slide(~ .group_key$geo_value),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(~ .y$geo_value),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(~ ..2$geo_value),
    expected_out
  )
  expect_equal(
    test_data %>% epi_slide(.f = function(x, g, t) g$geo_value),
    expected_out
  )
})

test_that("epi_slide computation via dots can use ref_time_value", {
  expect_equal(
    test_data %>% epi_slide(slide_value = .ref_time_value),
    test_data %>% mutate(slide_value = time_value)
  )
})

test_that("epi_slide computation via dots can use group", {
  expect_equal(
    test_data %>% epi_slide(slide_value = nrow(.group_key)),
    test_data %>% mutate(slide_value = 1L)
  )
  expect_equal(
    test_data %>% epi_slide(slide_value = .group_key$geo_value),
    test_data %>% mutate(slide_value = geo_value)
  )
})

test_that("epi_slide computation should not allow access from .data and .env", {
  expect_error(test_data %>% epi_slide(slide_value = .env$.ref_time_value))
  expect_error(test_data %>% epi_slide(slide_value = .data$.ref_time_value))
  expect_error(test_data %>% epi_slide(slide_value = .env$.group_key))
  expect_error(test_data %>% epi_slide(slide_value = .data$.group_key))
})

test_that("epi_slide computation via dots outputs the same result using col names and the data var", {
  expected_output <- test_data %>% epi_slide(slide_value = max(time_value))

  expect_equal(
    test_data %>% epi_slide(slide_value = max(.x$time_value)),
    expected_output
  )
  expect_equal(
    test_data %>% epi_slide(slide_value = max(.data$time_value)),
    expected_output
  )
})

test_that("`epi_slide` can access objects inside of helper functions", {
  helper <- function(archive_haystack, time_value_needle) {
    archive_haystack %>% epi_slide(
      has_needle = time_value_needle %in% time_value, .window_size = Inf
    )
  }
  expect_no_error(helper(test_data, as.Date("2021-01-01")))
})

test_that("epi_slide can use sequential data masking expressions including NULL", {
  edf_a <- tibble::tibble(
    geo_value = 1,
    time_value = 1:10,
    value = 1:10 * 1.0
  ) %>%
    as_epi_df(as_of = 12L)

  out1 <- edf_a %>%
    group_by(geo_value) %>%
    epi_slide(
      .window_size = 5L, .align = "center",
      m1 = .x$value[1],
      m5 = .x$value[5],
      derived_m5 = m1 + 4,
      m1 = NULL
    ) %>%
    ungroup() %>%
    tidyr::drop_na() %>%
    as_epi_df(as_of = 12L)
  expect_equal(out1$m5, out1$derived_m5)

  out2 <- edf_a %>%
    group_by(geo_value) %>%
    epi_slide(
      .window_size = 5L, .align = "center",
      m1 = list(.x$value[1]),
      m5 = list(.x$value[5]),
      derived_m5 = list(m1[[1]] + 4)
    ) %>%
    ungroup() %>%
    filter(!is.na(m5)) %>%
    as_epi_df(as_of = 12L)
  expect_equal(out2$m5, out2$derived_m5)
})

test_that("epi_slide complains on invalid computation outputs", {
  expect_error(
    test_data %>% epi_slide(~ lm(value ~ time_value, .x)),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_no_error(
    test_data %>% epi_slide(~ list(lm(value ~ time_value, .x))),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_error(
    test_data %>% epi_slide(model = lm(value ~ time_value, .x)),
    class = "epiprocess__invalid_slide_comp_tidyeval_output"
  )
  expect_no_error(
    test_data %>% epi_slide(model = list(lm(value ~ time_value, .x))),
    class = "epiprocess__invalid_slide_comp_tidyeval_output"
  )
  expect_error(
    test_data %>%
      epi_slide(.window_size = 6, ~ sum(.x$value) + c(0, 0, 0)),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_error(
    test_data %>%
      epi_slide(.window_size = 6, ~ as.list(sum(.x$value) + c(0, 0, 0))),
    class = "epiprocess__invalid_slide_comp_value"
  )
  expect_error(
    test_data %>%
      epi_slide(.window_size = 6, ~ data.frame(slide_value = sum(.x$value) + c(0, 0, 0))),
    class = "epiprocess__invalid_slide_comp_value"
  )
})

test_that("epi_slide can use {nm} :=", {
  nm <- "slide_value"
  expect_identical(
    # unfortunately, we can't pass this directly as `f` and need an extra comma
    test_data %>% epi_slide(, !!nm := sum(value), .window_size = 7),
    epi_slide_sum_test(test_data, .window_size = 7)
  )
})

test_that("epi_slide can produce packed outputs", {
  packed_basic_result <- epi_slide_sum_test(test_data, .window_size = 7) %>%
    tidyr::pack(container = c(slide_value)) %>%
    dplyr_reconstruct(epi_slide_sum_test(test_data, .window_size = 7))
  expect_identical(
    test_data %>%
      epi_slide(~ tibble::tibble(slide_value = sum(.x$value)), .new_col_name = "container", .window_size = 7),
    packed_basic_result
  )
  expect_identical(
    test_data %>%
      epi_slide(container = tibble::tibble(slide_value = sum(.x$value)), .window_size = 7),
    packed_basic_result
  )
  expect_identical(
    test_data %>%
      epi_slide(, tibble::tibble(slide_value = sum(.x$value)), .new_col_name = "container", .window_size = 7),
    packed_basic_result
  )
})

test_that("nested dataframe output names are controllable", {
  expect_equal(
    test_data %>% epi_slide(~ data.frame(result = sum(.x$value)), .window_size = 7),
    epi_slide_sum_test(test_data, .window_size = 7) %>% rename(result = slide_value)
  )
  expect_equal(
    test_data %>% epi_slide(~ data.frame(value_sum = sum(.x$value)), .window_size = 7),
    epi_slide_sum_test(test_data, .window_size = 7) %>% rename(value_sum = slide_value)
  )
})

test_that("`epi_slide` doesn't lose Date class output", {
  expect_true(
    test_data %>%
      epi_slide(.window_size = 7, ~ as.Date("2020-01-01")) %>%
      `[[`("slide_value") %>%
      inherits("Date")
  )
})

test_that("epi_slide_opt helper `full_date_seq` returns expected date values", {
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
      before = before, after = after, time_type = "day"
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
      before = before, after = after, time_type = "day"
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
      before = before, after = after, time_type = "day"
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


test_that("`epi_slide_opt` errors when passed non-`data.table`, non-`slider` functions", {
  reexport_frollmean <- data.table::frollmean
  expect_no_error(
    epi_slide_opt(
      test_data,
      .col_names = value, .f = reexport_frollmean
    )
  )
  expect_error(
    epi_slide_opt(
      test_data,
      .col_names = value, .f = mean
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
    multi_slid <- epi_slide_mean(multi_columns, .col_names = c("value", "value2"), .window_size = 7)
  )
  expect_equal(
    names(multi_slid),
    c("geo_value", "time_value", "value", "value2", "slide_value_value", "slide_value_value2")
  )
  expect_no_warning(
    multi_slid_select <- epi_slide_mean(multi_columns, c(value, value2), .window_size = 7)
  )
  expect_equal(multi_slid_select, multi_slid)
  expect_no_warning(
    multi_slid_select <- epi_slide_mean(multi_columns, starts_with("value"), .window_size = 7)
  )
  expect_equal(multi_slid_select, multi_slid)
})
