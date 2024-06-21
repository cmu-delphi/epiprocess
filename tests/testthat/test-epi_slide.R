## Create an epi. df and a function to test epi_slide with

d <- as.Date("2020-01-01")

ungrouped <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = d + 1:200, value = 1:200),
  dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5))
) %>%
  as_epi_df()
grouped <- ungrouped %>%
  group_by(geo_value)

small_x <- dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15),
  dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5))
) %>%
  as_epi_df(as_of = d + 6) %>%
  group_by(geo_value)


f <- function(x, g, t) dplyr::tibble(value = mean(x$value), count = length(x$value))

toy_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  "a", 1:10, 2L^(1:10),
  "b", 1:10, 2L^(11:20),
) %>%
  tidyr::unchop(c(time_value, value)) %>%
  as_epi_df(as_of = 100)

# nolint start: line_length_linter.
basic_sum_result <- tibble::tribble(
  ~geo_value, ~time_value, ~value, ~slide_value,
  "a", 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
  "b", 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
) %>%
  tidyr::unchop(c(time_value, value, slide_value)) %>%
  dplyr::arrange(time_value) %>%
  as_epi_df(as_of = 100)

basic_mean_result <- tibble::tribble(
  ~geo_value, ~time_value, ~value, ~slide_value,
  "a", 1:10, 2L^(1:10), data.table::frollmean(2L^(1:10), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
) %>%
  tidyr::unchop(c(time_value, value, slide_value)) %>%
  dplyr::arrange(time_value) %>%
  as_epi_df(as_of = 100)
# nolint end: line_length_linter.

## --- These cases generate errors (or not): ---
test_that("`before` and `after` are both vectors of length 1", {
  expect_error(
    epi_slide(grouped, f, before = c(0, 1), after = 0, ref_time_values = d + 3),
    "Assertion on 'before' failed: Must have length 1"
  )
  expect_error(
    epi_slide(grouped, f, before = 1, after = c(0, 1), ref_time_values = d + 3),
    "Assertion on 'after' failed: Must have length 1"
  )

  expect_error(
    epi_slide_mean(grouped, col_names = value, before = c(0, 1), after = 0, ref_time_values = d + 3),
    "Assertion on 'before' failed: Must have length 1"
  )
  expect_error(
    epi_slide_mean(grouped, col_names = value, before = 1, after = c(0, 1), ref_time_values = d + 3),
    "Assertion on 'after' failed: Must have length 1"
  )
})

test_that("Test errors/warnings for discouraged features", {
  expect_error(
    epi_slide(grouped, f, ref_time_values = d + 1),
    "Either or both of `before`, `after` must be provided."
  )
  expect_warning(
    epi_slide(grouped, f, before = 0L, ref_time_values = d + 1),
    "`before==0`, `after` missing"
  )
  expect_warning(
    epi_slide(grouped, f, after = 0L, ref_time_values = d + 1),
    "`before` missing, `after==0`"
  )

  expect_error(
    epi_slide_mean(grouped, col_names = value, ref_time_values = d + 1),
    "Either or both of `before`, `after` must be provided."
  )
  expect_warning(
    epi_slide_mean(grouped, col_names = value, before = 0L, ref_time_values = d + 1),
    "`before==0`, `after` missing"
  )
  expect_warning(
    epi_slide_mean(grouped, col_names = value, after = 0L, ref_time_values = d + 1),
    "`before` missing, `after==0`"
  )

  # Below cases should raise no errors/warnings:
  expect_no_warning(
    ref1 <- epi_slide(grouped, f, before = 1L, ref_time_values = d + 2)
  )
  expect_no_warning(
    ref2 <- epi_slide(grouped, f, after = 1L, ref_time_values = d + 2)
  )
  expect_no_warning(
    ref3 <- epi_slide(grouped, f,
      before = 0L, after = 0L, ref_time_values = d + 2
    )
  )

  expect_no_warning(
    opt1 <- epi_slide_mean(grouped,
      col_names = value,
      before = 1L, ref_time_values = d + 2, na.rm = TRUE
    )
  )
  expect_no_warning(
    opt2 <- epi_slide_mean(grouped,
      col_names = value,
      after = 1L, ref_time_values = d + 2, na.rm = TRUE
    )
  )
  expect_no_warning(
    opt3 <- epi_slide_mean(grouped,
      col_names = value,
      before = 0L, after = 0L, ref_time_values = d + 2, na.rm = TRUE
    )
  )

  # Results from epi_slide and epi_slide_mean should match
  expect_equal(select(ref1, -slide_value_count), opt1)
  expect_equal(select(ref2, -slide_value_count), opt2)
  expect_equal(select(ref3, -slide_value_count), opt3)
})

test_that("Both `before` and `after` must be non-NA, non-negative, integer-compatible", {
  expect_error(
    epi_slide(grouped, f, before = -1L, ref_time_values = d + 2L),
    "Assertion on 'before' failed: Element 1 is not >= 0"
  )
  expect_error(
    epi_slide(grouped, f, before = 2L, after = -1L, ref_time_values = d + 2L),
    "Assertion on 'after' failed: Element 1 is not >= 0"
  )
  expect_error(epi_slide(grouped, f, before = "a", ref_time_values = d + 2L),
    regexp = "before", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide(grouped, f, before = 1L, after = "a", ref_time_values = d + 2L),
    regexp = "after", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide(grouped, f, before = 0.5, ref_time_values = d + 2L),
    regexp = "before", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide(grouped, f, before = 1L, after = 0.5, ref_time_values = d + 2L),
    regexp = "after", class = "vctrs_error_incompatible_type"
  )
  expect_error(
    epi_slide(grouped, f, before = NA, after = 1L, ref_time_values = d + 2L),
    "Assertion on 'before' failed: May not be NA"
  )
  expect_error(
    epi_slide(grouped, f, before = 1L, after = NA, ref_time_values = d + 2L),
    "Assertion on 'after' failed: May not be NA"
  )

  expect_error(
    epi_slide_mean(grouped, col_names = value, before = -1L, ref_time_values = d + 2L),
    "Assertion on 'before' failed: Element 1 is not >= 0"
  )
  expect_error(
    epi_slide_mean(grouped, col_names = value, before = 2L, after = -1L, ref_time_values = d + 2L),
    "Assertion on 'after' failed: Element 1 is not >= 0"
  )
  expect_error(epi_slide_mean(grouped, col_names = value, before = "a", ref_time_values = d + 2L),
    regexp = "before", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide_mean(grouped, col_names = value, before = 1L, after = "a", ref_time_values = d + 2L),
    regexp = "after", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide_mean(grouped, col_names = value, before = 0.5, ref_time_values = d + 2L),
    regexp = "before", class = "vctrs_error_incompatible_type"
  )
  expect_error(epi_slide_mean(grouped, col_names = value, before = 1L, after = 0.5, ref_time_values = d + 2L),
    regexp = "after", class = "vctrs_error_incompatible_type"
  )
  expect_error(
    epi_slide_mean(grouped, col_names = value, before = NA, after = 1L, ref_time_values = d + 2L),
    "Assertion on 'before' failed: May not be NA"
  )
  expect_error(
    epi_slide_mean(grouped, col_names = value, before = 1L, after = NA, ref_time_values = d + 2L),
    "Assertion on 'after' failed: May not be NA"
  )

  # Non-integer-class but integer-compatible values are allowed:
  expect_no_error(
    ref <- epi_slide(grouped, f, before = 1, after = 1, ref_time_values = d + 2L)
  )
  expect_no_error(opt <- epi_slide_mean(
    grouped,
    col_names = value, before = 1, after = 1,
    ref_time_values = d + 2L, na.rm = TRUE
  ))

  # Results from epi_slide and epi_slide_mean should match
  expect_equal(select(ref, -slide_value_count), opt)
})

test_that("`ref_time_values` + `before` + `after` that result in no slide data, generate the error", {
  expect_error(
    epi_slide(grouped, f, before = 2L, ref_time_values = d),
    "`ref_time_values` must be a unique subset of the time values in `x`."
  ) # before the first, no data in the slide windows
  expect_error(
    epi_slide(grouped, f, before = 2L, ref_time_values = d + 207L),
    "`ref_time_values` must be a unique subset of the time values in `x`."
  ) # beyond the last, no data in window

  expect_error(
    epi_slide_mean(grouped, col_names = value, before = 2L, ref_time_values = d),
    "`ref_time_values` must be a unique subset of the time values in `x`."
  ) # before the first, no data in the slide windows
  expect_error(
    epi_slide_mean(grouped, col_names = value, before = 2L, ref_time_values = d + 207L),
    "`ref_time_values` must be a unique subset of the time values in `x`."
  ) # beyond the last, no data in window
})

test_that(
  c(
    "`ref_time_values` + `before` + `after` that have some slide data, but
            generate the error due to ref. time being out of time range (would
            also happen if they were in between `time_value`s)"
  ),
  {
    expect_error(
      epi_slide(grouped, f, before = 0L, after = 2L, ref_time_values = d),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # before the first, but we'd expect there to be data in the window
    expect_error(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 201L),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # beyond the last, but still with data in window

    expect_error(
      epi_slide_mean(grouped, value, before = 0L, after = 2L, ref_time_values = d),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # before the first, but we'd expect there to be data in the window
    expect_error(
      epi_slide_mean(grouped, value, before = 2L, ref_time_values = d + 201L),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # beyond the last, but still with data in window
  }
)

## --- These cases generate warnings (or not): ---
test_that("Warn user against having a blank `before`", {
  expect_no_warning(ref1 <- epi_slide(
    grouped, f,
    after = 1L, ref_time_values = d + 1L
  ))
  expect_no_warning(ref2 <- epi_slide(
    grouped, f,
    before = 0L, after = 1L, ref_time_values = d + 1L
  ))

  expect_no_warning(opt1 <- epi_slide_mean(
    grouped, value,
    after = 1L, ref_time_values = d + 1L, na.rm = TRUE
  ))
  expect_no_warning(opt2 <- epi_slide_mean(
    grouped, value,
    before = 0L, after = 1L,
    ref_time_values = d + 1L, na.rm = TRUE
  ))

  # Results from epi_slide and epi_slide_mean should match
  expect_equal(select(ref1, -slide_value_count), opt1)
  expect_equal(select(ref2, -slide_value_count), opt2)
})

## --- These cases doesn't generate the error: ---
test_that(
  c(
    "these doesn't produce an error; the error appears only if the ref time
            values are out of the range for every group"
  ),
  {
    expect_equal(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 200L) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = "ak", slide_value_value = 199)
    ) # out of range for one group
    expect_equal(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 3) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))
    ) # not out of range for either group

    expect_equal(
      epi_slide_mean(grouped, value, before = 2L, ref_time_values = d + 200L, na.rm = TRUE) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = "ak", slide_value_value = 199)
    ) # out of range for one group
    expect_equal(
      epi_slide_mean(grouped, value, before = 2L, ref_time_values = d + 3, na.rm = TRUE) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))
    ) # not out of range for either group
  }
)

test_that("computation output formats x as_list_col", {
  # See `toy_edf` and `basic_sum_result` definitions at top of file.
  # We'll try 7d sum with a few formats.
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value)),
    basic_sum_result
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value), as_list_col = TRUE),
    basic_sum_result %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value))),
    basic_sum_result %>% rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value)), as_list_col = TRUE),
    basic_sum_result %>%
      mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x)))
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
        before = 6L, na.rm = TRUE
      ),
    basic_mean_result %>% dplyr::mutate(
      slide_value_value = slide_value
    ) %>%
      select(-slide_value)
  )
  expect_error(
    toy_edf %>%
      filter(
        geo_value == "a"
      ) %>%
      epi_slide_mean(
        value,
        before = 6L, as_list_col = TRUE, na.rm = TRUE
      ),
    class = "epiprocess__epi_slide_opt__list_not_supported"
  )
  # `epi_slide_mean` doesn't return dataframe columns
})

test_that("nested dataframe output names are controllable", {
  expect_equal(
    toy_edf %>%
      epi_slide(
        before = 6L, ~ data.frame(value = sum(.x$value)),
        new_col_name = "result"
      ),
    basic_sum_result %>% rename(result_value = slide_value)
  )
  expect_equal(
    toy_edf %>%
      epi_slide(
        before = 6L, ~ data.frame(value_sum = sum(.x$value)),
        names_sep = NULL
      ),
    basic_sum_result %>% rename(value_sum = slide_value)
  )
})

test_that("non-size-1 outputs are recycled", {
  # trying with non-size-1 computation outputs:
  # nolint start: line_length_linter.
  basic_result_from_size2 <- tibble::tribble(
    ~geo_value, ~time_value, ~value, ~slide_value,
    "a", 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
    "b", 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE) + 1L,
  ) %>%
    tidyr::unchop(c(time_value, value, slide_value)) %>%
    dplyr::arrange(time_value) %>%
    as_epi_df(as_of = 100)
  # nolint end
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value) + 0:1),
    basic_result_from_size2
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value) + 0:1, as_list_col = TRUE),
    basic_result_from_size2 %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value) + 0:1)),
    basic_result_from_size2 %>% rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value) + 0:1), as_list_col = TRUE),
    basic_result_from_size2 %>%
      mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x)))
  )
})

test_that("epi_slide alerts if the provided f doesn't take enough args", {
  f_xgt <- function(x, g, t) dplyr::tibble(value = mean(x$value), count = length(x$value))
  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_error(epi_slide(grouped, f_xgt, before = 1L, ref_time_values = d + 1), regexp = NA)
  expect_warning(epi_slide(grouped, f_xgt, before = 1L, ref_time_values = d + 1), regexp = NA)

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$value), count = length(x$value))
  expect_warning(epi_slide(grouped, f_x_dots, before = 1L, ref_time_values = d + 1),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
})

test_that("`ref_time_values` + `all_rows = TRUE` works", {
  # See `toy_edf` definition at top of file. We'll do variants of a slide
  # returning the following:
  # nolint start: line_length_linter.
  basic_full_result <- tibble::tribble(
    ~geo_value, ~time_value, ~value, ~slide_value,
    "a", 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
    "b", 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
  ) %>%
    tidyr::unchop(c(time_value, value, slide_value)) %>%
    dplyr::arrange(time_value) %>%
    as_epi_df(as_of = 100)
  # nolint end
  # slide computations returning atomic vecs:
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value)),
    basic_full_result
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ sum(.x$value),
      ref_time_values = c(2L, 8L)
    ),
    basic_full_result %>% dplyr::filter(time_value %in% c(2L, 8L))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ sum(.x$value),
      ref_time_values = c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, NA_integer_
      ))
  )

  expect_equal(
    toy_edf %>% filter(
      geo_value == "a"
    ) %>%
      epi_slide_mean(
        value,
        before = 6L, names_sep = NULL, na.rm = TRUE
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
        before = 6L, ref_time_values = c(2L, 8L),
        names_sep = NULL, na.rm = TRUE
      ),
    filter(basic_mean_result, time_value %in% c(2L, 8L)) %>%
      rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% filter(
      geo_value == "a"
    ) %>%
      epi_slide_mean(
        value,
        before = 6L, ref_time_values = c(2L, 8L), all_rows = TRUE,
        names_sep = NULL, na.rm = TRUE
      ),
    basic_mean_result %>%
      dplyr::mutate(slide_value_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, NA_integer_
      )) %>%
      select(-slide_value)
  )

  # slide computations returning data frames:
  expect_equal(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value))),
    basic_full_result %>% dplyr::rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L)
    ),
    basic_full_result %>%
      dplyr::filter(time_value %in% c(2L, 8L)) %>%
      dplyr::rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, NA_integer_
      )) %>%
      dplyr::rename(slide_value_value = slide_value)
  )
  # slide computations returning data frames with `as_list_col=TRUE`:
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      as_list_col = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x)))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L),
      as_list_col = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x))) %>%
      dplyr::filter(time_value %in% c(2L, 8L))
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L), all_rows = TRUE,
      as_list_col = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x))) %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, list(NULL)
      ))
  )
  # slide computations returning data frames, `as_list_col = TRUE`, `unnest`:
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      as_list_col = TRUE
    ) %>%
      unnest(slide_value, names_sep = "_"),
    basic_full_result %>% dplyr::rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L),
      as_list_col = TRUE
    ) %>%
      unnest(slide_value, names_sep = "_"),
    basic_full_result %>%
      dplyr::filter(time_value %in% c(2L, 8L)) %>%
      dplyr::rename(slide_value_value = slide_value)
  )
  expect_equal(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L), all_rows = TRUE,
      as_list_col = TRUE
    ) %>%
      unnest(slide_value, names_sep = "_"),
    basic_full_result %>%
      # XXX unclear exactly what we want in this case. Current approach is
      # compatible with `vctrs::vec_detect_missing` but breaks `tidyr::unnest`
      # compatibility
      dplyr::filter(time_value %in% c(2L, 8L)) %>%
      dplyr::rename(slide_value_value = slide_value)
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
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L), all_rows = TRUE,
      as_list_col = TRUE
    ) %>%
      mutate(slide_value = rework_nulls(slide_value)) %>%
      unnest(slide_value, names_sep = "_"),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, NA_integer_
      )) %>%
      dplyr::rename(slide_value_value = slide_value)
  )
})

test_that("`epi_slide` doesn't decay date output", {
  expect_true(
    ungrouped %>%
      epi_slide(before = 5L, ~ as.Date("2020-01-01")) %>%
      `[[`("slide_value") %>%
      inherits("Date")
  )
})

test_that("basic grouped epi_slide computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15)),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = cumsum(-(1:5)))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  # formula
  result1 <- epi_slide(small_x, f = ~ sum(.x$value), before = 50)
  expect_equal(result1, expected_output)

  # function
  result2 <- epi_slide(small_x, f = function(x, g, t) sum(x$value), before = 50)
  expect_equal(result2, expected_output)

  # dots
  result3 <- epi_slide(small_x, slide_value = sum(value), before = 50)
  expect_equal(result3, expected_output)
})

test_that("basic grouped epi_slide_mean computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = cumsum(-(1:5)) / 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- epi_slide_mean(small_x, value, before = 50, names_sep = NULL, na.rm = TRUE)
  expect_equal(result1, expected_output %>% rename(slide_value_value = slide_value))
})

test_that("ungrouped epi_slide computation completes successfully", {
  expect_no_error(
    small_x %>%
      ungroup() %>%
      epi_slide(
        before = 2,
        slide_value = sum(.x$value)
      )
  )
})

test_that("basic ungrouped epi_slide computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15))
  ) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    ungroup() %>%
    filter(geo_value == "ak") %>%
    epi_slide(
      before = 50,
      slide_value = sum(.x$value)
    )
  expect_equal(result1, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(
      geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15) + cumsum(-(1:5))
    ),
    dplyr::tibble(
      geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = cumsum(11:15) + cumsum(-(1:5))
    )
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    arrange(time_value)

  result2 <- small_x %>%
    ungroup() %>%
    epi_slide(
      before = 50,
      slide_value = sum(.x$value)
    )
  expect_equal(result2, expected_output)
})

test_that("basic ungrouped epi_slide_mean computation produces expected output", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
  ) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    ungroup() %>%
    filter(geo_value == "ak") %>%
    epi_slide_mean(value, before = 50, names_sep = NULL, na.rm = TRUE)
  expect_equal(result1, expected_output %>% rename(slide_value_value = slide_value))

  # Ungrouped with multiple geos
  # epi_slide_mean fails when input data groups contain duplicate time_values,
  # e.g. aggregating across geos
  expect_error(
    small_x %>% ungroup() %>% epi_slide_mean(value, before = 6L),
    class = "epiprocess__epi_slide_opt__duplicate_time_values"
  )
})

test_that("epi_slide computation via formula can use ref_time_value", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = d + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(
      f = ~.ref_time_value,
      before = 50
    )

  expect_equal(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(
      f = ~.z,
      before = 50
    )

  expect_equal(result2, expected_output)

  result3 <- small_x %>%
    epi_slide(
      f = ~..3,
      before = 50
    )

  expect_equal(result3, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = d + 1:5)
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    arrange(time_value)

  result4 <- small_x %>%
    ungroup() %>%
    epi_slide(
      f = ~.ref_time_value,
      before = 50
    )
  expect_equal(result4, expected_output)
})

test_that("epi_slide computation via function can use ref_time_value", {
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = d + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(
      f = function(x, g, t) t,
      before = 2
    )

  expect_equal(result1, expected_output)
})

test_that("epi_slide computation via dots can use ref_time_value and group", {
  # ref_time_value
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = d + 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(
      before = 50,
      slide_value = .ref_time_value
    )

  expect_equal(result1, expected_output)

  # `.{x,group_key,ref_time_value}` should be inaccessible from `.data` and
  # `.env`.
  expect_error(small_x %>%
    epi_slide(
      before = 50,
      slide_value = .env$.ref_time_value
    ))

  # group_key
  # Use group_key column
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = "ak"),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = "al")
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result3 <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = .group_key$geo_value
    )

  expect_equal(result3, expected_output)

  # Use entire group_key object
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = 1L),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = 1L)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result4 <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = nrow(.group_key)
    )

  expect_equal(result4, expected_output)

  # Ungrouped with multiple geos
  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value = d + 1:5)
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    arrange(time_value)

  result5 <- small_x %>%
    ungroup() %>%
    epi_slide(
      before = 50,
      slide_value = .ref_time_value
    )
  expect_equal(result5, expected_output)
})

test_that("epi_slide computation via dots outputs the same result using col names and the data var", {
  expected_output <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = max(time_value)
    ) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = max(.x$time_value)
    )

  expect_equal(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = max(.data$time_value)
    )

  expect_equal(result2, expected_output)
})

test_that("`epi_slide` can access objects inside of helper functions", {
  helper <- function(archive_haystack, time_value_needle) {
    archive_haystack %>% epi_slide(has_needle = time_value_needle %in% time_value, before = 365000L)
  }
  expect_error(
    helper(small_x, as.Date("2021-01-01")),
    NA
  )
})

test_that("basic slide behavior is correct when groups have non-overlapping date ranges", {
  small_x_misaligned_dates <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5))
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    group_by(geo_value)

  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15) / 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5), slide_value = cumsum(-(1:5)) / 1:5)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- epi_slide(small_x_misaligned_dates, f = ~ mean(.x$value), before = 50)
  expect_equal(result1, expected_output)

  result2 <- epi_slide_mean(small_x_misaligned_dates, value, before = 50, names_sep = NULL, na.rm = TRUE)
  expect_equal(result2, expected_output %>% rename(slide_value_value = slide_value))
})


test_that("epi_slide gets correct ref_time_value when groups have non-overlapping date ranges", {
  small_x_misaligned_dates <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5))
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    group_by(geo_value)

  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = d + 1:5),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5), slide_value = d + 151:155)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x_misaligned_dates %>%
    epi_slide(
      before = 50,
      slide_value = .ref_time_value
    )

  expect_equal(result1, expected_output)
})

test_that("results for different `before`s and `after`s match between epi_slide and epi_slide_mean", {
  test_time_type_mean <- function(dates, vals, before = 6L, after = 0L, n, m, n_obs, k, ...) {
    # Three states, with 2 variables. a is linear, going up in one state and down in the other
    # b is just random. last (m-1):(n-1) dates are missing
    epi_data <- epiprocess::as_epi_df(rbind(tibble(
      geo_value = "al",
      time_value = dates,
      a = 1:n_obs,
      b = vals
    ), tibble(
      geo_value = "ca",
      time_value = dates,
      a = n_obs:1,
      b = vals + 10
    ))) %>%
      group_by(geo_value)

    # Use the `epi_slide` result as a reference.
    result1 <- epi_slide(epi_data, ~ data.frame(
      slide_value_a = mean(.x$a, rm.na = TRUE),
      slide_value_b = mean(.x$b, rm.na = TRUE)
    ),
    before = before, after = after, names_sep = NULL, ...
    )
    result2 <- epi_slide_mean(epi_data,
      col_names = c(a, b), na.rm = TRUE,
      before = before, after = after, ...
    )
    expect_equal(result1, result2)
  }

  set.seed(0)

  # 3 missing dates
  n <- 15 # Max date index
  m <- 3 # Number of missing dates
  n_obs <- n + 1 - m # Number of obs created
  k <- c(0:(n - (m + 1)), n) # Date indices

  rand_vals <- rnorm(n_obs)
  # Basic time type
  days <- as.Date("2022-01-01") + k

  test_time_type_mean(days, rand_vals, before = 6, after = 0, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 6, after = 1, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 6, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 1, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 0, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 0, after = 1, n = n, m = m, n_obs = n_obs, k = k)

  # Without any missing dates
  n <- 15 # Max date index
  m <- 0 # Number of missing dates
  n_obs <- n + 1 - m # Number of obs created
  k <- c(0:(n - (m + 1)), n) # Date indices

  rand_vals <- rnorm(n_obs)
  # Basic time type
  days <- as.Date("2022-01-01") + k

  test_time_type_mean(days, rand_vals, before = 6, after = 0, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 6, after = 1, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 6, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 1, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 0, after = 6, n = n, m = m, n_obs = n_obs, k = k)
  test_time_type_mean(days, rand_vals, before = 0, after = 1, n = n, m = m, n_obs = n_obs, k = k)
})

test_that("results for different time_types match between epi_slide and epi_slide_mean", {
  n <- 6L # Max date index
  m <- 1L # Number of missing dates
  n_obs <- n + 1L - m # Number of obs created
  k <- c(0L:(n - (m + 1L)), n) # Date indices

  set.seed(0)
  rand_vals <- rnorm(n_obs)

  generate_special_date_data <- function(date_seq, ...) {
    epiprocess::as_epi_df(rbind(tibble(
      geo_value = "al",
      time_value = date_seq,
      a = seq_along(date_seq),
      b = rand_vals
    ), tibble(
      geo_value = "ca",
      time_value = date_seq,
      a = rev(seq_along(date_seq)),
      b = rand_vals + 10
    ), tibble(
      geo_value = "fl",
      time_value = date_seq,
      a = rev(seq_along(date_seq)),
      b = rand_vals * 2
    )), ...)
  }

  # Basic time type
  days <- as.Date("2022-01-01") + k

  # Require lubridate::period function to be passed as `time_step`
  day_times_minute <- lubridate::ydm_h("2022-01-01-15") + lubridate::minutes(k) # needs time_step = lubridate::minutes
  day_times_hour <- lubridate::ydm_h("2022-01-01-15") + lubridate::hours(k) # needs time_step = lubridate::hours
  weeks <- as.Date("2022-01-01") + 7L * k # needs time_step = lubridate::weeks

  # Don't require a `time_step` fn
  yearweeks <- tsibble::yearweek(10L + k)
  yearmonths <- tsibble::yearmonth(10L + k)
  yearquarters <- tsibble::yearquarter(10L + k)
  years <- 2000L + k # does NOT need time_step = lubridate::years because dates are numeric, not a special date format

  # Not supported
  custom_dates <- c(
    "January 1, 2022", "January 2, 2022", "January 3, 2022",
    "January 4, 2022", "January 5, 2022", "January 6, 2022"
  )
  not_dates <- c("a", "b", "c", "d", "e", "f")

  ref_epi_data <- generate_special_date_data(days) %>%
    group_by(geo_value)

  ref_result <- epi_slide(ref_epi_data, ~ data.frame(
    slide_value_a = mean(.x$a, rm.na = TRUE),
    slide_value_b = mean(.x$b, rm.na = TRUE)
  ),
  before = 6L, after = 0L, names_sep = NULL
  )

  test_time_type_mean <- function(dates, before = 6L, after = 0L, ...) {
    # Three states, with 2 variables. a is linear, going up in one state and down in the other
    # b is just random. date 10 is missing
    epi_data <- generate_special_date_data(dates) %>%
      group_by(geo_value)

    result1 <- epi_slide(epi_data, ~ data.frame(
      slide_value_a = mean(.x$a, rm.na = TRUE),
      slide_value_b = mean(.x$b, rm.na = TRUE)
    ),
    before = before, after = after, names_sep = NULL, ...
    )
    result2 <- epi_slide_mean(epi_data,
      col_names = c(a, b), na.rm = TRUE,
      before = before, after = after, ...
    )
    expect_equal(result1, result2)

    # All fields except dates
    expect_equal(select(ref_result, -time_value), select(result1, -time_value))
    expect_equal(select(ref_result, -time_value), select(result2, -time_value))
  }

  test_time_type_mean(days)
  test_time_type_mean(yearweeks)
  test_time_type_mean(yearmonths)
  test_time_type_mean(yearquarters)
  test_time_type_mean(years)
  test_time_type_mean(day_times_minute, time_step = lubridate::minutes)
  test_time_type_mean(day_times_hour, time_step = lubridate::hours)
  test_time_type_mean(weeks, time_step = lubridate::weeks)

  # `epi_slide_mean` can also handle `weeks` without `time_step` being
  # provided, but `epi_slide` can't
  epi_data <- generate_special_date_data(weeks) %>%
    group_by(geo_value)
  result2 <- epi_slide_mean(epi_data,
    col_names = c(a, b), na.rm = TRUE,
    before = 6L, after = 0L
  )
  expect_equal(select(ref_result, -time_value), select(result2, -time_value))
})

test_that("special time_types without time_step fail in epi_slide_mean", {
  n_obs <- 6
  k <- 1:n_obs

  day_times_minute <- lubridate::ydm_h("2022-01-01-15") + lubridate::minutes(k) # needs time_step = lubridate::minutes
  day_times_hour <- lubridate::ydm_h("2022-01-01-15") + lubridate::hours(k) # needs time_step = lubridate::hours

  # Not supported
  custom_dates <- c(
    "January 1, 2022", "January 2, 2022", "January 3, 2022",
    "January 4, 2022", "January 5, 2022", "January 6, 2022"
  )
  not_dates <- c("a", "b", "c", "d", "e", "f")

  test_time_type_mean <- function(dates, before = 6L, after = 0L, ...) {
    epi_data <- epiprocess::as_epi_df(tibble(
      geo_value = "al",
      time_value = dates,
      a = 1:n_obs
    ))

    expect_error(
      epi_slide_mean(epi_data,
        col_names = a,
        before = before, after = after
      ),
      class = "epiprocess__full_date_seq__unmappable_time_type"
    )
  }

  test_time_type_mean(custom_dates)
  test_time_type_mean(not_dates)
  test_time_type_mean(day_times_minute)
  test_time_type_mean(day_times_hour)
})

test_that("helper `full_date_seq` returns expected date values", {
  n <- 6L # Max date index
  m <- 1L # Number of missing dates
  n_obs <- n + 1L - m # Number of obs created
  k <- c(0L:(n - (m + 1L)), n) # Date indices

  set.seed(0)
  rand_vals <- rnorm(n_obs)

  generate_special_date_data <- function(date_seq, ...) {
    epiprocess::as_epi_df(rbind(tibble(
      geo_value = "al",
      time_value = date_seq,
      a = seq_along(date_seq),
      b = rand_vals
    ), tibble(
      geo_value = "ca",
      time_value = date_seq,
      a = rev(seq_along(date_seq)),
      b = rand_vals + 10
    ), tibble(
      geo_value = "fl",
      time_value = date_seq,
      a = rev(seq_along(date_seq)),
      b = rand_vals * 2
    )), ...)
  }

  # Basic time type
  days <- as.Date("2022-01-01") + k

  # Require lubridate::period function to be passed as `time_step`
  day_times_minute <- lubridate::ydm_h("2022-01-01-15") + lubridate::minutes(k) # needs time_step = lubridate::minutes
  day_times_hour <- lubridate::ydm_h("2022-01-01-15") + lubridate::hours(k) # needs time_step = lubridate::hours
  weeks <- as.Date("2022-01-01") + 7L * k # needs time_step = lubridate::weeks

  # Don't require a `time_step` fn
  yearweeks <- tsibble::yearweek(10L + k)
  yearmonths <- tsibble::yearmonth(10L + k)
  yearquarters <- tsibble::yearquarter(10L + k)
  years <- 2000L + k # does NOT need time_step = lubridate::years because dates are numeric, not a special date format

  before <- 2L
  after <- 1L

  expect_identical(
    full_date_seq(
      generate_special_date_data(days),
      before = before, after = after
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
      generate_special_date_data(yearweeks),
      before = before, after = after
    ),
    list(
      all_dates = tsibble::yearweek(10:16),
      pad_early_dates = tsibble::yearweek(8:9),
      pad_late_dates = tsibble::yearweek(17)
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(yearmonths),
      before = before, after = after
    ),
    list(
      all_dates = tsibble::yearmonth(10:16),
      pad_early_dates = tsibble::yearmonth(8:9),
      pad_late_dates = tsibble::yearmonth(17)
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(yearquarters),
      before = before, after = after
    ),
    list(
      all_dates = tsibble::yearquarter(10:16),
      pad_early_dates = tsibble::yearquarter(8:9),
      pad_late_dates = tsibble::yearquarter(17)
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(years),
      before = before, after = after
    ),
    list(
      all_dates = 2000L:2006L,
      pad_early_dates = 1998L:1999L,
      pad_late_dates = 2007L
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(day_times_minute),
      before = before, after = after,
      time_step = lubridate::minutes
    ),
    list(
      all_dates = lubridate::ydm_h("2022-01-01-15") + lubridate::minutes(0:6),
      pad_early_dates = lubridate::ydm_h("2022-01-01-15") - lubridate::minutes(2:1),
      pad_late_dates = lubridate::ydm_h("2022-01-01-15") + lubridate::minutes(7)
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(day_times_hour),
      before = before, after = after,
      time_step = lubridate::hours
    ),
    list(
      all_dates = lubridate::ydm_h("2022-01-01-15") + lubridate::hours(0:6),
      pad_early_dates = lubridate::ydm_h("2022-01-01-15") - lubridate::hours(2:1),
      pad_late_dates = lubridate::ydm_h("2022-01-01-15") + lubridate::hours(7)
    )
  )
  expect_identical(
    full_date_seq(
      generate_special_date_data(weeks),
      before = before, after = after,
      time_step = lubridate::weeks
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
  # Check the middle branch (`if (missing(time_step))`) of `full_date_seq`.
  expect_identical(
    full_date_seq(
      generate_special_date_data(weeks, time_type = "week"),
      before = before, after = after
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

  # Other before/after values
  before <- 5L
  after <- 0L

  expect_identical(
    full_date_seq(
      generate_special_date_data(days),
      before = before, after = after
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
      generate_special_date_data(days),
      before = before, after = after
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

test_that("`epi_slide_mean` errors when passed `time_values` with closer than expected spacing", {
  time_df <- tibble(
    geo_value = 1,
    value = c(0:7, 3.5, 10, 20),
    # Adding the value 3.5 creates a time that has fractional seconds, which
    # doesn't follow the expected 1-second spacing of the `time_values`.
    # This leads to `frollmean` using obs spanning less than the expected
    # time frame for some computation windows.
    time_value = Sys.time() + value
  ) %>%
    as_epi_df()
  expect_error(
    epi_slide_mean(time_df, value, before = 6L, time_step = lubridate::seconds),
    class = "epiprocess__epi_slide_opt__unexpected_row_number"
  )
})

test_that("epi_slide_mean produces same output as epi_slide_opt", {
  result1 <- epi_slide_mean(small_x, value, before = 50, names_sep = NULL, na.rm = TRUE)
  result2 <- epi_slide_opt(small_x, value,
    f = data.table::frollmean,
    before = 50, names_sep = NULL, na.rm = TRUE
  )
  expect_equal(result1, result2)

  result3 <- epi_slide_opt(small_x, value,
    f = slider::slide_mean,
    before = 50, names_sep = NULL, na_rm = TRUE
  )
  expect_equal(result1, result3)
})

test_that("epi_slide_sum produces same output as epi_slide_opt", {
  result1 <- epi_slide_sum(small_x, value, before = 50, names_sep = NULL, na.rm = TRUE)
  result2 <- epi_slide_opt(small_x, value,
    f = data.table::frollsum,
    before = 50, names_sep = NULL, na.rm = TRUE
  )
  expect_equal(result1, result2)

  result3 <- epi_slide_opt(small_x, value,
    f = slider::slide_sum,
    before = 50, names_sep = NULL, na_rm = TRUE
  )
  expect_equal(result1, result3)
})

test_that("`epi_slide_opt` errors when passed non-`data.table`, non-`slider` functions", {
  expect_no_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = data.table::frollmean,
      before = 1L, after = 0L, ref_time_values = d + 1
    )
  )
  expect_no_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = slider::slide_min,
      before = 1L, after = 0L, ref_time_values = d + 1
    )
  )

  reexport_frollmean <- data.table::frollmean
  expect_no_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = reexport_frollmean,
      before = 1L, after = 0L, ref_time_values = d + 1
    )
  )

  expect_error(
    epi_slide_opt(
      grouped,
      col_names = value, f = mean,
      before = 1L, after = 0L, ref_time_values = d + 1
    ),
    class = "epiprocess__epi_slide_opt__unsupported_slide_function"
  )
})
