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
  # Below cases should raise no errors/warnings:
  expect_warning(epi_slide(grouped, f, before = 1L, ref_time_values = d + 2), NA)
  expect_warning(epi_slide(grouped, f, after = 1L, ref_time_values = d + 2), NA)
  expect_warning(epi_slide(grouped, f, before = 0L, after = 0L, ref_time_values = d + 2), NA)
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
  # Non-integer-class but integer-compatible values are allowed:
  expect_error(epi_slide(grouped, f, before = 1, after = 1, ref_time_values = d + 2L), NA)
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
})

test_that(
  "`ref_time_values` + `before` + `after` that have some slide data, but
  generate the error due to ref. time being out of time range (would also
  happen if they were in between `time_value`s)",
  {
    expect_error(
      epi_slide(grouped, f, before = 0L, after = 2L, ref_time_values = d),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # before the first, but we'd expect there to be data in the window
    expect_error(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 201L),
      "`ref_time_values` must be a unique subset of the time values in `x`."
    ) # beyond the last, but still with data in window
  }
)

## --- These cases generate warnings (or not): ---
test_that("Warn user against having a blank `before`", {
  expect_warning(epi_slide(grouped, f, after = 1L, ref_time_values = d + 1L), NA)
  expect_warning(epi_slide(grouped, f, before = 0L, after = 1L, ref_time_values = d + 1L), NA)
})

## --- These cases doesn't generate the error: ---
test_that(
  "these doesn't produce an error; the error appears only if the ref
  time values are out of the range for every group",
  {
    expect_identical(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 200L) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = "ak", slide_value_value = 199)
    ) # out of range for one group
    expect_identical(
      epi_slide(grouped, f, before = 2L, ref_time_values = d + 3) %>%
        ungroup() %>%
        dplyr::select("geo_value", "slide_value_value"),
      dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))
    ) # not out of range for either group
  }
)

test_that("computation output formats x as_list_col", {
  # See `toy_edf` definition at top of file.
  # We'll try 7d sum with a few formats.
  # nolint start: line_length_linter.
  basic_result_from_size1 <- tibble::tribble(
    ~geo_value, ~time_value, ~value, ~slide_value,
    "a", 1:10, 2L^(1:10), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
    "b", 1:10, 2L^(11:20), data.table::frollsum(2L^(1:10) + 2L^(11:20), c(1:7, rep(7L, 3L)), adaptive = TRUE, na.rm = TRUE),
  ) %>%
    tidyr::unchop(c(time_value, value, slide_value)) %>%
    dplyr::arrange(time_value) %>%
    as_epi_df(as_of = 100)
  # nolint end
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value)),
    basic_result_from_size1
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value), as_list_col = TRUE),
    basic_result_from_size1 %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value))),
    basic_result_from_size1 %>% rename(slide_value_value = slide_value)
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value)), as_list_col = TRUE),
    basic_result_from_size1 %>%
      mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x)))
  )
  # output naming functionality:
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      new_col_name = "result"
    ),
    basic_result_from_size1 %>% rename(result_value = slide_value)
  )
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value_sum = sum(.x$value)),
      names_sep = NULL
    ),
    basic_result_from_size1 %>% rename(value_sum = slide_value)
  )
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
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value) + 0:1),
    basic_result_from_size2
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value) + 0:1, as_list_col = TRUE),
    basic_result_from_size2 %>% dplyr::mutate(slide_value = as.list(slide_value))
  )
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value) + 0:1)),
    basic_result_from_size2 %>% rename(slide_value_value = slide_value)
  )
  expect_identical(
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
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ sum(.x$value)),
    basic_full_result
  )
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ sum(.x$value),
      ref_time_values = c(2L, 8L)
    ),
    basic_full_result %>% dplyr::filter(time_value %in% c(2L, 8L))
  )
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ sum(.x$value),
      ref_time_values = c(2L, 8L), all_rows = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = dplyr::if_else(time_value %in% c(2L, 8L),
        slide_value, NA_integer_
      ))
  )
  # slide computations returning data frames:
  expect_identical(
    toy_edf %>% epi_slide(before = 6L, ~ data.frame(value = sum(.x$value))),
    basic_full_result %>% dplyr::rename(slide_value_value = slide_value)
  )
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L)
    ),
    basic_full_result %>%
      dplyr::filter(time_value %in% c(2L, 8L)) %>%
      dplyr::rename(slide_value_value = slide_value)
  )
  expect_identical(
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
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      as_list_col = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x)))
  )
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      ref_time_values = c(2L, 8L),
      as_list_col = TRUE
    ),
    basic_full_result %>%
      dplyr::mutate(slide_value = purrr::map(slide_value, ~ data.frame(value = .x))) %>%
      dplyr::filter(time_value %in% c(2L, 8L))
  )
  expect_identical(
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
  expect_identical(
    toy_edf %>% epi_slide(
      before = 6L, ~ data.frame(value = sum(.x$value)),
      as_list_col = TRUE
    ) %>%
      unnest(slide_value, names_sep = "_"),
    basic_full_result %>% dplyr::rename(slide_value_value = slide_value)
  )
  expect_identical(
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
  expect_identical(
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
  expect_identical(
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
  expect_identical(result1, expected_output)

  # function
  result2 <- epi_slide(small_x, f = function(x, g, t) sum(x$value), before = 50)
  expect_identical(result2, expected_output)

  # dots
  result3 <- epi_slide(small_x, slide_value = sum(value), before = 50)
  expect_identical(result3, expected_output)
})

test_that("ungrouped epi_slide computation completes successfully", {
  expect_error(
    small_x %>%
      ungroup() %>%
      epi_slide(
        before = 2,
        slide_value = sum(.x$value)
      ),
    regexp = NA
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
  expect_identical(result1, expected_output)

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
  expect_identical(result2, expected_output)
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

  expect_identical(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(
      f = ~.z,
      before = 50
    )

  expect_identical(result2, expected_output)

  result3 <- small_x %>%
    epi_slide(
      f = ~..3,
      before = 50
    )

  expect_identical(result3, expected_output)

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
  expect_identical(result4, expected_output)
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

  expect_identical(result1, expected_output)
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

  expect_identical(result1, expected_output)

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

  expect_identical(result3, expected_output)

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

  expect_identical(result4, expected_output)

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
  expect_identical(result5, expected_output)
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

  expect_identical(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(
      before = 2,
      slide_value = max(.data$time_value)
    )

  expect_identical(result2, expected_output)
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

test_that("epi_slide basic behavior is correct when groups have non-overlapping date ranges", {
  small_x_misaligned_dates <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5))
  ) %>%
    as_epi_df(as_of = d + 6) %>%
    group_by(geo_value)

  expected_output <- dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value = cumsum(11:15)),
    dplyr::tibble(geo_value = "al", time_value = d + 151:155, value = -(1:5), slide_value = cumsum(-(1:5)))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- epi_slide(small_x_misaligned_dates, f = ~ sum(.x$value), before = 50)
  expect_identical(result1, expected_output)
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

  expect_identical(result1, expected_output)
})
