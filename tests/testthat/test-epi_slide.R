## Create an epi. df and a function to test epi_slide with

d <- as.Date("2020-01-01")

grouped = dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = d + 1:200, value=1:200),
  dplyr::tibble(geo_value = "al", time_value = d + 1:5, value=-(1:5))
) %>%
  as_epi_df() %>%
  group_by(geo_value)

small_x = dplyr::bind_rows(
  dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value=11:15),
  dplyr::tibble(geo_value = "al", time_value = d + 1:5, value=-(1:5))
) %>%
  as_epi_df(as_of = d + 6) %>%
  group_by(geo_value)


f = function(x, g, t) dplyr::tibble(value=mean(x$value), count=length(x$value))

## --- These cases generate errors (or not): ---
test_that("`before` and `after` are both vectors of length 1", {
  expect_error(epi_slide(grouped, f, before = c(0,1), after = 0, ref_time_values = d+3),
               "`before`.*length-1")
  expect_error(epi_slide(grouped, f, before = 1, after = c(0,1), ref_time_values = d+3),
               "`after`.*length-1")
})

test_that("Test errors/warnings for discouraged features", {
  expect_error(epi_slide(grouped, f, ref_time_values = d+1),
               "Either or both of `before`, `after` must be provided.")
  expect_warning(epi_slide(grouped, f, before = 0L, ref_time_values = d+1),
                 "`before==0`, `after` missing")
  expect_warning(epi_slide(grouped, f, after = 0L, ref_time_values = d+1),
                 "`before` missing, `after==0`")
  # Below cases should raise no errors/warnings:
  expect_warning(epi_slide(grouped, f, before = 1L, ref_time_values = d+2),NA)
  expect_warning(epi_slide(grouped, f, after = 1L, ref_time_values = d+2),NA)
  expect_warning(epi_slide(grouped, f, before = 0L, after = 0L, ref_time_values = d+2),NA)
})

test_that("Both `before` and `after` must be non-NA, non-negative, integer-compatible",{
  expect_error(epi_slide(grouped, f, before = -1L, ref_time_values = d+2L),
               "`before`.*non-negative")
  expect_error(epi_slide(grouped, f, before = 2L, after = -1L, ref_time_values = d+2L),
               "`after`.*non-negative")
  expect_error(epi_slide(grouped, f, before = "a", ref_time_values = d+2L),
               regexp="before", class="vctrs_error_incompatible_type")
  expect_error(epi_slide(grouped, f, before = 1L, after = "a", ref_time_values = d+2L),
               regexp="after", class="vctrs_error_incompatible_type")
  expect_error(epi_slide(grouped, f, before = 0.5, ref_time_values = d+2L),
               regexp="before", class="vctrs_error_incompatible_type")
  expect_error(epi_slide(grouped, f, before = 1L, after = 0.5, ref_time_values = d+2L),
               regexp="after", class="vctrs_error_incompatible_type")
  expect_error(epi_slide(grouped, f, before = NA, after = 1L, ref_time_values = d+2L),
               "`before`.*non-NA")
  expect_error(epi_slide(grouped, f, before = 1L, after = NA, ref_time_values = d+2L),
               "`after`.*non-NA")
  # Non-integer-class but integer-compatible values are allowed:
  expect_error(epi_slide(grouped, f, before = 1, after = 1, ref_time_values = d+2L),NA)
})

test_that("`ref_time_values` + `before` + `after` that result in no slide data, generate the error", {
  expect_error(epi_slide(grouped, f, before=2L, ref_time_values = d), 
               "All `ref_time_values` must appear in `x\\$time_value`.") # before the first, no data in the slide windows
  expect_error(epi_slide(grouped, f, before=2L, ref_time_values = d+207L), 
               "All `ref_time_values` must appear in `x\\$time_value`.") # beyond the last, no data in window
})

test_that("`ref_time_values` + `before` + `after` that have some slide data, but generate the error due to ref. time being out of time range (would also happen if they were in between `time_value`s)", {
  expect_error(epi_slide(grouped, f, before=0L, after=2L, ref_time_values = d), 
               "All `ref_time_values` must appear in `x\\$time_value`.") # before the first, but we'd expect there to be data in the window
  expect_error(epi_slide(grouped, f, before=2L, ref_time_values = d+201L), 
               "All `ref_time_values` must appear in `x\\$time_value`.") # beyond the last, but still with data in window
})

## --- These cases generate warnings (or not): ---
test_that("Warn user against having a blank `before`",{
  expect_warning(epi_slide(grouped, f, after = 1L,
                           ref_time_values = d+1L), NA)
  expect_warning(epi_slide(grouped, f, before = 0L, after = 1L,
                           ref_time_values = d+1L), NA)
})

## --- These cases doesn't generate the error: ---
test_that("these doesn't produce an error; the error appears only if the ref time values are out of the range for every group", {
  expect_identical(epi_slide(grouped, f, before=2L, ref_time_values = d+200L) %>% 
                     ungroup() %>%
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = "ak", slide_value_value = 199)) # out of range for one group
  expect_identical(epi_slide(grouped, f, before=2L, ref_time_values=d+3) %>% 
                     ungroup() %>%
                     dplyr::select("geo_value","slide_value_value"), 
                   dplyr::tibble(geo_value = c("ak", "al"), slide_value_value = c(2, -2))) # not out of range for either group
})

test_that("epi_slide alerts if the provided f doesn't take enough args", {
  f_xgt = function(x, g, t) dplyr::tibble(value=mean(x$value), count=length(x$value))
  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_error(epi_slide(grouped, f_xgt, before = 1L, ref_time_values = d+1), regexp = NA)
  expect_warning(epi_slide(grouped, f_xgt, before = 1L, ref_time_values = d+1), regexp = NA)

  f_x_dots = function(x, ...) dplyr::tibble(value=mean(x$value), count=length(x$value))
  expect_warning(epi_slide(grouped, f_x_dots, before = 1L, ref_time_values = d+1),
    regexp = "positional arguments before the `...` args",
    class = "check_sufficient_f_args__f_needs_min_args_before_dots")
})

test_that("basic grouped epi_slide computation produces expected output", {
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=cumsum(11:15)),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value=cumsum(-(1:5)))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)
  
  # formula
  result1 <- epi_slide(small_x, f = ~sum(.x$value), before=50)
  expect_identical(result1, expected_output)

  # function
  result2 <- epi_slide(small_x, f = function(x, g, t) sum(x$value), before=50)
  expect_identical(result2, expected_output)

  # dots
  result3 <- epi_slide(small_x, slide_value = sum(value), before=50)
  expect_identical(result3, expected_output)
})

test_that("ungrouped epi_slide computation completes successfully", {
  expect_error(
    small_x %>%
    ungroup() %>%
    epi_slide(before = 2,
      slide_value = sum(.x$value)),
    regexp=NA
  )
})

test_that("basic ungrouped epi_slide computation produces expected output", {
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=cumsum(11:15))
  ) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    ungroup() %>%
    filter(geo_value == "ak") %>%
    epi_slide(before = 50,
              slide_value = sum(.x$value))
  expect_identical(result1, expected_output)
})

test_that("epi_slide computation via formula can use ref_time_value", {
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=as.double(d + 1:5)),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value=as.double(d + 1:5))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(f = ~ .ref_time_value,
               before = 50)

  expect_identical(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(f = ~ .z,
               before = 50)

  expect_identical(result2, expected_output)

  result3 <- small_x %>%
    epi_slide(f = ~ ..3,
               before = 50)

  expect_identical(result3, expected_output)
})

test_that("epi_slide computation via function can use ref_time_value", {
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=as.double(d + 1:5)),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value=as.double(d + 1:5))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(f = function(x, g, t) t,
               before = 2)

  expect_identical(result1, expected_output)
})

test_that("epi_slide computation via dots can use ref_time_value and group", {
  # ref_time_value
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=as.double(d + 1:5)),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value=as.double(d + 1:5))
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(before = 50,
      slide_value = .ref_time_value)

  expect_identical(result1, expected_output)

  result2 <- small_x %>%
    epi_slide(before = 50,
      slide_value = .env$.ref_time_value)

  expect_identical(result2, expected_output)

  # group_key
  # Use group_key column
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value="ak"),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value="al")
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result3 <- small_x %>%
    epi_slide(before = 2,
      slide_value = .group_key$geo_value)

  expect_identical(result3, expected_output)

  # Use entire group_key object
  expected_output = dplyr::bind_rows(
    dplyr::tibble(geo_value = "ak", time_value = d + 1:5, value = 11:15, slide_value=1L),
    dplyr::tibble(geo_value = "al", time_value = d + 1:5, value = -(1:5), slide_value=1L)
  ) %>%
    group_by(geo_value) %>%
    as_epi_df(as_of = d + 6)

  result4 <- small_x %>%
    epi_slide(before = 2,
        slide_value = nrow(.group_key))

  expect_identical(result4, expected_output)
})

test_that("epi_slide computation via dots outputs the same result using col names and the data var", {
  expected_output <- small_x %>%
    epi_slide(before = 2,
      slide_value = max(time_value)) %>%
    as_epi_df(as_of = d + 6)

  result1 <- small_x %>%
    epi_slide(before = 2,
      slide_value = max(.x$time_value))

  expect_identical(result1, expected_output)
})
