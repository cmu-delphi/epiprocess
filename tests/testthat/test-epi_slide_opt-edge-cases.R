# Some epi_slide vs. epi_slide_opt edge cases that failed under new
# epi_slide_opt implementation during development. Isolated to a separate test
# file for quicker iteration.

start_date <- as.Date("2020-01-01")
edf <- tibble::tibble(
  geo_value = rep(c("a", "c"), each = 5),
  time_value = start_date + c(0:4, 2:6),
  value = c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)
) %>%
  as_epi_df(as_of = start_date + 30) %>%
  group_epi_df(exclude = "time_value")

test_that("epi_slide and epi_slide_sum agree on Inf window for a later-starting geo", {
  # (currently, padding with NAs back to min time_value in entire edf, making
  # all output values for "c" also NA for many `.f`s)
  expect_equal(
    epi_slide(edf, ~ sum(.x$value), .window_size = Inf),
    epi_slide_sum(edf, value, .window_size = Inf, .new_col_names = "slide_value")
  )
})

test_that("epi_slide_sum handles Inf window with none of .ref_time_values in a geo's range", {
  ref_time_values <- start_date + 0:1
  expect_equal(
    epi_slide(edf, ~ sum(.x$value),
      .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = TRUE
    ),
    epi_slide_sum(edf, value,
      .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = TRUE,
      .new_col_names = "slide_value"
    )
  )
})

test_that("epi_slide_sum handles left-aligned window with no ref_time_values in a geo's range", {
  ref_time_values <- start_date + 0:1
  expect_equal(
    epi_slide(edf, ~ sum(.x$value),
      .window_size = 7, .align = "left", .ref_time_values = ref_time_values
    ),
    epi_slide_sum(edf, value,
      .window_size = 7, .align = "left", .ref_time_values = ref_time_values,
      .new_col_names = "slide_value"
    )
  )
})
