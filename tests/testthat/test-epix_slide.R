library(dplyr)

test_that("epix_slide only works on an epi_archive",{
  expect_error(epix_slide(data.frame(x=1)))
})

x <- tibble::tribble(~version, ~time_value, ~binary,
                     4,       c(1:3),       2^(1:3),
                     5,       c(1:2,4),     2^(4:6),
                     6,       c(1:2,4:5),   2^(7:10),
                     7,       2:6,          2^(11:15)) %>%
  tidyr::unnest(c(time_value,binary))

xx <- bind_cols(geo_value = rep("x",15), x) %>%
  as_epi_archive()

test_that("epix_slide works as intended",{
  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ sum(.x$binary),
               before = 2,
               new_col_name = "sum_binary")
  
  xx2 <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                sum_binary = c(2^3+2^2,
                               2^6+2^3,
                               2^10+2^9,
                               2^15+2^14)) %>%
    as_epi_df(as_of = 4) %>% # Also a bug (issue #213)
    group_by(geo_value)
  
  expect_identical(xx1,xx2) # *
  
  xx3 <- (
    xx
    $group_by(dplyr::across(dplyr::all_of("geo_value")))
    $slide(f = ~ sum(.x$binary),
           before = 2,
           new_col_name = 'sum_binary')
  )
  
  expect_identical(xx1,xx3) # This and * Imply xx2 and xx3 are identical
})

test_that("epix_slide `before` validation works", {
  expect_error(xx$slide(f = ~ sum(.x$binary)),
               "`before` is required")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=NA),
               "`before`.*NA")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=-1),
               "`before`.*negative")
  expect_error(xx$slide(f = ~ sum(.x$binary), before=1.5),
               regexp="before",
               class="vctrs_error_incompatible_type")
  # We might want to allow this at some point (issue #219):
  expect_error(xx$slide(f = ~ sum(.x$binary), before=Inf),
               regexp="before",
               class="vctrs_error_incompatible_type")
  # (wrapper shouldn't introduce a value:)
  expect_error(epix_slide(xx, f = ~ sum(.x$binary)), "`before` is required")
  # These `before` values should be accepted:
  expect_error(xx$slide(f = ~ sum(.x$binary), before=0),
               NA)
  expect_error(xx$slide(f = ~ sum(.x$binary), before=2L),
               NA)
  expect_error(xx$slide(f = ~ sum(.x$binary), before=365000),
               NA)
})

test_that("quosure passing issue in epix_slide is resolved + other potential issues", {
  # (First part adapted from @examples)
  time_values <- seq(as.Date("2020-06-01"),
                     as.Date("2020-06-02"),
                     by = "1 day")
  # We only have one non-version, non-time key in the example archive. Add
  # another so that we don't accidentally pass tests due to accidentally
  # matching the default grouping.
  ea = as_epi_archive(archive_cases_dv_subset$DT %>%
                        dplyr::mutate(modulus = seq_len(nrow(.)) %% 5L),
                      other_keys = "modulus",
                      compactify = TRUE)
  reference_by_modulus = ea %>%
    group_by(modulus) %>%
    epix_slide(f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av')
  reference_by_neither = ea %>%
    group_by() %>%
    epix_slide(f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av')
  # test the passing-something-that-must-be-enquosed behavior:
  #
  # (S3 group_by behavior for this case is the `reference_by_modulus`)
  expect_identical(
    ea$group_by(modulus)$slide(
      f = ~ mean(.x$case_rate_7d_av),
      before = 2,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the .data pronoun behavior:
  expect_identical(
    epix_slide(x = ea %>% group_by(.data$modulus),
               f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(.data$modulus)$slide(
      f = ~ mean(.x$case_rate_7d_av),
      before = 2,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the passing across-all-of-string-literal behavior:
  expect_identical(
    epix_slide(x = ea %>% group_by(dplyr::across(all_of("modulus"))),
               f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(across(all_of("modulus")))$slide(
      f = ~ mean(.x$case_rate_7d_av),
      before = 2,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the passing-across-all-of-string-var behavior:
  my_group_by = "modulus"
  expect_identical(
    epix_slide(x = ea %>% group_by(dplyr::across(tidyselect::all_of(my_group_by))),
               f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_modulus
  )
  expect_identical(
    ea$group_by(dplyr::across(tidyselect::all_of(my_group_by)))$slide(
      f = ~ mean(.x$case_rate_7d_av),
      before = 2,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_modulus
  )
  # test the default behavior (default in this case should just be grouping by neither):
  expect_identical(
    epix_slide(x = ea,
               f = ~ mean(.x$case_rate_7d_av),
               before = 2,
               ref_time_values = time_values,
               new_col_name = 'case_rate_3d_av'),
    reference_by_neither
  )
  expect_identical(
    ea$slide(
      f = ~ mean(.x$case_rate_7d_av),
      before = 2,
      ref_time_values = time_values,
      new_col_name = 'case_rate_3d_av'
    ),
    reference_by_neither
  )
})

ea <- tibble::tribble(~version, ~time_value, ~binary,
                     2,        1:1,          2^(1:1),
                     3,        1:2,          2^(2:1),
                     4,        1:3,          2^(3:1),
                     5,        1:4,          2^(4:1),
                     6,        1:5,          2^(5:1),
                     7,        1:6,          2^(6:1)) %>%
  tidyr::unnest(c(time_value,binary))

ea$geo_value <- "x"
ea <- as_epi_archive(ea)

test_that("epix_slide with all_versions option has access to all older versions",{
  slide_fn <- function(x, g) {
    return(data.frame(n_versions = length(unique(x$DT$version)), n_row = nrow(x$DT)))
  }

  ea1 <- ea %>% group_by() %>%
    epix_slide(f = slide_fn,
               before = 10^3,
               new_col_name = "out",
               all_versions = TRUE)

  expect_true(inherits(ea1, "tbl_df"))

  ea2 <- tibble::tribble(
      ~time_value, ~out_n_versions, ~out_n_row,
             2,        1L,            sum(1:1),
             3,        2L,            sum(1:2),
             4,        3L,            sum(1:3),
             5,        4L,            sum(1:4),
             6,        5L,            sum(1:5),
             7,        6L,            sum(1:6)
    )

  expect_identical(ea1,ea2) # *

  ea3 <- (
    ea
    $group_by()
    $slide(f = slide_fn,
               before = 10^3,
               new_col_name = "out",
               all_versions = TRUE)
  )

  expect_identical(ea1,ea3) # This and * Imply ea2 and ea3 are identical
})

test_that("epix_slide `f` is passed an ungrouped `epi_archive`",{
  slide_fn <- function(x, g) {
    expect_true(is_epi_archive(x))
    return(NA)
  }

  ea %>% group_by() %>%
    epix_slide(f = slide_fn,
               before = 1,
               ref_time_values = 5,
               new_col_name = "out",
               all_versions = TRUE)
})

test_that("epix_slide with all_versions option works as intended",{
  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ sum(.x$DT$binary),
               before = 2,
               new_col_name = "sum_binary",
               all_versions = TRUE)

  xx2 <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                sum_binary = c(2^3+2^2,
                               2^6+2^3,
                               2^10+2^9+2^6,
                               2^15+2^14+2^10)) %>%
    group_by(geo_value)

  expect_identical(xx1,xx2) # *

  xx3 <- (
    xx
    $group_by(dplyr::across(dplyr::all_of("geo_value")))
    $slide(f = ~ sum(.x$DT$binary),
           before = 2,
           new_col_name = 'sum_binary',
           all_versions = TRUE)
  )

  expect_identical(xx1,xx3) # This and * Imply xx2 and xx3 are identical
})
