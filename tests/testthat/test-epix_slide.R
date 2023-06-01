library(dplyr)

test_that("epix_slide only works on an epi_archive", {
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

test_that("epix_slide works as intended", {
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
  
  expect_identical(xx1,xx3) # This and * imply xx2 and xx3 are identical
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

test_that("epix_slide with all_versions option has access to all older versions", {
  library(data.table)
  # Make sure we're using testthat edition 3, where `expect_identical` doesn't
  # actually mean `base::identical` but something more content-based using
  # `waldo` package:
  testthat::local_edition(3)

  slide_fn <- function(x, gk, rtv) {
    return(tibble(n_versions = length(unique(x$DT$version)),
                  n_row = nrow(x$DT),
                  dt_class1 = class(x$DT)[[1L]],
                  dt_key = list(key(x$DT))))
  }

  ea_orig_mirror = ea$clone(deep=TRUE)
  ea_orig_mirror$DT <- copy(ea_orig_mirror$DT)

  result1 <- ea %>% group_by() %>%
    epix_slide(f = slide_fn,
               before = 10^3,
               names_sep = NULL,
               all_versions = TRUE)

  expect_true(inherits(result1, "tbl_df"))

  result2 <- tibble::tribble(
      ~time_value, ~n_versions,   ~n_row,    ~dt_class1,    ~dt_key,
             2,        1L,      sum(1:1),  "data.table", key(ea$DT),
             3,        2L,      sum(1:2),  "data.table", key(ea$DT),
             4,        3L,      sum(1:3),  "data.table", key(ea$DT),
             5,        4L,      sum(1:4),  "data.table", key(ea$DT),
             6,        5L,      sum(1:5),  "data.table", key(ea$DT),
             7,        6L,      sum(1:6),  "data.table", key(ea$DT),
    )

  expect_identical(result1,result2) # *

  result3 <- (
    ea
    $group_by()
    $slide(f = slide_fn,
               before = 10^3,
               names_sep = NULL,
               all_versions = TRUE)
  )

  expect_identical(result1,result3) # This and * Imply result2 and result3 are identical

  expect_identical(ea, ea_orig_mirror) # We shouldn't have mutated ea
})

test_that("as_of and epix_slide with long enough window are compatible", {
  library(data.table)
  testthat::local_edition(3)

  # For all_versions = FALSE:

  f1 = function(x, gk, rtv) {
    tibble(
      diff_mean = mean(diff(x$binary))
    )
  }
  ref_time_value1 = 5

  expect_identical(
    ea$as_of(ref_time_value1) %>% f1() %>% mutate(time_value = ref_time_value1, .before=1L),
    ea$slide(f1, before=1000L, ref_time_values=ref_time_value1, names_sep=NULL)
  )

  # For all_versions = TRUE:

  f2 = function(x, gk, rtv) {
    x %>%
      # extract time&version-lag-1 data:
      epix_slide(
        function(subx, subgk, rtv) {
          tibble(data = list(
            subx %>%
              filter(time_value == attr(subx, "metadata")$as_of - 1) %>%
              rename(real_time_value = time_value, lag1 = binary)
          ))
        }, before = 1, names_sep = NULL
      ) %>%
      # assess as nowcast:
      unnest(data) %>%
      inner_join(x$as_of(x$versions_end), by = setdiff(key(x$DT), c("version"))) %>%
      summarize(mean_abs_delta = mean(abs(binary - lag1)))
  }
  ref_time_value2 = 5

  expect_identical(
    ea$as_of(ref_time_value2, all_versions=TRUE) %>% f2() %>% mutate(time_value = ref_time_value2, .before=1L),
    ea$slide(f2, before=1000L, ref_time_values=ref_time_value2, all_versions=TRUE, names_sep=NULL)
  )

  # Test the same sort of thing when grouping by geo in an archive with multiple geos.
  ea_multigeo = ea$clone()
  ea_multigeo$DT <- rbind(ea_multigeo$DT,
                          copy(ea_multigeo$DT)[,geo_value:="y"][,binary:=-binary][])
  setkeyv(ea_multigeo$DT, key(ea$DT))

  expect_identical(
    ea_multigeo %>%
      group_by(geo_value) %>%
      epix_slide(f2, before=1000L, ref_time_values=ref_time_value2, all_versions=TRUE, names_sep=NULL) %>%
      filter(geo_value == "x"),
    ea %>% # using `ea` here is like filtering `ea_multigeo` to `geo_value=="x"`
      epix_as_of(ref_time_value2, all_versions=TRUE) %>%
      f2() %>%
      transmute(geo_value = "x", time_value = ref_time_value2, mean_abs_delta) %>%
      group_by(geo_value)
  )
})

test_that("epix_slide `f` is passed an ungrouped `epi_archive`", {
  slide_fn <- function(x, gk, rtv) {
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

test_that("epix_slide with all_versions option works as intended", {
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

test_that("epix_slide alerts if the provided f doesn't take enough args", {
  f_xgt = function(x, g, t) dplyr::tibble(value=mean(x$binary), count=length(x$binary))
  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_error(epix_slide(xx, f = f_xgt, before = 2L), regexp = NA)
  expect_warning(epix_slide(xx, f = f_xgt, before = 2L), regexp = NA)

  f_x_dots = function(x, ...) dplyr::tibble(value=mean(x$binary), count=length(x$binary))
  expect_warning(epix_slide(xx, f_x_dots, before = 2L),
    regexp = "positional arguments before the `...` args",
    class = "check_sufficient_f_args__f_needs_min_args_before_dots")
})

test_that("epix_slide computation via formula can use ref_time_value", {
  xx_ref <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                slide_value = c(4,5,6,7)
                ) %>%
    as_epi_df(as_of = 4) %>% # Also a bug (issue #213)
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ .ref_time_value,
               before = 2)

  expect_identical(xx1, xx_ref)

  xx2 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ .z,
               before = 2)

  expect_identical(xx2, xx_ref)

  xx3 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ ..3,
               before = 2)

  expect_identical(xx3, xx_ref)
})

test_that("epix_slide computation via function can use ref_time_value", {
  xx_ref <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                slide_value = c(4,5,6,7)
                ) %>%
    as_epi_df(as_of = 4) %>% # Also a bug (issue #213)
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = function(x, g, t) t,
               before = 2)

  expect_identical(xx1, xx_ref)
})

test_that("epix_slide computation via dots can use ref_time_value and group", {
  # ref_time_value
  xx_ref <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                slide_value = c(4,5,6,7)
                ) %>%
    as_epi_df(as_of = 4) %>% # Also a bug (issue #213)
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(before = 2,
      slide_value = .ref_time_value)

  expect_identical(xx1, xx_ref)

  # group_key
  xx_ref <- tibble(geo_value = rep("x",4),
                time_value = c(4,5,6,7),
                slide_value = "x"
                ) %>%
    as_epi_df(as_of = 4) %>% # Also a bug (issue #213)
    group_by(geo_value)

  # Use group_key column
  xx3 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(before = 2,
      slide_value = .group_key$geo_value)

  expect_identical(xx3, xx_ref)

  # Use entire group_key object
  expect_error(
    xx %>%
      group_by(.data$geo_value) %>%
      epix_slide(before = 2,
        slide_value = nrow(.group_key)),
    NA
  )
})

test_that("epix_slide computation via dots outputs the same result using col names and the data var", {
  xx_ref <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(before = 2,
      sum_binary = sum(time_value))

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(before = 2,
      sum_binary = sum(.x$time_value))

  expect_identical(xx1, xx_ref)
})
