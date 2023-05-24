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

  # function interface
  xx4 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = function(x, g) {
      tibble::tibble(sum_binary = sum(x$binary))
    }, before = 2, names_sep = NULL)
  
  expect_identical(xx1,xx4)

  # tidyeval interface
  xx5 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(sum_binary = sum(binary),
               before = 2)
  
  expect_identical(xx1,xx5)
})

test_that("epix_slide works as intended with `as_list_col=TRUE`",{
  xx_dfrow1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ data.frame(bin_sum = sum(.x$binary)),
               before = 2,
               as_list_col = TRUE)
  
  xx_dfrow2 <- tibble(
    geo_value = rep("x",4),
    time_value = c(4,5,6,7),
    slide_value =
      c(2^3+2^2,
        2^6+2^3,
        2^10+2^9,
        2^15+2^14) %>%
      purrr::map(~ data.frame(bin_sum = .x))
  ) %>%
    group_by(geo_value)
  
  expect_identical(xx_dfrow1,xx_dfrow2) # *
  
  xx_dfrow3 <- (
    xx
    $group_by(dplyr::across(dplyr::all_of("geo_value")))
    $slide(f = ~ data.frame(bin_sum = sum(.x$binary)),
           before = 2,
           as_list_col = TRUE)
  )
  
  expect_identical(xx_dfrow1,xx_dfrow3) # This and * Imply xx_dfrow2 and xx_dfrow3 are identical
  
  xx_df1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ data.frame(bin = .x$binary),
               before = 2,
               as_list_col = TRUE)
  
  xx_df2 <- tibble(
    geo_value = rep("x",4),
    time_value = c(4,5,6,7),
    slide_value =
      list(c(2^3,2^2),
           c(2^6,2^3),
           c(2^10,2^9),
           c(2^15,2^14)) %>%
      purrr::map(~ data.frame(bin = rev(.x)))
  ) %>%
    group_by(geo_value)
  
  expect_identical(xx_df1,xx_df2)

  xx_scalar1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ sum(.x$binary),
               before = 2,
               as_list_col = TRUE)
  
  xx_scalar2 <- tibble(
    geo_value = rep("x",4),
    time_value = c(4,5,6,7),
    slide_value =
      list(2^3+2^2,
           2^6+2^3,
           2^10+2^9,
           2^15+2^14)
  ) %>%
    group_by(geo_value)
  
  expect_identical(xx_scalar1,xx_scalar2)
  
  xx_vec1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(f = ~ .x$binary,
               before = 2,
               as_list_col = TRUE)
  
  xx_vec2 <- tibble(
    geo_value = rep("x",4),
    time_value = c(4,5,6,7),
    slide_value = 
      list(c(2^3,2^2),
           c(2^6,2^3),
           c(2^10,2^9),
           c(2^15,2^14)) %>%
      purrr::map(rev)
  ) %>%
    group_by(geo_value)
  
  expect_identical(xx_vec1,xx_vec2)
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
                             2,         1:1, 2^(1:1),
                             3,         1:2, 2^(2:1),
                             4,         1:3, 2^(3:1),
                             5,         1:4, 2^(4:1),
                             6,         1:5, 2^(5:1),
                             7,         1:6, 2^(6:1)) %>%
  tidyr::unnest(c(time_value,binary)) %>%
  mutate(geo_value = "x") %>%
  as_epi_archive()

test_that("epix_slide with all_versions option has access to all older versions", {
  library(data.table)
  # Make sure we're using testthat edition 3, where `expect_identical` doesn't
  # actually mean `base::identical` but something more content-based using
  # `waldo` package:
  testthat::local_edition(3)

  slide_fn <- function(x, g) {
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

  # formula interface
  result4 <- ea %>% group_by() %>%
    epix_slide(f = ~ slide_fn(.x, .y),
               before = 10^3,
               names_sep = NULL,
               all_versions = TRUE)

  expect_identical(result1,result4) # This and * Imply result2 and result4 are identical

  # tidyeval interface
  result5 <- ea %>%
    group_by() %>%
    epix_slide(data = slide_fn(
      .data$clone(), # hack to convert from pronoun back to archive
      stop("slide_fn doesn't use group key, no need to prepare it")
    ),
    before = 10^3,
    names_sep = NULL,
    all_versions = TRUE)

  expect_identical(result1,result5) # This and * Imply result2 and result5 are identical

  expect_identical(ea, ea_orig_mirror) # We shouldn't have mutated ea
})

test_that("as_of and epix_slide with long enough window are compatible", {
  library(data.table)
  testthat::local_edition(3)

  # For all_versions = FALSE:

  f1 = function(x, g) {
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

  f2 = function(x, g) {
    x %>%
      # extract time&version-lag-1 data:
      epix_slide(
        function(subx, subg) {
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

test_that("epix_slide `f` is passed an ungrouped `epi_archive` when `all_versions=TRUE`",{
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

# XXX currently, we're using a stopgap measure of having `epix_slide` always
# output a (grouped/ungrouped) tibble while we think about the class, columns,
# and attributes of `epix_slide` output more carefully. We might bring this test
# back depending on the decisions there:
#
# test_that("`epix_slide` uses `versions_end` as a resulting `epi_df`'s `as_of`", {
#   ea_updated_stale = ea$clone()
#   ea_updated_stale$versions_end <- ea_updated_stale$versions_end + 3 # (dbl)
#   #
#   expect_identical(
#     ea_updated_stale %>%
#       group_by(geo_value) %>%
#       epix_slide(~ slice_head(.x, n = 1L), before = 10L) %>%
#       ungroup() %>%
#       attr("metadata") %>%
#       .$as_of,
#     10
#   )
# })

test_that("epix_slide works with 0-row computation outputs", {
  epix_slide_empty = function(ea, ...) {
    ea %>%
      epix_slide(before = 5L, ..., function(x, g) {
        tibble::tibble()
      })
  }
  expect_identical(
    ea %>%
      epix_slide_empty(),
    tibble::tibble(
      time_value = ea$DT$version[integer(0)]
    )
  )
  expect_identical(
    ea %>%
      group_by(geo_value) %>%
      epix_slide_empty(),
    tibble::tibble(
      geo_value = ea$DT$geo_value[integer(0)],
      time_value = ea$DT$version[integer(0)]
    ) %>%
      # new_epi_df(geo_type = ea$geo_type, time_type = ea$time_type,
      #            as_of = ea$versions_end) %>%
    group_by(geo_value)
  )
  # with `all_versions=TRUE`, we have something similar but never get an
  # `epi_df`:
  expect_identical(
    ea %>%
      epix_slide_empty(all_versions=TRUE),
    tibble::tibble(
      time_value = ea$DT$version[integer(0)]
    )
  )
  expect_identical(
    ea %>%
      group_by(geo_value) %>%
      epix_slide_empty(all_versions=TRUE),
    tibble::tibble(
      geo_value = ea$DT$geo_value[integer(0)],
      time_value = ea$DT$version[integer(0)]
    ) %>%
      group_by(geo_value)
  )
})

# test_that("epix_slide grouped by geo can produce `epi_df` output", {
#   # This is a characterization test. Not sure we actually want this behavior;
#   # https://github.com/cmu-delphi/epiprocess/pull/290#issuecomment-1489099157
#   expect_identical(
#     ea %>%
#       group_by(geo_value) %>%
#       epix_slide(before = 5L, function(x,g) {
#         tibble::tibble(value = 42)
#       }, names_sep = NULL),
#     tibble::tibble(
#       geo_value = "x",
#       time_value = epix_slide_ref_time_values_default(ea),
#       value = 42
#     ) %>%
#       new_epi_df(as_of = ea$versions_end)
#   )
# })

test_that("epix_slide alerts if the provided f doesn't take enough args", {
  f_xg = function(x, g) dplyr::tibble(value=mean(x$binary), count=length(x$binary))
  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_error(epix_slide(xx, f = f_xg, before = 2L), regexp = NA)
  expect_warning(epix_slide(xx, f = f_xg, before = 2L), regexp = NA)

  f_x_dots = function(x, ...) dplyr::tibble(value=mean(x$binary), count=length(x$binary))
  expect_warning(epix_slide(xx, f_x_dots, before = 2L),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots")
})
