suppressPackageStartupMessages(library(dplyr))

test_date <- as.Date("2020-01-01")

test_that("epix_slide only works on an epi_archive", {
  expect_error(epix_slide(data.frame(x = 1)))
})

x <- tibble::tribble(
  ~version, ~time_value, ~binary,
  test_date + 4, test_date + c(1:3), 2^(1:3),
  test_date + 5, test_date + c(1:2, 4), 2^(4:6),
  test_date + 6, test_date + c(1:2, 4:5), 2^(7:10),
  test_date + 7, test_date + 2:6, 2^(11:15)
) %>%
  tidyr::unchop(c(time_value, binary))

xx <- bind_cols(geo_value = rep("ak", 15), x) %>%
  as_epi_archive()

test_that("epix_slide works as intended", {
  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ sum(.x$binary),
      .before = 2,
      .new_col_name = "sum_binary"
    )

  xx2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    sum_binary = c(
      2^3 + 2^2,
      2^6 + 2^3,
      2^10 + 2^9,
      2^15 + 2^14
    )
  ) %>%
    group_by(geo_value)

  expect_identical(xx1, xx2) # *

  xx3 <- xx %>%
    group_by(dplyr::across(dplyr::all_of("geo_value"))) %>%
    epix_slide(
      .f = ~ sum(.x$binary),
      .before = 2,
      .new_col_name = "sum_binary"
    )

  expect_identical(xx1, xx3) # This and * imply xx2 and xx3 are identical

  # function interface
  xx4 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(.f = function(x, gk, rtv) {
      tibble::tibble(sum_binary = sum(x$binary))
    }, .before = 2)

  expect_identical(xx1, xx4)

  # tidyeval interface
  xx5 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      sum_binary = sum(binary),
      .before = 2
    )

  expect_identical(xx1, xx5)
})

test_that("epix_slide works as intended with list cols", {
  xx_dfrow1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ list(data.frame(bin_sum = sum(.x$binary))),
      .before = 2
    )
  xx_dfrow2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value =
      c(
        2^3 + 2^2,
        2^6 + 2^3,
        2^10 + 2^9,
        2^15 + 2^14
      ) %>% purrr::map(~ data.frame(bin_sum = .x))
  ) %>%
    group_by(geo_value)
  expect_identical(xx_dfrow1, xx_dfrow2) # *

  xx_dfrow3 <- xx %>%
    group_by(dplyr::across(dplyr::all_of("geo_value"))) %>%
    epix_slide(
      .f = ~ list(data.frame(bin_sum = sum(.x$binary))),
      .before = 2
    )
  expect_identical(xx_dfrow1, xx_dfrow3) # This and * Imply xx_dfrow2 and xx_dfrow3 are identical

  xx_df1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ list(data.frame(bin = .x$binary)),
      .before = 2
    )
  xx_df2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value =
      list(
        c(2^3, 2^2),
        c(2^6, 2^3),
        c(2^10, 2^9),
        c(2^15, 2^14)
      ) %>% purrr::map(~ data.frame(bin = rev(.x)))
  ) %>%
    group_by(geo_value)
  expect_identical(xx_df1, xx_df2)

  xx_scalar1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ list(sum(.x$binary)),
      .before = 2
    )
  xx_scalar2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value =
      list(
        2^3 + 2^2,
        2^6 + 2^3,
        2^10 + 2^9,
        2^15 + 2^14
      )
  ) %>%
    group_by(geo_value)
  expect_identical(xx_scalar1, xx_scalar2)

  xx_vec1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ list(.x$binary),
      .before = 2
    )
  xx_vec2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value =
      list(
        c(2^3, 2^2),
        c(2^6, 2^3),
        c(2^10, 2^9),
        c(2^15, 2^14)
      ) %>% purrr::map(rev)
  ) %>%
    group_by(geo_value)
  expect_identical(xx_vec1, xx_vec2)
})

test_that("epix_slide `.before` validation works", {
  expect_error(
    xx %>% epix_slide(.f = ~ sum(.x$binary), .before = NA),
    class = "epiprocess__validate_slide_window_arg"
  )
  expect_error(
    xx %>% epix_slide(.f = ~ sum(.x$binary), .before = -1),
    class = "epiprocess__validate_slide_window_arg"
  )
  expect_error(
    xx %>% epix_slide(.f = ~ sum(.x$binary), .before = 1.5),
    class = "epiprocess__validate_slide_window_arg"
  )
  # These `before` values should be accepted:
  expect_no_error(xx %>% epix_slide(.f = ~ sum(.x$binary), .before = 0))
  expect_no_error(xx %>% epix_slide(.f = ~ sum(.x$binary), .before = 2))
  expect_no_error(xx %>% epix_slide(.f = ~ sum(.x$binary), .before = as.difftime(365000, units = "days")))
})

test_that("quosure passing issue in epix_slide is resolved + other potential issues", {
  # (First part adapted from @examples)
  versions <- seq(as.Date("2020-06-01"),
    as.Date("2020-06-02"),
    by = "1 day"
  )
  # We only have one non-version, non-time key in the example archive. Add
  # another so that we don't accidentally pass tests due to accidentally
  # matching the default grouping.
  ea <- as_epi_archive(
    archive_cases_dv_subset$DT %>%
      dplyr::mutate(modulus = seq_len(nrow(.)) %% 5L),
    other_keys = "modulus",
    compactify = TRUE
  )
  reference_by_modulus <- ea %>%
    group_by(modulus) %>%
    epix_slide(
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    )
  reference_by_neither <- ea %>%
    group_by() %>%
    epix_slide(
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    )
  # test the passing-something-that-must-be-enquosed behavior:
  #
  # (S3 group_by behavior for this case is the `reference_by_modulus`)
  expect_identical(
    ea %>%
      group_by(modulus) %>%
      epix_slide(
        .f = ~ mean(.x$case_rate_7d_av),
        .before = 2,
        .versions = versions,
        .new_col_name = "case_rate_3d_av"
      ),
    reference_by_modulus
  )
  # test the .data pronoun behavior:
  expect_identical(
    epix_slide(
      .x = ea %>% group_by(.data$modulus),
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    ),
    reference_by_modulus
  )
  expect_identical(
    ea %>%
      group_by(.data$modulus) %>%
      epix_slide(
        .f = ~ mean(.x$case_rate_7d_av),
        .before = 2,
        .versions = versions,
        .new_col_name = "case_rate_3d_av"
      ),
    reference_by_modulus
  )
  # test the passing across-all-of-string-literal behavior:
  expect_identical(
    epix_slide(
      .x = ea %>% group_by(dplyr::across(all_of("modulus"))),
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    ),
    reference_by_modulus
  )
  expect_identical(
    ea %>%
      group_by(across(all_of("modulus"))) %>%
      epix_slide(
        .f = ~ mean(.x$case_rate_7d_av),
        .before = 2,
        .versions = versions,
        .new_col_name = "case_rate_3d_av"
      ),
    reference_by_modulus
  )
  # test the passing-across-all-of-string-var behavior:
  my_group_by <- "modulus"
  expect_identical(
    epix_slide(
      .x = ea %>% group_by(dplyr::across(tidyselect::all_of(my_group_by))),
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    ),
    reference_by_modulus
  )
  expect_identical(
    ea %>%
      group_by(dplyr::across(tidyselect::all_of(my_group_by))) %>%
      epix_slide(
        .f = ~ mean(.x$case_rate_7d_av),
        .before = 2,
        .versions = versions,
        .new_col_name = "case_rate_3d_av"
      ),
    reference_by_modulus
  )
  # test the default behavior (default in this case should just be grouping by neither):
  expect_identical(
    epix_slide(
      .x = ea,
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    ),
    reference_by_neither
  )
  expect_identical(
    ea %>% epix_slide(
      .f = ~ mean(.x$case_rate_7d_av),
      .before = 2,
      .versions = versions,
      .new_col_name = "case_rate_3d_av"
    ),
    reference_by_neither
  )
})

ea <- tibble::tribble(
  ~version, ~time_value, ~binary,
  test_date + 2, test_date + 1:1, 2^(1:1),
  test_date + 3, test_date + 1:2, 2^(2:1),
  test_date + 4, test_date + 1:3, 2^(3:1),
  test_date + 5, test_date + 1:4, 2^(4:1),
  test_date + 6, test_date + 1:5, 2^(5:1),
  test_date + 7, test_date + 1:6, 2^(6:1)
) %>%
  tidyr::unchop(c(time_value, binary)) %>%
  mutate(geo_value = "ak") %>%
  as_epi_archive()

test_that("epix_slide with .all_versions option has access to all older versions", {
  slide_fn <- function(x, gk, rtv) {
    return(tibble(
      n_versions = length(unique(x$DT$version)),
      n_row = nrow(x$DT),
      dt_class1 = class(x$DT)[[1L]],
      dt_key = list(key(x$DT))
    ))
  }

  ea_orig_mirror <- ea %>% clone()

  result1 <- ea %>%
    group_by() %>%
    epix_slide(
      .f = slide_fn,
      .before = 10^3,
      .all_versions = TRUE
    )

  expect_true(inherits(result1, "tbl_df"))

  result2 <- tibble::tribble(
    ~version, ~n_versions, ~n_row, ~dt_class1, ~dt_key,
    test_date + 2, 1L, sum(1:1), "data.table", key(ea$DT),
    test_date + 3, 2L, sum(1:2), "data.table", key(ea$DT),
    test_date + 4, 3L, sum(1:3), "data.table", key(ea$DT),
    test_date + 5, 4L, sum(1:4), "data.table", key(ea$DT),
    test_date + 6, 5L, sum(1:5), "data.table", key(ea$DT),
    test_date + 7, 6L, sum(1:6), "data.table", key(ea$DT),
  )

  expect_identical(result1, result2) # *

  result3 <- ea %>%
    group_by() %>%
    epix_slide(
      .f = slide_fn,
      .before = 10^3,
      .all_versions = TRUE
    )

  expect_identical(result1, result3) # This and * Imply result2 and result3 are identical

  # formula interface
  result4 <- ea %>%
    group_by() %>%
    epix_slide(
      .f = ~ slide_fn(.x, .y),
      .before = 10^3,
      .all_versions = TRUE
    )

  expect_identical(result1, result4) # This and * Imply result2 and result4 are identical

  # tidyeval interface
  result5 <- ea %>%
    group_by() %>%
    epix_slide(
      # unfortunately, we can't pass this directly as `f` and need an extra comma
      ,
      slide_fn(.x, .group_key, .version),
      .before = 10^3,
      .all_versions = TRUE
    )

  expect_identical(result1, result5) # This and * Imply result2 and result5 are identical
  expect_identical(ea, ea_orig_mirror) # We shouldn't have mutated ea
})

test_that("epix_as_of and epix_slide with long enough window are compatible", {
  # For .all_versions = FALSE:
  f1 <- function(x, gk, rtv) {
    tibble(
      diff_mean = mean(diff(x$binary))
    )
  }
  version1 <- test_date

  expect_identical(
    ea %>% epix_as_of(version1) %>% f1() %>% mutate(version = version1, .before = 1L),
    ea %>% epix_slide(
      f1,
      .before = 1000,
      .versions = version1
    )
  )

  # For .all_versions = TRUE:
  f2 <- function(x, gk, rtv) {
    x %>%
      # extract time&version-lag-1 data:
      epix_slide(
        function(subx, subgk, version) {
          tibble(data = list(
            subx %>%
              filter(time_value == version - 1) %>%
              rename(lag1 = binary)
          ))
        },
        .before = 1
      ) %>%
      # assess as nowcast:
      unnest(data) %>%
      inner_join(
        x %>% epix_as_of(x$versions_end),
        by = setdiff(key(x$DT), c("version"))
      ) %>%
      summarize(mean_abs_delta = mean(abs(binary - lag1)))
  }
  version2 <- test_date + 5

  expect_identical(
    ea %>%
      epix_as_of(version2, all_versions = TRUE) %>%
      f2() %>%
      mutate(version = version2, .before = 1L),
    ea %>% epix_slide(
      f2,
      .before = 1000,
      .versions = version2,
      .all_versions = TRUE
    )
  )

  # Test the same sort of thing when grouping by geo in an archive with multiple geos.
  ea_multigeo <- ea
  ea_multigeo$DT <- rbind(
    ea_multigeo$DT,
    copy(ea_multigeo$DT)[, geo_value := "ak"][, binary := -binary][]
  )
  setkeyv(ea_multigeo$DT, key(ea$DT))

  expect_identical(
    ea_multigeo %>%
      group_by(geo_value) %>%
      epix_slide(
        f2,
        .before = 1000,
        .versions = version2,
        .all_versions = TRUE
      ) %>%
      filter(geo_value == "ak"),
    ea %>% # using `ea` here is like filtering `ea_multigeo` to `geo_value=="x"`
      epix_as_of(version2, all_versions = TRUE) %>%
      f2() %>%
      transmute(geo_value = "ak", version = version2, mean_abs_delta) %>%
      group_by(geo_value)
  )
})

test_that("epix_slide `f` is passed an ungrouped `epi_archive` when `.all_versions=TRUE`", {
  slide_fn <- function(x, gk, rtv) {
    expect_class(x, "epi_archive")
    return(NA)
  }

  ea %>%
    group_by() %>%
    epix_slide(
      .f = slide_fn,
      .before = 1,
      .versions = test_date + 5,
      .new_col_name = "out",
      .all_versions = TRUE
    )
})

test_that("epix_slide with .all_versions option works as intended", {
  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~ sum(.x$DT$binary),
      .before = 2,
      .new_col_name = "sum_binary",
      .all_versions = TRUE
    )

  xx2 <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    sum_binary = c(
      2^3 + 2^2,
      2^6 + 2^3,
      2^10 + 2^9 + 2^6,
      2^15 + 2^14 + 2^10
    )
  ) %>%
    group_by(geo_value)

  expect_identical(xx1, xx2) # *

  xx3 <- xx %>%
    group_by(dplyr::across(dplyr::all_of("geo_value"))) %>%
    epix_slide(
      .f = ~ sum(.x$DT$binary),
      .before = 2,
      .new_col_name = "sum_binary",
      .all_versions = TRUE
    )

  expect_identical(xx1, xx3) # This and * Imply xx2 and xx3 are identical
})

# nolint start: commented_code_linter.
# XXX currently, we're using a stopgap measure of having `epix_slide` always
# output a (grouped/ungrouped) tibble while we think about the class, columns,
# and attributes of `epix_slide` output more carefully. We might bring this test
# back depending on the decisions there:
#
# test_that("`epix_slide` uses `versions_end` as a resulting `epi_df`'s `as_of`", {
#   ea_updated_stale = ea
#   ea_updated_stale$versions_end <- ea_updated_stale$versions_end + 3 # (dbl)
#   #
#   expect_identical(
#     ea_updated_stale %>%
#       group_by(geo_value) %>%
#       epix_slide(~ slice_head(.x, n = 1L), .before = 10L) %>%
#       ungroup() %>%
#       attr("metadata") %>%
#       .$as_of,
#     10
#   )
# })
# nolint end

test_that("epix_slide works with 0-row computation outputs", {
  epix_slide_empty <- function(ea, ...) {
    ea %>%
      epix_slide(.before = 5, ..., function(x, gk, rtv) {
        tibble::tibble()
      })
  }
  expect_identical(
    ea %>%
      epix_slide_empty(),
    tibble::tibble(
      version = ea$DT$version[integer(0)]
    )
  )
  expect_identical(
    ea %>%
      group_by(geo_value) %>%
      epix_slide_empty(),
    tibble::tibble(
      geo_value = ea$DT$geo_value[integer(0)],
      version = ea$DT$version[integer(0)]
    ) %>%
      group_by(geo_value)
  )
  # with `.all_versions=TRUE`, we have something similar but never get an
  # `epi_df`:
  expect_identical(
    ea %>%
      epix_slide_empty(.all_versions = TRUE),
    tibble::tibble(
      version = ea$DT$version[integer(0)]
    )
  )
  expect_identical(
    ea %>%
      group_by(geo_value) %>%
      epix_slide_empty(.all_versions = TRUE),
    tibble::tibble(
      geo_value = ea$DT$geo_value[integer(0)],
      version = ea$DT$version[integer(0)]
    ) %>%
      group_by(geo_value)
  )
})

test_that("epix_slide alerts if the provided f doesn't take enough args", {
  f_xgt <- function(x, g, t) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  # If `regexp` is NA, asserts that there should be no errors/messages.
  expect_error(epix_slide(xx, .f = f_xgt, .before = 2), regexp = NA)
  expect_warning(epix_slide(xx, .f = f_xgt, .before = 2), regexp = NA)

  f_x_dots <- function(x, ...) dplyr::tibble(value = mean(x$binary), count = length(x$binary))
  expect_warning(epix_slide(xx, f_x_dots, .before = 2),
    class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots"
  )
})

test_that("epix_slide computation via formula can use version", {
  xx_ref <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value = test_date + c(4, 5, 6, 7)
  ) %>%
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~.version,
      .before = 2
    )

  expect_identical(xx1, xx_ref)

  xx2 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~.z,
      .before = 2
    )

  expect_identical(xx2, xx_ref)

  xx3 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = ~..3,
      .before = 2
    )

  expect_identical(xx3, xx_ref)
})

test_that("epix_slide computation via function can use version", {
  xx_ref <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value = test_date + c(4, 5, 6, 7)
  ) %>%
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .f = function(x, g, t) t,
      .before = 2
    )

  expect_identical(xx1, xx_ref)
})

test_that("epix_slide computation via dots can use version and group", {
  # version
  xx_ref <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value = test_date + c(4, 5, 6, 7)
  ) %>%
    group_by(geo_value)

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .before = 2,
      slide_value = .version
    )

  expect_identical(xx1, xx_ref)

  # group_key
  xx_ref <- tibble(
    geo_value = rep("ak", 4),
    version = test_date + c(4, 5, 6, 7),
    slide_value = "ak"
  ) %>%
    group_by(geo_value)

  # Use group_key column
  xx3 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .before = 2,
      slide_value = .group_key$geo_value
    )

  expect_identical(xx3, xx_ref)

  # Use entire group_key object
  expect_error(
    xx %>%
      group_by(.data$geo_value) %>%
      epix_slide(
        .before = 2,
        slide_value = nrow(.group_key)
      ),
    NA
  )
})

test_that("epix_slide computation via dots outputs the same result using col names and the data var", {
  xx_ref <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .before = 2,
      sum_binary = sum(binary)
    )

  xx1 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .before = 2,
      sum_binary = sum(.x$binary)
    )

  expect_identical(xx1, xx_ref)

  xx2 <- xx %>%
    group_by(.data$geo_value) %>%
    epix_slide(
      .before = 2,
      sum_binary = sum(.data$binary)
    )

  expect_identical(xx2, xx_ref)
})

test_that("`epix_slide` doesn't decay date output", {
  expect_true(
    xx$DT %>%
      as_tibble() %>%
      as_epi_archive() %>%
      epix_slide(.before = 5, ~ attr(.x, "metadata")$as_of) %>%
      `[[`("slide_value") %>%
      inherits("Date")
  )
})

test_that("`epix_slide` can access objects inside of helper functions", {
  helper <- function(archive_haystack, time_value_needle) {
    archive_haystack %>% epix_slide(has_needle = time_value_needle %in% time_value)
  }
  expect_no_error(helper(archive_cases_dv_subset, as.Date("2021-01-01")))
  expect_no_error(helper(xx, 3L))
})

test_that("`epix_slide` works with .before = Inf", {
  expect_equal(
    xx %>%
      group_by(geo_value) %>%
      epix_slide(sum_binary = sum(binary), .before = Inf) %>%
      pull(sum_binary),
    xx %>%
      group_by(geo_value) %>%
      epix_slide(sum_binary = sum(binary), .before = 365000) %>%
      pull(sum_binary)
  )
})
