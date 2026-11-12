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

# Sum implementations from each supported package, to exercise both the
# data.table and slider code paths:
sum_fs <- list(frollsum = data.table::frollsum, slide_sum = slider::slide_sum)

test_that("epi_slide and epi_slide_opt sums agree on Inf window for a later-starting geo", {
  # (Both pad with NAs back to the min time_value in the entire edf, making all
  # output values for "c" NA for many `.f`s.)
  for (f_name in names(sum_fs)) {
    expect_equal(
      epi_slide(edf, ~ sum(.x$value), .window_size = Inf),
      epi_slide_opt(edf, value, sum_fs[[f_name]], .window_size = Inf, .new_col_names = "slide_value"),
      info = f_name
    )
  }
})

test_that("epi_slide_opt sums handle Inf window with none of .ref_time_values in a geo's range", {
  for (f_name in names(sum_fs)) {
    f <- sum_fs[[f_name]]
    # Geo "c" time range starts later; try .ref_time_values all preceding its
    # min (should NA pad its slide computation inputs, leading to everything NA
    # for basic sum):
    ref_time_values <- start_date + 0:1
    expect_equal(
      epi_slide(edf, ~ sum(.x$value),
        .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = TRUE
      ),
      epi_slide_opt(edf, value, f,
        .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = TRUE,
        .new_col_names = "slide_value"
      ),
      info = f_name
    )
    # Geo "a" time range ends earlier; test .ref_time_values all following its
    # max (should not have rows):
    ref_time_values <- start_date + 5:6
    for (all_rows in c(FALSE, TRUE)) {
      expect_equal(
        epi_slide(edf, ~ sum(.x$value),
          .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = all_rows
        ),
        epi_slide_opt(edf, value, f,
          .window_size = Inf, .ref_time_values = ref_time_values, .all_rows = all_rows,
          .new_col_names = "slide_value"
        ),
        info = glue::glue("{f_name}, .all_rows = {all_rows}")
      )
    }
  }
})

test_that("epi_slide_opt with type-transforming frollapply FUN handles a geo with nothing to slide over", {
  # Target .ref_time_values where geo "a" has no input, or where geo "c" has no
  # input (for Inf windows, the latter means sliding over NA padding from the
  # edf-wide min time_value, on which `FUN` may output a different type):
  to_string_if_complete <- function(x) if (anyNA(x)) NA else toString(x)
  for (fun_name in c("toString", "to_string_if_complete")) {
    fun <- get(fun_name)
    for (ref_time_values in list(start_date + 5:6, start_date + 0:1)) {
      for (window_size in c(3, Inf)) {
        for (all_rows in c(FALSE, TRUE)) {
          expect_equal(
            epi_slide_opt(edf, value, data.table::frollapply,
              FUN = fun,
              .window_size = window_size, .ref_time_values = ref_time_values, .all_rows = all_rows,
              .new_col_names = "slide_value"
            ),
            epi_slide(edf, ~ fun(.x$value),
              .window_size = window_size, .ref_time_values = ref_time_values, .all_rows = all_rows
            ),
            info = glue::glue(
              "FUN = {fun_name}, .ref_time_values = {toString(ref_time_values)}, ",
              ".window_size = {window_size}, .all_rows = {all_rows}"
            )
          )
        }
      }
    }
  }
  # Really, frollapply still has some issues with its output typing, e.g., with
  # computations yielding bare lists, list_ofs, length > 1 numeric, etc. We
  # should probably see if slider::slide_vec or other slider alternatives are
  # better-behaved.
})

test_that("epi_slide_opt sums handle left-aligned window with no ref_time_values in a geo's range", {
  ref_time_values <- start_date + 0:1
  for (f_name in names(sum_fs)) {
    expect_equal(
      epi_slide(edf, ~ sum(.x$value),
        .window_size = 7, .align = "left", .ref_time_values = ref_time_values
      ),
      epi_slide_opt(edf, value, sum_fs[[f_name]],
        .window_size = 7, .align = "left", .ref_time_values = ref_time_values,
        .new_col_names = "slide_value"
      ),
      info = f_name
    )
  }
})

# Archive tests: all comparing epi_slide_opt.epi_archive vs. (slow) reference
# implementation based on epix_slide + epi_slide_opt; not checking latter's
# behavior.

expect_archive_slide_sum_matches_refimpl <- function(arch, ...) {
  for (f_name in names(sum_fs)) {
    f <- sum_fs[[f_name]]
    opt_result <- epi_slide_opt(arch, value, f, ...)
    ref_result <- epix_slide(arch, function(snapshot, group_key, version) {
      epi_slide_opt(snapshot, value, f, ...)
    }) %>%
      select(names(arch$DT), everything()) %>%
      as_epi_archive()
    testthat::expect_equal(opt_result, ref_result, info = f_name)
  }
}

test_that("epi_slide_opt sums on archive with Inf window match epix_slide + epi_slide_opt on edfs", {
  # Test input revision timeline by version:
  # 1: geo "a" with earlier min time; geo "c" with later min time.
  # 2: new geo "d" introduced (also with later min time).
  # 3: "c" revises a value.
  # 4: "c" adds an earlier time value, decreasing the archive-wide min, which
  #    should change "a"'s outputs even though "a" has no update in version 4.
  arch <- tibble::tribble(
    ~geo_value, ~time_value, ~version, ~value,
    "a", 0:4, 1, c(1, 2, 3, 4, 5),
    "c", 2:6, 1, c(10, 20, 30, 40, 50),
    "d", 4:6, 2, c(100, 200, 300),
    "c", 3L, 3, 25,
    "c", -1L, 4, 5
  ) %>%
    tidyr::unchop(c("time_value", "value")) %>%
    dplyr::mutate(
      time_value = start_date + .data$time_value,
      version = start_date + 10 + .data$version
    ) %>%
    as_epi_archive()
  expect_archive_slide_sum_matches_refimpl(arch, .window_size = Inf)
})

test_that("epi_slide_opt sums on arch with Inf window handle NA revisions same as epix_slide + epi_slide_opt on edfs", {
  # Versions:
  # 1: geo "a" starts at the archive-wide min time; geo "c" starts later.
  # 2: "a" revises its first value to NA (but this doesn't change the min t).
  # 3: "a" revises it back to a non-NA value.
  arch <- tibble::tribble(
    ~geo_value, ~time_value, ~version, ~value,
    "a", 0:4, 1, c(1, 2, 3, 4, 5),
    "c", 2:6, 1, c(10, 20, 30, 40, 50),
    "a", 0L, 2, NA_real_,
    "a", 0L, 3, 7
  ) %>%
    tidyr::unchop(c("time_value", "value")) %>%
    dplyr::mutate(
      time_value = start_date + .data$time_value,
      version = start_date + 10 + .data$version
    ) %>%
    as_epi_archive()
  expect_archive_slide_sum_matches_refimpl(arch, .window_size = Inf)
})

test_that("epi_slide_opt sums on archive with finite windows match epix_slide + epi_slide_opt on edfs", {
  # Versions:
  # 1: geo "a" starts at the archive-wide min time; geo "c" starts later, with
  #    a gap in its time values.
  # 2: "c" fills in its gap; "a" revises a value.
  # 3: "a" adds an earlier time value.
  arch <- tibble::tribble(
    ~geo_value, ~time_value, ~version, ~value,
    "a", 0:6, 1, c(1, 2, 3, 4, 5, 6, 7),
    "c", c(2:3, 5:8), 1, c(10, 20, 40, 50, 60, 70),
    "c", 4L, 2, 30,
    "a", 3L, 2, 40,
    "a", -1L, 3, 0.5
  ) %>%
    tidyr::unchop(c("time_value", "value")) %>%
    dplyr::mutate(
      time_value = start_date + .data$time_value,
      version = start_date + 10 + .data$version
    ) %>%
    as_epi_archive()
  for (align in c("right", "center", "left")) {
    expect_archive_slide_sum_matches_refimpl(arch, .window_size = 3, .align = align)
  }
})

test_that("epi_slide_opt.epi_archive with type-transforming frollapply FUN matches epix_slide+epi_slide_opt on edfs", {
  # This is just a basic sanity check. The real tests would come in if we
  # allowed a `.ref_time_values` parameter etc.
  arch <- tibble::tribble(
    ~geo_value, ~time_value, ~version, ~value,
    "a", 0:4, 1, c(1, 2, 3, 4, 5),
    "c", 2:6, 1, c(10, 20, 30, 40, 50),
    "d", 4:5, 2, c(100, 200),
    "c", 3L, 3, 25
  ) %>%
    tidyr::unchop(c("time_value", "value")) %>%
    dplyr::mutate(
      time_value = start_date + .data$time_value,
      version = start_date + 10 + .data$version
    ) %>%
    as_epi_archive()
  opt_result <- epi_slide_opt(arch, value, data.table::frollapply, FUN = toString, .window_size = 3)
  ref_result <- epix_slide(arch, function(snapshot, group_key, version) {
    epi_slide_opt(snapshot, value, data.table::frollapply, FUN = toString, .window_size = 3)
  }) %>%
    select(names(arch$DT), everything()) %>%
    as_epi_archive()
  expect_type(opt_result$DT$value_3dslide, "character")
  expect_equal(opt_result, ref_result)
})

test_that("epi_slide_opt on a 0-row archive gives a 0-row archive with output columns", {
  arch <- tibble::tibble(
    geo_value = character(),
    time_value = as.Date(character()),
    version = as.Date(character()),
    value = numeric()
  ) %>%
    as_epi_archive(versions_end = start_date + 10, clobberable_versions_start = NA)
  for (window_size in list(3, Inf)) {
    for (f in list(data.table::frollsum, slider::slide_sum)) {
      arch_out <- epi_slide_opt(arch, value, f, .window_size = window_size, .new_col_names = "slide_value")
      expect_identical(names(arch_out$DT), c(names(arch$DT), "slide_value"))
      expect_identical(nrow(arch_out$DT), 0L)
      expect_type(arch_out$DT$slide_value, "double")
      expect_identical(arch_out$versions_end, arch$versions_end)
      expect_identical(arch_out$clobberable_versions_start, arch$clobberable_versions_start)
    }
  }
  # `frollapply`'s output type depends on its `FUN`, so we can't know it:
  expect_error(
    epi_slide_opt(arch, value, data.table::frollapply,
      FUN = function(x) paste(x, collapse = "+"),
      .window_size = 3, .new_col_names = "slide_value"
    ),
    class = "epiprocess__epi_slide_opt__frollapply_0_rows"
  )
  # Input should not be modified:
  expect_identical(names(arch$DT), c("geo_value", "time_value", "version", "value"))
})
