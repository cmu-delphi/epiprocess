#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' @param x The `epi_df` object under consideration, [grouped][dplyr::group_by]
#'   or ungrouped. If ungrouped, all data in `x` will be treated as part of a
#'   single data group.
#' @param f Function, formula, or missing; together with `...` specifies the
#'   computation to slide. To "slide" means to apply a computation within a
#'   sliding (a.k.a. "rolling") time window for each data group. The window is
#'   determined by the `before` and `after` parameters described below. One time
#'   step is typically one day or one week; see details for more explanation. If
#'   a function, `f` must take a data frame with the same column names as
#'   the original object, minus any grouping variables, containing the time
#'   window data for one group-`ref_time_value` combination; followed by a
#'   one-row tibble containing the values of the grouping variables for the
#'   associated group; followed by any number of named arguments. If a formula,
#'   `f` can operate directly on columns accessed via `.x$var` or `.$var`, as
#'   in `~mean(.x$var)` to compute a mean of a column `var` for each
#'   `ref_time_value`-group combination. The group key can be accessed via `.y`.
#'   If `f` is missing, then `...` will specify the computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the `...` is interpreted as
#'   an expression for tidy evaluation; in addition to referring to columns
#'   directly by name, the expression has access to `.data` and `.env` pronouns
#'   as in `dplyr` verbs, and can also refer to `.x`, `.group_key`, and
#'   `.ref_time_value`. See details.
#' @param before,after How far `before` and `after` each `ref_time_value` should
#'   the sliding window extend? At least one of these two arguments must be
#'   provided; the other's default will be 0. Any value provided for either
#'   argument must be a single, non-`NA`, non-negative,
#'   [integer-compatible][vctrs::vec_cast] number of time steps. Endpoints of
#'   the window are inclusive. Common settings:
#'   * For trailing/right-aligned windows from `ref_time_value - time_step
#'     (k)` to `ref_time_value`: either pass `before=k` by itself, or pass
#'     `before=k, after=0`.
#'   * For center-aligned windows from `ref_time_value - time_step(k)` to
#'     `ref_time_value + time_step(k)`: pass `before=k, after=k`.
#'   * For leading/left-aligned windows from `ref_time_value` to
#'     `ref_time_value + time_step(k)`: either pass pass `after=k` by itself,
#'     or pass `before=0, after=k`.
#'   See "Details:" about the definition of a time step,(non)treatment of
#'   missing rows within the window, and avoiding warnings about
#'   `before`&`after` settings for a certain uncommon use case.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a non-negative integer and
#'   return an object of class `lubridate::period`. For example, we can use
#'   `time_step = lubridate::hours` in order to set the time step to be one hour
#'   (this would only be meaningful if `time_value` is of class `POSIXct`).
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the slide results be held in a list column, or be
#'   [unchopped][tidyr::unchop]/[unnested][tidyr::unnest]? Default is `FALSE`,
#'   in which case a list object returned by `f` would be unnested (using
#'   [`tidyr::unnest()`]), and, if the slide computations output data frames,
#'   the names of the resulting columns are given by prepending `new_col_name`
#'   to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_rows If `all_rows = TRUE`, then all rows of `x` will be kept in
#'   the output even with `ref_time_values` provided, with some type of missing
#'   value marker for the slide computation output column(s) for `time_value`s
#'   outside `ref_time_values`; otherwise, there will be one row for each row in
#'   `x` that had a `time_value` in `ref_time_values`. Default is `FALSE`. The
#'   missing value marker is the result of `vctrs::vec_cast`ing `NA` to the type
#'   of the slide computation output. If using `as_list_col = TRUE`, note that
#'   the missing marker is a `NULL` entry in the list column; for certain
#'   operations, you might want to replace these `NULL` entries with a different
#'   `NA` marker.
#' @return An `epi_df` object given by appending a new column to `x`, named
#'   according to the `new_col_name` argument.
#'
#' @details To "slide" means to apply a function or formula over a rolling
#'   window of time steps for each data group, where the window is entered at a
#'   reference time and left and right endpoints are given by the `before` and
#'   `after` arguments. The unit (the meaning of one time step) is implicitly
#'   defined by the way the `time_value` column treats addition and subtraction;
#'   for example, if the time values are coded as `Date` objects, then one time
#'   step is one day, since `as.Date("2022-01-01") + 1` equals
#'   `as.Date("2022-01-02")`. Alternatively, the time step can be set explicitly
#'   using the `time_step` argument (which if specified would override the
#'   default choice based on `time_value` column). If there are not enough time
#'   steps available to complete the window at any given reference time, then
#'   `epi_slide()` still attempts to perform the computation anyway (it does not
#'   require a complete window). The issue of what to do with partial
#'   computations (those run on incomplete windows) is therefore left up to the
#'   user, either through the specified function or formula `f`, or through
#'   post-processing. For a centrally-aligned slide of `n` `time_value`s in a
#'   sliding window, set `before = (n-1)/2` and `after = (n-1)/2` when the
#'   number of `time_value`s in a sliding window is odd and `before = n/2-1` and
#'   `after = n/2` when `n` is even.
#'
#'   Sometimes, we want to experiment with various trailing or leading window
#'   widths and compare the slide outputs. In the (uncommon) case where
#'   zero-width windows are considered, manually pass both the `before` and
#'   `after` arguments in order to prevent potential warnings. (E.g., `before=k`
#'   with `k=0` and `after` missing may produce a warning. To avoid warnings,
#'   use `before=k, after=0` instead; otherwise, it looks too much like a
#'   leading window was intended, but the `after` argument was forgotten or
#'   misspelled.)
#'
#'   If `f` is missing, then an expression for tidy evaluation can be specified,
#'   for example, as in:
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), before = 6)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, function(x, g) mean(x$cases), before = 6,
#'             new_col_name = "cases_7dav")
#'   ```
#'   Thus, to be clear, when the computation is specified via an expression for
#'   tidy evaluation (first example, above), then the name for the new column is
#'   inferred from the given expression and overrides any name passed explicitly
#'   through the `new_col_name` argument.
#'
#' @importFrom lubridate days weeks
#' @importFrom dplyr bind_rows group_vars filter select
#' @importFrom rlang .data .env !! enquo enquos sym env missing_arg
#' @export
#' @seealso [`epi_slide_mean`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' # This and other simple sliding means are much faster to do using
#' # the `epi_slide_mean` function instead.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), before = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), after = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), before = 3, after = 3) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 14-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_14dav = mean(cases), before = 6, after = 7) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_14dav) %>%
#'   ungroup()
#'
#' # nested new columns
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(
#'     a = data.frame(
#'       cases_2dav = mean(cases),
#'       cases_2dma = mad(cases)
#'     ),
#'     before = 1, as_list_col = TRUE
#'   ) %>%
#'   ungroup()
epi_slide <- function(x, f, ..., before, after, ref_time_values,
                      time_step,
                      new_col_name = "slide_value", as_list_col = FALSE,
                      names_sep = "_", all_rows = FALSE) {
  assert_class(x, "epi_df")

  if (missing(ref_time_values)) {
    ref_time_values <- unique(x$time_value)
  } else {
    assert_numeric(ref_time_values, min.len = 1L, null.ok = FALSE, any.missing = FALSE)
    if (!test_subset(ref_time_values, unique(x$time_value))) {
      cli_abort(
        "`ref_time_values` must be a unique subset of the time values in `x`."
      )
    }
    if (anyDuplicated(ref_time_values) != 0L) {
      cli_abort("`ref_time_values` must not contain any duplicates; use `unique` if appropriate.")
    }
  }
  ref_time_values <- sort(ref_time_values)

  # Validate and pre-process `before`, `after`:
  if (!missing(before)) {
    before <- vctrs::vec_cast(before, integer())
    assert_int(before, lower = 0, null.ok = FALSE, na.ok = FALSE)
  }
  if (!missing(after)) {
    after <- vctrs::vec_cast(after, integer())
    assert_int(after, lower = 0, null.ok = FALSE, na.ok = FALSE)
  }
  if (missing(before)) {
    if (missing(after)) {
      cli_abort("Either or both of `before`, `after` must be provided.")
    } else if (after == 0L) {
      cli_warn("`before` missing, `after==0`; maybe this was intended to be some
            non-zero-width trailing window, but since `before` appears to be
            missing, it's interpreted as a zero-width window (`before=0,
            after=0`).")
    }
    before <- 0L
  } else if (missing(after)) {
    if (before == 0L) {
      cli_warn("`before==0`, `after` missing; maybe this was intended to be some
            non-zero-width leading window, but since `after` appears to be
            missing, it's interpreted as a zero-width window (`before=0,
            after=0`).")
    }
    after <- 0L
  }

  # If a custom time step is specified, then redefine units
  if (!missing(time_step)) {
    before <- time_step(before)
    after <- time_step(after)
  }

  # Arrange by increasing time_value
  x <- arrange(x, time_value)

  # Now set up starts and stops for sliding/hopping
  starts <- ref_time_values - before
  stops <- ref_time_values + after

  # Symbolize new column name
  new_col <- sym(new_col_name)

  # Computation for one group, all time values
  slide_one_grp <- function(.data_group,
                            .group_key, # see `?group_modify`
                            ..., # `...` to `epi_slide` forwarded here
                            f_factory,
                            starts,
                            stops,
                            ref_time_values,
                            all_rows,
                            new_col) {
    # Figure out which reference time values appear in the data group in the
    # first place (we need to do this because it could differ based on the
    # group, hence the setup/checks for the reference time values based on all
    # the data could still be off):
    o <- ref_time_values %in% .data_group$time_value
    starts <- starts[o]
    stops <- stops[o]
    kept_ref_time_values <- ref_time_values[o]

    f <- f_factory(kept_ref_time_values)

    # Compute the slide values
    slide_values_list <- slider::hop_index(
      .x = .data_group,
      .i = .data_group$time_value,
      .starts = starts,
      .stops = stops,
      .f = f,
      .group_key, ...
    )

    # Now figure out which rows in the data group are in the reference time
    # values; this will be useful for all sorts of checks that follow
    o <- .data_group$time_value %in% kept_ref_time_values
    num_ref_rows <- sum(o)

    # Count the number of appearances of each kept reference time value.
    counts <- dplyr::filter(.data_group, .data$time_value %in% kept_ref_time_values) %>%
      dplyr::count(.data$time_value) %>%
      `[[`("n")

    if (!all(purrr::map_lgl(slide_values_list, is.atomic)) &&
      !all(purrr::map_lgl(slide_values_list, is.data.frame))) {
      cli_abort("The slide computations must return always atomic vectors or data frames (and not a mix of these two structures).")
    }

    # Unlist if appropriate:
    slide_values <-
      if (as_list_col) {
        slide_values_list
      } else {
        vctrs::list_unchop(slide_values_list)
      }

    if (all(purrr::map_int(slide_values_list, vctrs::vec_size) == 1L) &&
      length(slide_values_list) != 0L) {
      # Recycle to make size stable (one slide value per ref time value).
      # (Length-0 case also could be handled here, but causes difficulties;
      # leave it to the next branch, where it also belongs.)
      slide_values <- vctrs::vec_rep_each(slide_values, times = counts)
    } else {
      # Split and flatten if appropriate, perform a (loose) check on number of
      # rows.
      if (as_list_col) {
        slide_values <- purrr::list_flatten(purrr::map(
          slide_values, ~ vctrs::vec_split(.x, seq_len(vctrs::vec_size(.x)))[["val"]]
        ))
      }
      if (vctrs::vec_size(slide_values) != num_ref_rows) {
        cli_abort("The slide computations must either (a) output a single element/row each, or (b) one element/row per appearance of the reference time value in the local window.")
      }
    }

    # If all rows, then pad slide values with NAs, else filter down data group
    if (all_rows) {
      orig_values <- slide_values
      slide_values <- vctrs::vec_rep(vctrs::vec_cast(NA, orig_values), nrow(.data_group))
      # ^ using vctrs::vec_init would be shorter but docs don't guarantee it
      # fills with NA equivalent.
      vctrs::vec_slice(slide_values, o) <- orig_values
    } else {
      .data_group <- filter(.data_group, o)
    }
    return(mutate(.data_group, !!new_col := slide_values))
  }

  # If `f` is missing, interpret ... as an expression for tidy evaluation
  if (missing(f)) {
    quos <- enquos(...)
    if (length(quos) == 0) {
      cli_abort("If `f` is missing then a computation must be specified via `...`.")
    }
    if (length(quos) > 1) {
      cli_abort("If `f` is missing then only a single computation can be specified via `...`.")
    }

    f <- quos[[1]]
    new_col <- sym(names(rlang::quos_auto_name(quos)))
    ... <- missing_arg() # magic value that passes zero args as dots in calls below
  }

  f <- as_slide_computation(f, ...)
  # Create a wrapper that calculates and passes `.ref_time_value` to the
  # computation. `i` is contained in the `f_wrapper_factory` environment such
  # that when called within `slide_one_grp` `i` is reset for every group.
  f_wrapper_factory <- function(kept_ref_time_values) {
    # Use `i` to advance through list of start dates.
    i <- 1L
    f_wrapper <- function(.x, .group_key, ...) {
      .ref_time_value <- kept_ref_time_values[[i]]
      i <<- i + 1L
      f(.x, .group_key, .ref_time_value, ...)
    }
    return(f_wrapper)
  }
  x <- group_modify(x, slide_one_grp,
    ...,
    f_factory = f_wrapper_factory,
    starts = starts,
    stops = stops,
    ref_time_values = ref_time_values,
    all_rows = all_rows,
    new_col = new_col,
    .keep = FALSE
  )

  # Unnest if we need to, and return
  if (!as_list_col) {
    x <- unnest(x, !!new_col, names_sep = names_sep)
  }

  return(x)
}

#' Optimized slide function for performing rolling averages on an `epi_df` object
#'
#' Slides an n-timestep mean over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' @param x The `epi_df` object under consideration, [grouped][dplyr::group_by]
#'   or ungrouped. If ungrouped, all data in `x` will be treated as part of a
#'   single data group.
#' @param col_names A character vector of the names of one or more columns for
#'   which to calculate the rolling mean.
#' @param ... Additional arguments to pass to `data.table::frollmean`, for
#'   example, `na.rm` and `algo`. `data.table::frollmean` is automatically
#'   passed the data `x` to operate on, the window size `n`, and the alignment
#'   `align`. Providing these args via `...` will cause an error.
#' @param before,after How far `before` and `after` each `ref_time_value` should
#'   the sliding window extend? At least one of these two arguments must be
#'   provided; the other's default will be 0. Any value provided for either
#'   argument must be a single, non-`NA`, non-negative,
#'   [integer-compatible][vctrs::vec_cast] number of time steps. Endpoints of
#'   the window are inclusive. Common settings:
#'   * For trailing/right-aligned windows from `ref_time_value - time_step
#'     (k)` to `ref_time_value`: either pass `before=k` by itself, or pass
#'     `before=k, after=0`.
#'   * For center-aligned windows from `ref_time_value - time_step(k)` to
#'     `ref_time_value + time_step(k)`: pass `before=k, after=k`.
#'   * For leading/left-aligned windows from `ref_time_value` to
#'     `ref_time_value + time_step(k)`: either pass pass `after=k` by itself,
#'     or pass `before=0, after=k`.
#'   See "Details:" about the definition of a time step,(non)treatment of
#'   missing rows within the window, and avoiding warnings about
#'   `before`&`after` settings for a certain uncommon use case.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a non-negative integer and
#'   return an object of class `lubridate::period`. For example, we can use
#'   `time_step = lubridate::hours` in order to set the time step to be one hour
#'   (this would only be meaningful if `time_value` is of class `POSIXct`).
#' @param new_col_names String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_names` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the slide results be held in a list column, or be
#'   [unchopped][tidyr::unchop]/[unnested][tidyr::unnest]? Default is `FALSE`,
#'   in which case a list object returned by `f` would be unnested (using
#'   [`tidyr::unnest()`]), and, if the slide computations output data frames,
#'   the names of the resulting columns are given by prepending `new_col_names`
#'   to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_names` entirely.
#' @param all_rows If `all_rows = TRUE`, then all rows of `x` will be kept in
#'   the output even with `ref_time_values` provided, with some type of missing
#'   value marker for the slide computation output column(s) for `time_value`s
#'   outside `ref_time_values`; otherwise, there will be one row for each row in
#'   `x` that had a `time_value` in `ref_time_values`. Default is `FALSE`. The
#'   missing value marker is the result of `vctrs::vec_cast`ing `NA` to the type
#'   of the slide computation output. If using `as_list_col = TRUE`, note that
#'   the missing marker is a `NULL` entry in the list column; for certain
#'   operations, you might want to replace these `NULL` entries with a different
#'   `NA` marker.
#' @return An `epi_df` object given by appending one or more new columns to
#'  `x`, depending on the `col_names` argument, named according to the
#'  `new_col_names` argument.
#'
#' @details To "slide" means to apply a function or formula over a rolling
#'   window of time steps for each data group, where the window is entered at a
#'   reference time and left and right endpoints are given by the `before` and
#'   `after` arguments. The unit (the meaning of one time step) is implicitly
#'   defined by the way the `time_value` column treats addition and subtraction;
#'   for example, if the time values are coded as `Date` objects, then one time
#'   step is one day, since `as.Date("2022-01-01") + 1` equals
#'   `as.Date("2022-01-02")`. Alternatively, the time step can be set explicitly
#'   using the `time_step` argument (which if specified would override the
#'   default choice based on `time_value` column). If there are not enough time
#'   steps available to complete the window at any given reference time, then
#'   `epi_slide()` still attempts to perform the computation anyway (it does not
#'   require a complete window). The issue of what to do with partial
#'   computations (those run on incomplete windows) is therefore left up to the
#'   user, either through the specified function or formula `f`, or through
#'   post-processing. For a centrally-aligned slide of `n` `time_value`s in a
#'   sliding window, set `before = (n-1)/2` and `after = (n-1)/2` when the
#'   number of `time_value`s in a sliding window is odd and `before = n/2-1` and
#'   `after = n/2` when `n` is even.
#'
#'   Sometimes, we want to experiment with various trailing or leading window
#'   widths and compare the slide outputs. In the (uncommon) case where
#'   zero-width windows are considered, manually pass both the `before` and
#'   `after` arguments in order to prevent potential warnings. (E.g., `before=k`
#'   with `k=0` and `after` missing may produce a warning. To avoid warnings,
#'   use `before=k, after=0` instead; otherwise, it looks too much like a
#'   leading window was intended, but the `after` argument was forgotten or
#'   misspelled.)
#'
#' @importFrom dplyr bind_rows mutate %>% arrange tibble
#' @importFrom purrr map
#' @importFrom data.table frollmean
#' @importFrom lubridate as.period
#' @importFrom checkmate assert_function
#' @export
#' @seealso [`epi_slide`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean("cases", new_col_names = "cases_7dav", names_sep = NULL, before = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day trailing average formula on cases. Adjust `frollmean` settings for speed
#' # and accuracy, and to allow partially-missing windows.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean("cases", new_col_names = "cases_7dav", names_sep = NULL, before = 6,
#'     # `frollmean` options
#'     na.rm = TRUE, algo = "exact", hasNA = TRUE
#'   ) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean("cases", new_col_names = "cases_7dav", names_sep = NULL, after = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean("cases", new_col_names = "cases_7dav", names_sep = NULL, before = 3, after = 3) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 14-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean("cases", new_col_names = "cases_14dav", names_sep = NULL, before = 6, after = 7) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_14dav) %>%
#'   ungroup()
epi_slide_mean = function(x, col_names, ..., before, after, ref_time_values,
                     time_step,
                     new_col_names = "slide_value", as_list_col = FALSE,
                     names_sep = "_", all_rows = FALSE) {
  assert_class(x, "epi_df")

  if (as_list_col) {
    cli::cli_abort(
      "`as_list_col` is not supported for `epi_slide_mean`",
      class = "epiproces__epi_slide_mean__list_not_supported"
    )
  }

  user_provided_rtvs <- !missing(ref_time_values)
  if (!user_provided_rtvs) {
    ref_time_values <- unique(x$time_value)
  } else {
    assert_numeric(ref_time_values, min.len = 1L, null.ok = FALSE, any.missing = FALSE)
    if (!test_subset(ref_time_values, unique(x$time_value))) {
      cli_abort(
        "`ref_time_values` must be a unique subset of the time values in `x`."
      )
    }
    if (anyDuplicated(ref_time_values) != 0L) {
      cli_abort("`ref_time_values` must not contain any duplicates; use `unique` if appropriate.")
    }
  }
  ref_time_values <- sort(ref_time_values)

  # Validate and pre-process `before`, `after`:
  if (!missing(before)) {
    before <- vctrs::vec_cast(before, integer())
    assert_int(before, lower = 0, null.ok = FALSE, na.ok = FALSE)
  }
  if (!missing(after)) {
    after <- vctrs::vec_cast(after, integer())
    assert_int(after, lower = 0, null.ok = FALSE, na.ok = FALSE)
  }
  if (missing(before)) {
    if (missing(after)) {
      cli_abort("Either or both of `before`, `after` must be provided.")
    } else if (after == 0L) {
      cli_warn("`before` missing, `after==0`; maybe this was intended to be some
            non-zero-width trailing window, but since `before` appears to be
            missing, it's interpreted as a zero-width window (`before=0,
            after=0`).")
    }
    before <- 0L
  } else if (missing(after)) {
    if (before == 0L) {
      cli_warn("`before==0`, `after` missing; maybe this was intended to be some
            non-zero-width leading window, but since `after` appears to be
            missing, it's interpreted as a zero-width window (`before=0,
            after=0`).")
    }
    after <- 0L
  }

  # Make a complete date sequence between min(x$time_value) and max(x$time_value).
  date_seq_list <- full_date_seq(x, before, after, time_step)
  all_dates <- date_seq_list$all_dates
  pad_early_dates <- date_seq_list$pad_early_dates
  pad_late_dates <- date_seq_list$pad_late_dates

  # `frollmean` is 1-indexed, so create a new window width based on our
  # `before` and `after` params.
  m <- before + after + 1L

  if (is.null(names_sep) && !as_list_col) {
    if (length(new_col_names) != length(col_names)) {
     cli_abort(
        "`new_col_names` must be the same length as `col_names` when `names_sep` is NULL.",
        class = "epiprocess__epi_slide_mean__col_names_length_mismatch",
        epiprocess__new_col_names = new_col_names,
        epiprocess__col_names = col_names
      )
    }
    result_col_names <- new_col_names
  } else {
    if (length(new_col_names) != 1L && length(new_col_names) != length(col_names)) {
     cli_abort(
        "`new_col_names` must be either length 1 or the same length as `col_names`.",
        class = "epiprocess__epi_slide_mean__col_names_length_mismatch_and_not_one",
        epiprocess__new_col_names = new_col_names,
        epiprocess__col_names = col_names
      )
    }
    result_col_names <- paste(new_col_names, col_names, sep = names_sep)
  }

  slide_one_grp <- function(.data_group, .group_key, ...) {
    missing_times <- all_dates[!(all_dates %in% .data_group$time_value)]

    # `frollmean` requires a full window to compute a result. Add NA values
    # to beginning and end of the group so that we get results for the
    # first `before` and last `after` elements.
    .data_group <- bind_rows(
      .data_group,
      tibble(time_value = c(missing_times, pad_early_dates, pad_late_dates), .real = FALSE)
    ) %>%
      arrange(time_value)

    # If a group contains duplicate time values, `frollmean` will still only
    # use the last `k` obs. It isn't looking at dates, it just goes in row
    # order. So if the computation is aggregating across multiple obs for the
    # same date, `epi_slide_mean` will produce incorrect results; `epi_slide`
    # should be used instead.
    if (anyDuplicated(.data_group$time_value) > 0) {
      cli_abort(c(
          "group contains duplicate time values. Using `epi_slide_mean` on this
            group will result in incorrect results",
          "i" = "Please change the grouping structure of the input data so that
            each group has non-duplicate time values",
          "i" = "Use `epi_slide` to aggregate across groups"
        ),
        class = "epiprocess__epi_slide_mean__duplicate_time_values",
        epiprocess__data_group = .data_group,
        epiprocess__group_key = .group_key
      )
    }

    roll_output <- data.table::frollmean(
      x = .data_group[, col_names], n = m, align = "right", ...
    )

    if (after >= 1) {
      # Right-aligned `frollmean` results' `ref_time_value`s will be `after`
      # timesteps ahead of where they should be. Shift results to the left by
      # `after` timesteps.
      .data_group[, result_col_names] <- purrr::map(roll_output, function(.x) {
          c(.x[(after + 1L):length(.x)], rep(NA, after))
        }
      )
    } else {
      .data_group[, result_col_names] <- roll_output
    }

    return(.data_group)
  }

  result <- mutate(x, .real = TRUE) %>%
    group_modify(slide_one_grp, ..., .keep = FALSE)

  result <- result[result$.real, ]
  result$.real <- NULL

  if (all_rows) {
    result[!(result$time_value %in% ref_time_values), result_col_names] <- NA
  } else if (user_provided_rtvs) {
    result <- result[result$time_value %in% ref_time_values, ]
  }

  if (!is_epi_df(result)) {
    # `all_rows` and `as_list_col` handling strip epi_df format and metadata.
    # Restore them.
    result <- reclass(result, attributes(x)$metadata)
  }

  return(result)
}

#' Make a complete date sequence between min(x$time_value) and max
#' (x$time_value). Produce lists of dates before min(x$time_value) and after
#' max(x$time_value) for padding initial and final windows to size `n`.
#'
#' `before` and `after` inputs here should be raw (numeric) values;
#' `time_step` function should NOT have been applied. `full_date_seq` applies
#' `time_step` as needed.
#'
#' @importFrom checkmate assert_function
#' @noRd
full_date_seq <- function(x, before, after, time_step) {
  pad_early_dates <- c()
  pad_late_dates <- c()

  # If dates are one of tsibble-provided classes, can step by numeric. `tsibble`
  # defines a step of 1 to automatically be the quantum (smallest resolvable
  # unit) of the date class. For example, one step = 1 quarter for `yearquarter`.
  #
  # `tsibble` classes apparently can't be added to in different units, so even
  # if `time_step` is provided by the user, use a unit step.
  if (inherits(x$time_value, c("yearquarter", "yearweek", "yearmonth")) ||
    is.numeric(x$time_value)) {
    all_dates <- seq(min(x$time_value), max(x$time_value), by = 1L)

    if (before != 0) {
      pad_early_dates <- Start(all_dates) - before:1
    }
    if (after != 0) {
      pad_late_dates <- End(all_dates) + 1:after
    }
  } else if (missing(time_step)) {
    # Guess what `by` should be based on the epi_df's `time_type`.
    ttype <- attributes(x)$metadata$time_type
    by <- switch(ttype,
      day = "days",
      week = "weeks",
      yearweek = "weeks",
      yearmonth = "months",
      yearquarter = "quarters",
      year = "years",
      NA # default value for "custom", "day-time"
    )

    if (is.na(by)) {
     cli_abort(
        c(
          "`frollmean` requires a full window to compute a result, but
          `time_type` associated with the epi_df was not mappable to period
          type valid for creating a date sequence.",
          "i" = c("The input data's `time_type` was probably `custom` or `day-time`.
          These require also passing a `time_step` function.")
        ),
        class = "epiprocess__epi_slide_mean__unmappable_time_type",
        epiprocess__time_type = ttype
      )
    }

    # `seq.Date` `by` arg can be any of `c("days", "weeks", "months", "quarters", "years")`.
    all_dates <- seq(min(x$time_value), max(x$time_value), by = by)

    if (before != 0) {
      # Use `seq.Date` here to avoid having to map `epi_df` `time_type` to
      # `time_step` functions.
      #
      # The first element `seq.Date` returns is always equal to the provided
      # `from` date (`from + 0`). The full return value is equivalent to
      # `from + 0:n`. In our case, we `from + 1:n`, so drop the first
      # element.
      #
      # Adding "-1" to the `by` arg makes `seq.Date` go backwards in time.
      pad_early_dates <- sort(seq(Start(all_dates), by = paste("-1", by), length.out = before + 1)[-1])
    }
    if (after != 0) {
      pad_late_dates <- seq(End(all_dates), by = by, length.out = after + 1)[-1]
    }
  } else {
    # A custom time step is specified.
    assert_function(time_step)

    # Calculate the number of `time_step`s required to go between min and max time
    # values. This is roundabout because difftime objects, lubridate::period objects,
    # and Dates are hard to convert to the same time scale and add.
    t_elapsed_s <- difftime(max(x$time_value), min(x$time_value), units = "secs")
    step_size_s <- lubridate::as.period(time_step(1), unit = "secs")
    n_steps <- ceiling(as.numeric(t_elapsed_s) / as.numeric(step_size_s))

    all_dates <- min(x$time_value) + time_step(0:n_steps)

    if (before != 0) {
      pad_early_dates <- Start(all_dates) - time_step(before:1)
    }
    if (after != 0) {
      pad_late_dates <- End(all_dates) + time_step(1:after)
    }
  }

  return(list(
    all_dates = all_dates,
    pad_early_dates = pad_early_dates,
    pad_late_dates = pad_late_dates
  ))
}