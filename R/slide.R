#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the
#' [slide vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html)
#' for examples.
#'
#' @template basic-slide-params
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
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the slide results be held in a list column, or be
#'   [unchopped][tidyr::unchop]/[unnested][tidyr::unnest]? Default is `FALSE`,
#'   in which case a list object returned by `f` would be unnested (using
#'   [`tidyr::unnest()`]), and, if the slide computations output data frames,
#'   the names of the resulting columns are given by prepending `new_col_name`
#'   to the names of the list elements.
#'
#' @template basic-slide-details
#'
#' @importFrom lubridate days weeks
#' @importFrom dplyr bind_rows group_vars filter select
#' @importFrom rlang .data .env !! enquos sym env missing_arg
#' @export
#' @seealso [`epi_slide_opt`] [`epi_slide_mean`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' # Simple sliding means and sums are much faster to do using
#' # the `epi_slide_mean` and `epi_slide_sum` functions instead.
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
epi_slide <- function(x, f, ..., before = NULL, after = NULL, ref_time_values = NULL,
                      new_col_name = "slide_value", as_list_col = FALSE,
                      names_sep = "_", all_rows = FALSE) {
  assert_class(x, "epi_df")

  if (nrow(x) == 0L) {
    return(x)
  }

  if (is.null(ref_time_values)) {
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

  # Handle defaults for before/after
  time_type <- attr(x, "metadata")$time_type
  if (is.null(before) && !is.null(after)) {
    if (inherits(after, "difftime")) {
      before <- as.difftime(0, units = units(after))
    } else {
      before <- 0
    }
  }
  if (is.null(after) && !is.null(before)) {
    if (inherits(before, "difftime")) {
      after <- as.difftime(0, units = units(before))
    } else {
      if (before == Inf && time_type %in% c("day", "week")) {
        after <- as.difftime(0, units = glue::glue("{time_type}s"))
      } else {
        after <- 0
      }
    }
  }
  validate_slide_window_arg(before, time_type)
  validate_slide_window_arg(after, time_type, allow_inf = FALSE)

  # Arrange by increasing time_value
  x <- arrange(x, .data$time_value)

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

    if (
      !all(purrr::map_lgl(slide_values_list, is.atomic)) &&
        !all(purrr::map_lgl(slide_values_list, is.data.frame))
    ) {
      cli_abort(
        "The slide computations must return always atomic vectors
          or data frames (and not a mix of these two structures)."
      )
    }

    # Unlist if appropriate:
    slide_values <-
      if (as_list_col) {
        slide_values_list
      } else {
        vctrs::list_unchop(slide_values_list)
      }

    if (
      all(purrr::map_int(slide_values_list, vctrs::vec_size) == 1L) &&
        length(slide_values_list) != 0L
    ) {
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
        cli_abort(
          "The slide computations must either (a) output a single element/row each, or
          (b) one element/row per appearance of the reference time value in the local window."
        )
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
    # Magic value that passes zero args as dots in calls below. Equivalent to
    # `... <- missing_arg()`, but use `assign` to avoid warning about
    # improper use of dots.
    assign("...", missing_arg())
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

#' Optimized slide function for performing common rolling computations on an `epi_df` object
#'
#' Slides an n-timestep [data.table::froll] or [slider::summary-slide] function
#' over variables in an `epi_df` object. See the
#' [slide vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html)
#' for examples.
#'
#' @template basic-slide-params
#' @template opt-slide-params
#' @param f Function; together with `...` specifies the computation to slide.
#'  `f` must be one of `data.table`'s rolling functions
#'  (`frollmean`, `frollsum`, `frollapply`. See [data.table::roll]) or one
#'  of `slider`'s specialized sliding functions (`slide_mean`, `slide_sum`,
#'  etc. See [slider::summary-slide]). To "slide" means to apply a
#'  computation within a sliding (a.k.a. "rolling") time window for each data
#'  group. The window is determined by the `before` and `after` parameters
#'  described below. One time step is typically one day or one week; see
#'  details for more explanation.
#'
#'  The optimized `data.table` and `slider` functions can't be directly passed
#'  as the computation function in `epi_slide` without careful handling to
#'  make sure each computation group is made up of the `n` dates rather than
#'  `n` points. `epi_slide_opt` (and wrapper functions `epi_slide_mean` and
#'  `epi_slide_sum`) take care of window completion automatically to prevent
#'  associated errors.
#' @param ... Additional arguments to pass to the slide computation `f`, for
#'  example, `na.rm` and `algo` if `f` is a `data.table` function. If `f` is
#'  a `data.table` function, it is automatically passed the data `x` to
#'  operate on, the window size `n`, and the alignment `align`. Providing
#'  these args via `...` will cause an error. If `f` is a `slider` function,
#'  it is automatically passed the data `x` to operate on, and number of
#'  points `before` and `after` to use in the computation.
#'
#' @template opt-slide-details
#'
#' @importFrom dplyr bind_rows mutate %>% arrange tibble select all_of
#' @importFrom rlang enquo quo_get_expr as_label expr_label caller_arg
#' @importFrom tidyselect eval_select
#' @importFrom purrr map map_lgl
#' @importFrom data.table frollmean frollsum frollapply
#' @importFrom lubridate as.period
#' @importFrom checkmate assert_function
#' @importFrom slider slide_sum slide_prod slide_mean slide_min slide_max slide_all slide_any
#' @export
#' @seealso [`epi_slide`] [`epi_slide_mean`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases. This can also be done with `epi_slide_mean`
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     f = data.table::frollmean, before = 6
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed, and rename new col
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day trailing average formula on cases. Adjust `frollmean` settings for speed
#' # and accuracy, and to allow partially-missing windows.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     f = data.table::frollmean, before = 6,
#'     # `frollmean` options
#'     na.rm = TRUE, algo = "exact", hasNA = TRUE
#'   ) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     f = slider::slide_mean, after = 6
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day centre-aligned sum. This can also be done with `epi_slide_sum`
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     f = data.table::frollsum, before = 3, after = 3
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
epi_slide_opt <- function(x, col_names, f, ..., before = NULL, after = NULL, ref_time_values = NULL,
                          new_col_name = NULL, as_list_col = NULL,
                          names_sep = NULL, all_rows = FALSE) {
  assert_class(x, "epi_df")

  if (nrow(x) == 0L) {
    cli_abort(
      c(
        "input data `x` unexpectedly has 0 rows",
        "i" = "If this computation is occuring within an `epix_slide` call,
          check that `epix_slide` `ref_time_values` argument was set appropriately"
      ),
      class = "epiprocess__epi_slide_opt__0_row_input",
      epiprocess__x = x
    )
  }

  if (!is.null(as_list_col)) {
    cli_abort(
      "`as_list_col` is not supported for `epi_slide_[opt/mean/sum]`",
      class = "epiprocess__epi_slide_opt__list_not_supported"
    )
  }
  if (!is.null(new_col_name)) {
    cli_abort(
      c(
        "`new_col_name` is not supported for `epi_slide_[opt/mean/sum]`",
        "i" = "If you want to customize the output column names, use [`dplyr::rename`] after the slide."
      ),
      class = "epiprocess__epi_slide_opt__new_name_not_supported"
    )
  }
  if (!is.null(names_sep)) {
    cli_abort(
      c(
        "`names_sep` is not supported for `epi_slide_[opt/mean/sum]`",
        "i" = "If you want to customize the output column names, use [`dplyr::rename`] after the slide."
      ),
      class = "epiprocess__epi_slide_opt__name_sep_not_supported"
    )
  }

  # Check that slide function `f` is one of those short-listed from
  # `data.table` and `slider` (or a function that has the exact same
  # definition, e.g. if the function has been reexported or defined
  # locally).
  if (any(map_lgl(
    list(frollmean, frollsum, frollapply),
    function(roll_fn) {
      identical(f, roll_fn)
    }
  ))) {
    f_from_package <- "data.table"
  } else if (any(map_lgl(
    list(slide_sum, slide_prod, slide_mean, slide_min, slide_max, slide_all, slide_any),
    function(roll_fn) {
      identical(f, roll_fn)
    }
  ))) {
    f_from_package <- "slider"
  } else {
    # `f` is from somewhere else and not supported
    cli_abort(
      c(
        "problem with {rlang::expr_label(rlang::caller_arg(f))}",
        "i" = "`f` must be one of `data.table`'s rolling functions (`frollmean`,
              `frollsum`, `frollapply`. See `?data.table::roll`) or one of
              `slider`'s specialized sliding functions (`slide_mean`, `slide_sum`,
              etc. See `?slider::\`summary-slide\`` for more options)."
      ),
      class = "epiprocess__epi_slide_opt__unsupported_slide_function",
      epiprocess__f = f
    )
  }

  user_provided_rtvs <- !is.null(ref_time_values)
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

  # Handle defaults for before/after
  time_type <- attr(x, "metadata")$time_type
  if (is.null(before) && !is.null(after)) {
    if (inherits(after, "difftime")) {
      before <- as.difftime(0, units = units(after))
    } else {
      before <- 0
    }
  }
  if (is.null(after) && !is.null(before)) {
    if (inherits(before, "difftime")) {
      after <- as.difftime(0, units = units(before))
    } else {
      if (before == Inf && time_type %in% c("day", "week")) {
        after <- as.difftime(0, units = glue::glue("{time_type}s"))
      } else {
        after <- 0
      }
    }
  }
  validate_slide_window_arg(before, time_type)
  validate_slide_window_arg(after, time_type, allow_inf = FALSE)

  # Make a complete date sequence between min(x$time_value) and max(x$time_value).
  date_seq_list <- full_date_seq(x, before, after, time_type)
  all_dates <- date_seq_list$all_dates
  pad_early_dates <- date_seq_list$pad_early_dates
  pad_late_dates <- date_seq_list$pad_late_dates

  # The position of a given column can be differ between input `x` and
  # `.data_group` since the grouping step by default drops grouping columns.
  # To avoid rerunning `eval_select` for every `.data_group`, convert
  # positions of user-provided `col_names` into string column names. We avoid
  # using `names(pos)` directly for robustness and in case we later want to
  # allow users to rename fields via tidyselection.
  if (class(quo_get_expr(enquo(col_names))) == "call") {
    pos <- eval_select(enquo(col_names), data = x, allow_rename = FALSE)
  } else {
    pos <- eval_select(all_of(col_names), data = x, allow_rename = FALSE)
  }
  col_names_chr <- names(x)[pos]
  # Always rename results to "slide_value_<original column name>".
  result_col_names <- paste0("slide_value_", col_names_chr)
  slide_one_grp <- function(.data_group, .group_key, ...) {
    missing_times <- all_dates[!(all_dates %in% .data_group$time_value)]
    # `frollmean` requires a full window to compute a result. Add NA values
    # to beginning and end of the group so that we get results for the
    # first `before` and last `after` elements.
    .data_group <- bind_rows(
      .data_group,
      tibble(time_value = c(missing_times, pad_early_dates, pad_late_dates), .real = FALSE)
    ) %>%
      arrange(.data$time_value)

    if (f_from_package == "data.table") {
      # If a group contains duplicate time values, `frollmean` will still only
      # use the last `k` obs. It isn't looking at dates, it just goes in row
      # order. So if the computation is aggregating across multiple obs for the
      # same date, `epi_slide_opt` and derivates will produce incorrect results;
      # `epi_slide` should be used instead.
      if (anyDuplicated(.data_group$time_value) != 0L) {
        cli_abort(
          c(
            "group contains duplicate time values. Using `epi_slide_[opt/mean/sum]` on this
              group will result in incorrect results",
            "i" = "Please change the grouping structure of the input data so that
              each group has non-duplicate time values (e.g. `x %>% group_by(geo_value)
              %>% epi_slide_opt(f = frollmean)`)",
            "i" = "Use `epi_slide` to aggregate across groups"
          ),
          class = "epiprocess__epi_slide_opt__duplicate_time_values",
          epiprocess__data_group = .data_group,
          epiprocess__group_key = .group_key
        )
      }

      if (nrow(.data_group) != length(c(all_dates, pad_early_dates, pad_late_dates))) {
        cli_abort(
          c(
            "group contains an unexpected number of rows",
            "i" = c("Input data may contain `time_values` closer together than the
              expected `time_step` size")
          ),
          class = "epiprocess__epi_slide_opt__unexpected_row_number",
          epiprocess__data_group = .data_group,
          epiprocess__group_key = .group_key
        )
      }

      # `frollmean` is 1-indexed, so create a new window width based on our
      # `before` and `after` params. Right-aligned `frollmean` results'
      # `ref_time_value`s will be `after` timesteps ahead of where they should
      # be; shift results to the left by `after` timesteps.
      if (before != Inf) {
        window_size <- before + after + 1L
        roll_output <- f(x = .data_group[, col_names_chr], n = window_size, ...)
      } else {
        window_size <- list(seq_along(.data_group$time_value))
        roll_output <- f(x = .data_group[, col_names_chr], n = window_size, adaptive = TRUE, ...)
      }
      if (after >= 1) {
        .data_group[, result_col_names] <- purrr::map(roll_output, function(.x) {
          c(.x[(after + 1L):length(.x)], rep(NA, after))
        })
      } else {
        .data_group[, result_col_names] <- roll_output
      }
    }
    if (f_from_package == "slider") {
      for (i in seq_along(col_names_chr)) {
        .data_group[, result_col_names[i]] <- f(
          x = .data_group[[col_names_chr[i]]],
          before = as.numeric(before),
          after = as.numeric(after),
          ...
        )
      }
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
    # `all_rows`handling strips epi_df format and metadata.
    # Restore them.
    result <- reclass(result, attributes(x)$metadata)
  }

  return(result)
}

#' Optimized slide function for performing rolling averages on an `epi_df` object
#'
#' Slides an n-timestep mean over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' Wrapper around `epi_slide_opt` with `f = datatable::frollmean`.
#'
#' @template basic-slide-params
#' @template opt-slide-params
#' @param ... Additional arguments to pass to `data.table::frollmean`, for
#'   example, `na.rm` and `algo`. `data.table::frollmean` is automatically
#'   passed the data `x` to operate on, the window size `n`, and the alignment
#'   `align`. Providing these args via `...` will cause an error.
#'
#' @template opt-slide-details
#'
#' @export
#' @seealso [`epi_slide`] [`epi_slide_opt`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, before = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day trailing average formula on cases. Adjust `frollmean` settings for speed
#' # and accuracy, and to allow partially-missing windows.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(
#'     cases,
#'     before = 6,
#'     # `frollmean` options
#'     na.rm = TRUE, algo = "exact", hasNA = TRUE
#'   ) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, after = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, before = 3, after = 3) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 14-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, before = 6, after = 7) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_14dav = slide_value_cases) %>%
#'   ungroup()
epi_slide_mean <- function(x, col_names, ..., before = NULL, after = NULL, ref_time_values = NULL,
                           new_col_name = NULL, as_list_col = NULL,
                           names_sep = NULL, all_rows = FALSE) {
  epi_slide_opt(
    x = x,
    col_names = {{ col_names }},
    f = data.table::frollmean,
    ...,
    before = before,
    after = after,
    ref_time_values = ref_time_values,
    new_col_name = new_col_name,
    as_list_col = as_list_col,
    names_sep = names_sep,
    all_rows = all_rows
  )
}

#' Optimized slide function for performing rolling sums on an `epi_df` object
#'
#' Slides an n-timestep sum over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' Wrapper around `epi_slide_opt` with `f = datatable::frollsum`.
#'
#' @template basic-slide-params
#' @template opt-slide-params
#' @param ... Additional arguments to pass to `data.table::frollsum`, for
#'   example, `na.rm` and `algo`. `data.table::frollsum` is automatically
#'   passed the data `x` to operate on, the window size `n`, and the alignment
#'   `align`. Providing these args via `...` will cause an error.
#'
#' @template opt-slide-details
#'
#' @export
#' @seealso [`epi_slide`] [`epi_slide_opt`] [`epi_slide_mean`]
#' @examples
#' # slide a 7-day trailing sum formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_sum(cases, before = 6) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dsum = slide_value_cases) %>%
#'   ungroup()
epi_slide_sum <- function(x, col_names, ..., before = NULL, after = NULL, ref_time_values = NULL,
                          new_col_name = NULL, as_list_col = NULL,
                          names_sep = NULL, all_rows = FALSE) {
  epi_slide_opt(
    x = x,
    col_names = {{ col_names }},
    f = data.table::frollsum,
    ...,
    before = before,
    after = after,
    ref_time_values = ref_time_values,
    new_col_name = new_col_name,
    as_list_col = as_list_col,
    names_sep = names_sep,
    all_rows = all_rows
  )
}

#' Make a complete date sequence between min(x$time_value) and max
#' (x$time_value). Produce lists of dates before min(x$time_value) and after
#' max(x$time_value) for padding initial and final windows to size `n`.
#'
#' `before` and `after` args are assumed to have been validated by the calling
#' function (using `validate_slide_window_arg`).
#'
#' @importFrom checkmate assert_function
#' @noRd
full_date_seq <- function(x, before, after, time_type) {
  if (!time_type %in% c("day", "week", "yearmonth", "integer")) {
    cli_abort(
      "time_type must be one of 'day', 'week', or 'integer'."
    )
  }

  pad_early_dates <- c()
  pad_late_dates <- c()

  # `tsibble` time types have their own behavior, where adding 1 corresponds to
  # incrementing by a quantum (smallest resolvable unit) of the date class. For
  # example, one step = 1 quarter for `yearquarter`.
  if (time_type %in% c("yearmonth", "integer")) {
    all_dates <- seq(min(x$time_value), max(x$time_value), by = 1L)

    if (before != 0 && before != Inf) {
      pad_early_dates <- all_dates[1L] - before:1
    }
    if (after != 0) {
      pad_late_dates <- all_dates[length(all_dates)] + 1:after
    }
  } else {
    by <- switch(time_type,
      day = "days",
      week = "weeks",
    )

    all_dates <- seq(min(x$time_value), max(x$time_value), by = by)
    if (before != 0 && before != Inf) {
      # The behavior is analogous to the branch with tsibble types above. For
      # more detail, note that the function `seq.Date(from, ..., length.out =
      # n)` returns `from + 0:n`. Since we want `from + 1:n`, we drop the first
      # element. Adding "-1" to the `by` arg makes `seq.Date` go backwards in
      # time.
      pad_early_dates <- sort(seq(all_dates[1L], by = paste("-1", by), length.out = before + 1)[-1])
    }
    if (after != 0) {
      pad_late_dates <- seq(all_dates[length(all_dates)], by = by, length.out = after + 1)[-1]
    }
  }

  return(list(
    all_dates = all_dates,
    pad_early_dates = pad_early_dates,
    pad_late_dates = pad_late_dates
  ))
}
