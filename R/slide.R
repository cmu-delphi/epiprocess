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
#'   the window are inclusive. Common settings: * For trailing/right-aligned
#'   windows from `ref_time_value - time_step(k)` to `ref_time_value`: either
#'   pass `before=k` by itself, or pass `before=k, after=0`. * For
#'   center-aligned windows from `ref_time_value - time_step(k)` to
#'   `ref_time_value + time_step(k)`: pass `before=k, after=k`. * For
#'   leading/left-aligned windows from `ref_time_value` to `ref_time_value +
#'   time_step(k)`: either pass pass `after=k` by itself, or pass `before=0,
#'   after=k`. See "Details:" about the definition of a time step,
#'   (non)treatment of missing rows within the window, and avoiding warnings
#'   about `before`&`after` settings for a certain uncommon use case.
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
#' @importFrom rlang .data .env !! enquo enquos sym env
#' @export
#' @examples 
#' # slide a 7-day trailing average formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), before = 6) %>% 
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av) 
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), after = 6) %>%
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av)
#'
#' # slide a 7-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), before = 3, after = 3) %>% 
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av) 
#'
#' # slide a 14-day centre-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), before = 6, after = 7) %>% 
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av) 
#'
#' # nested new columns
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(a = data.frame(cases_2dav = mean(cases),
#'                            cases_2dma = mad(cases)),
#'             before = 1, as_list_col = TRUE)
epi_slide = function(x, f, ..., before, after, ref_time_values,
                     time_step, 
                     new_col_name = "slide_value", as_list_col = FALSE,
                     names_sep = "_", all_rows = FALSE) { 

  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) Abort("`x` must be of class `epi_df`.")
  
  if (missing(ref_time_values)) {
    ref_time_values = unique(x$time_value)
  }
  
  # Some of these `ref_time_values` checks and processing steps also apply to
  # the `ref_time_values` default; for simplicity, just apply all the steps
  # regardless of whether we are working with a default or user-provided
  # `ref_time_values`:
  if (length(ref_time_values) == 0L) {
    Abort("`ref_time_values` must have at least one element.")
  } else if (any(is.na(ref_time_values))) {
    Abort("`ref_time_values` must not include `NA`.")
  } else if (anyDuplicated(ref_time_values) != 0L) {
    Abort("`ref_time_values` must not contain any duplicates; use `unique` if appropriate.")
  } else if (!all(ref_time_values %in% unique(x$time_value))) {
    Abort("All `ref_time_values` must appear in `x$time_value`.")
  } else {
    ref_time_values = sort(ref_time_values)
  }
  
  # Validate and pre-process `before`, `after`:
  if (!missing(before)) {
    before <- vctrs::vec_cast(before, integer())
    if (length(before) != 1L || is.na(before) || before < 0L) {
      Abort("`before` must be length-1, non-NA, non-negative")
    }
  }
  if (!missing(after)) {
    after <- vctrs::vec_cast(after, integer())
    if (length(after) != 1L || is.na(after) || after < 0L) {
      Abort("`after` must be length-1, non-NA, non-negative")
    }
  }
  if (missing(before)) {
    if (missing(after)) {
      Abort("Either or both of `before`, `after` must be provided.")
    } else if (after == 0L) {
      Warn("`before` missing, `after==0`; maybe this was intended to be some
            non-zero-width trailing window, but since `before` appears to be
            missing, it's interpreted as a zero-width window (`before=0,
            after=0`).")
    }
    before <- 0L
  } else if (missing(after)) {
    if (before == 0L) {
      Warn("`before==0`, `after` missing; maybe this was intended to be some
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

  min_ref_time_values = ref_time_values - before
  min_ref_time_values_not_in_x <- min_ref_time_values[!(min_ref_time_values %in% unique(x$time_value))]

  # Do set up to let us recover `ref_time_value`s later.
  # A helper column marking real observations.
  x$.real = TRUE

  # Create df containing phony data. Df has the same columns and attributes as
  # `x`, but filled with `NA`s aside from grouping columns. Number of rows is
  # equal to the number of `min_ref_time_values_not_in_x` we have * the
  # number of unique levels seen in the grouping columns.
  before_time_values_df = data.frame(time_value=min_ref_time_values_not_in_x)
  if (length(group_vars(x)) != 0) {
    before_time_values_df = dplyr::cross_join(
      # Get unique combinations of grouping columns seen in real data.
      unique(x[, group_vars(x)]),
      before_time_values_df
    )
  }
  # Automatically fill in all other columns from `x` with `NA`s, and carry
  # attributes over to new df.
  before_time_values_df <- bind_rows(x[0,], before_time_values_df)
  before_time_values_df$.real <- FALSE

  x <- bind_rows(before_time_values_df, x)

  # Arrange by increasing time_value
  x = arrange(x, time_value)

  # Now set up starts and stops for sliding/hopping
  time_range = range(unique(x$time_value))
  starts = in_range(ref_time_values - before, time_range)
  stops = in_range(ref_time_values + after, time_range)
  
  if( length(starts) == 0 || length(stops) == 0 ) { 
    Abort("The starting and/or stopping times for sliding are out of bounds with respect to the range of times in your data. Check your settings for ref_time_values and align (and before, if specified).")
  }

  # Symbolize new column name
  new_col = sym(new_col_name)

  # Computation for one group, all time values
  slide_one_grp = function(.data_group,
                           f, ...,  
                           starts,
                           stops,
                           time_values,
                           all_rows,
                           new_col) {
    # Figure out which reference time values appear in the data group in the
    # first place (we need to do this because it could differ based on the
    # group, hence the setup/checks for the reference time values based on all
    # the data could still be off)
    o = time_values %in% .data_group$time_value
    starts = starts[o]
    stops = stops[o]
    time_values = time_values[o] 
    
    # Compute the slide values 
    slide_values_list = slider::hop_index(.x = .data_group,
                                          .i = .data_group$time_value,
                                          .f = f, ...,
                                          .starts = starts,
                                          .stops = stops)

    # Now figure out which rows in the data group are in the reference time
    # values; this will be useful for all sorts of checks that follow
    o = .data_group$time_value %in% time_values
    num_ref_rows = sum(o)
    
    # Count the number of appearances of each reference time value (these
    # appearances should all be real for now, but if we allow ref time values
    # outside of .data_group's time values):
    counts = .data_group %>%
      dplyr::filter(.data$time_value %in% time_values) %>%
      dplyr::count(.data$time_value) %>%
      dplyr::pull(n)

    if (!all(purrr::map_lgl(slide_values_list, is.atomic)) &&
          !all(purrr::map_lgl(slide_values_list, is.data.frame))) {
      Abort("The slide computations must return always atomic vectors or data frames (and not a mix of these two structures).")
    }

    # Unlist if appropriate:
    slide_values =
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
      slide_values = vctrs::vec_rep_each(slide_values, times = counts)
    } else {
      # Split and flatten if appropriate, perform a (loose) check on number of
      # rows.
      if (as_list_col) {
        slide_values = purrr::list_flatten(purrr::map(
          slide_values, ~ vctrs::vec_split(.x, seq_len(vctrs::vec_size(.x)))[["val"]]
        ))
      }
      if (vctrs::vec_size(slide_values) != num_ref_rows) {
        Abort("The slide computations must either (a) output a single element/row each, or (b) one element/row per appearance of the reference time value in the local window.")
      }
    }

    # If all rows, then pad slide values with NAs, else filter down data group
    if (all_rows) {
      orig_values = slide_values
      slide_values = vctrs::vec_rep(vctrs::vec_cast(NA, orig_values), nrow(.data_group))
      # ^ using vctrs::vec_init would be shorter but docs don't guarantee it
      # fills with NA equivalent.
      vctrs::vec_slice(slide_values, o) = orig_values
    } else {
      # This implicitly removes phony (`.real` == FALSE) observations.
      .data_group = filter(.data_group, o)
    }
    return(mutate(.data_group, !!new_col := slide_values))
  }

  # If f is not missing, then just go ahead, slide by group
  if (!missing(f)) {
    f = as_slide_computation(f, ...)
    f_rtv_wrapper = function(x, g, ...) {
      ref_time_value = min(x$time_value) + before
      x <- x[x$.real,]
      x$.real <- NULL
      f(x, g, ref_time_value, ...)
    }
    x = x %>%  
      group_modify(slide_one_grp,
                   f = f_rtv_wrapper, ...,
                   starts = starts,
                   stops = stops,
                   time_values = ref_time_values, 
                   all_rows = all_rows,
                   new_col = new_col,
                   .keep = FALSE)
  }
  
  # Else interpret ... as an expression for tidy evaluation
  else {
    quos = enquos(...)
    if (length(quos) == 0) {
      Abort("If `f` is missing then a computation must be specified via `...`.")
    }
    if (length(quos) > 1) {
      Abort("If `f` is missing then only a single computation can be specified via `...`.")
    }
    
    quo = quos[[1]]
    f = function(.x, .group_key, quo, ...) {
      .ref_time_value = min(.x$time_value) + before
      .x <- .x[.x$.real,]
      .x$.real <- NULL
      data_mask = rlang::as_data_mask(.x)
      # We'll also install `.x` directly, not as an `rlang_data_pronoun`, so
      # that we can, e.g., use more dplyr and epiprocess operations.
      data_mask$.x = .x
      data_mask$.group_key = .group_key
      data_mask$.ref_time_value = .ref_time_value
      rlang::eval_tidy(quo, data_mask)
    }
    new_col = sym(names(rlang::quos_auto_name(quos)))
    
    x = x %>%  
      group_modify(slide_one_grp,
                   f = f, quo = quo,
                   starts = starts,
                   stops = stops,
                   time_values = ref_time_values, 
                   all_rows = all_rows,
                   new_col = new_col,
                   .keep = FALSE)
  }
  
  # Unnest if we need to, and return
  if (!as_list_col) {
    x = unnest(x, !!new_col, names_sep = names_sep)
  }

  # Remove any remaining phony observations. When `all_rows` is TRUE, phony
  # observations aren't necessarily removed in `slide_one_grp`.
  if (all_rows) {
    x <- x[x$.real,]
  }

  # Drop helper column `.real`.
  x$.real <- NULL

  return(x)
}
