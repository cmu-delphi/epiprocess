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
#'   a function, `f` must take `x`, a data frame with the same column names as
#'   the original object, minus any grouping variables, containing the time
#'   window data for one `ref_time_value`-group combination; followed by `g`, a
#'   one-row tibble containing the values of the grouping variables for the
#'   associated group; followed by any number of named arguments. If a formula,
#'   `f` can operate directly on columns accessed via `.x$var`, as in `~
#'   mean(.x$var)` to compute a mean of a column `var` for each
#'   `ref_time_value`-group combination. If `f` is missing, then `...` will
#'   specify the computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the `...` is interpreted as
#'   an expression for tidy evaluation. See details.
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
#' @param as_list_col If the computations return data frames, should the slide
#'   result hold these in a single list column or try to unnest them? Default is
#'   `FALSE`, in which case a list object returned by `f` would be unnested
#'   (using [`tidyr::unnest()`]), and the names of the resulting columns are given
#'   by prepending `new_col_name` to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_rows If `all_rows = TRUE`, then all rows of `x` will be kept in
#'   the output; otherwise, there will be one row for each time value in `x`
#'   that acts as a reference time value. Default is `FALSE`.
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
#' @importFrom rlang .data .env !! enquo enquos sym
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

  # Check that `f` takes enough args
  if (!missing(f) && is.function(f)) {
    # We need `args` here to work properly on primitive functions
    arg_names = names(formals(args(f)))
    if ("..." %in% arg_names) {
      # Keep all arg names before `...`
      dots_i <- which(arg_names == "...")
      arg_names <- arg_names[seq_len(dots_i - 1)]
    }
    if (length(arg_names) < 2) {
      Abort("`f` must take at least 2 arguments",
          class="epiprocess__epi_slide__f_must_take_at_least_2_args",
          epiprocess__f = f,
          epiprocess__arg_names = arg_names)
    }
  }

  # Arrange by increasing time_value
  x = arrange(x, time_value)
  
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
    slide_values = slider::hop_index(.x = .data_group,
                                     .i = .data_group$time_value,
                                     .f = f, ...,
                                     .starts = starts,
                                     .stops = stops)

    # Now figure out which rows in the data group are in the reference time
    # values; this will be useful for all sorts of checks that follow
    o = .data_group$time_value %in% time_values
    num_ref_rows = sum(o)
    
    # Count the number of appearances of each reference time value
    counts = .data_group %>%
      dplyr::filter(.data$time_value %in% time_values) %>%
      dplyr::count(.data$time_value) %>%
      dplyr::pull(n)

    # If they're all atomic vectors 
    if (all(sapply(slide_values, is.atomic))) {
      if (all(sapply(slide_values, length) == 1)) {
        # Recycle to make size stable (one slide value per ref time value)
        slide_values = rep(unlist(slide_values), times = counts)
      }
      else {
        # Unlist, then check its length, and abort if not right
        slide_values = unlist(slide_values)
        if (length(slide_values) != num_ref_rows) {
          Abort("If the slide computations return atomic vectors, then they must each have a single element, or else one element per appearance of the reference time value in the local window.")
        }
      }
    }
      
    # If they're all data frames
    else if (all(sapply(slide_values, is.data.frame))) {
      if (all(sapply(slide_values, nrow) == 1)) {
        # Recycle to make size stable (one slide value per ref time value)
        slide_values = rep(slide_values, times = counts)
      }
      else {
        # Split (each row on its own), check length, abort if not right
        slide_df = dplyr::bind_rows(slide_values)
        slide_values = split(slide_df, 1:nrow(slide_df))
        if (length(slide_values) != num_ref_rows) {
          Abort("If the slide computations return data frames, then they must each have a single row, or else one row per appearance of the reference time value in the local window.")
        }
      }
    }
      
    # If neither all atomic vectors or all data frames, then abort 
    else {
      Abort("The slide computations must return always atomic vectors or data frames (and not a mix of these two structures).")
    }      
    
    # If all rows, then pad slide values with NAs, else filter down data group
    if (all_rows) {
      orig_values = slide_values
      slide_values = rep(NA, nrow(.data_group))
      slide_values[o] = orig_values
    }
    else .data_group = filter(.data_group, o)
    return(mutate(.data_group, !!new_col := slide_values))
  }

  # If f is not missing, then just go ahead, slide by group
  if (!missing(f)) {
    x = x %>%  
      group_modify(slide_one_grp,
                   f = f, ...,
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
    f = function(x, quo, ...) rlang::eval_tidy(quo, x)
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
  return(x)
}
