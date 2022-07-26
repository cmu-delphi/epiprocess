#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' @param x The `epi_df` object under consideration.
#' @param f Function or formula to slide over variables in `x`. To "slide" means
#'   to apply a function or formula over a running window of `n` time steps
#'   (where one time step is typically one day or one week; see details for more
#'   explanation). If a function, `f` should take `x`, an `epi_df` with the same 
#'   names as the non-grouping columns, followed by `g` to refer to the one row 
#'   tibble with one column per grouping variable that identifies the group, 
#'   and any number of named arguments (which will be taken from `...`). If a 
#'   formula, `f` can operate directly on columns accessed via `.x$var`, as 
#'   in `~ mean(.x$var)` to compute a mean of a column var over a sliding 
#'   window of n time steps. As well, `.y` may be used in the formula to refer 
#'   to the groupings that would be described by `g` if `f` was a function.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the current argument is
#'   interpreted as an expression for tidy evaluation. See details.  
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 7`, one time step is one day, and the alignment is "right", then to
#'   produce a value on January 7 we apply the given function or formula to data
#'   in between January 1 and 7. Default is 7.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param align One of "right", "center", or "left", indicating the alignment of
#'   the sliding window relative to the reference time point. If the alignment
#'   is "center" and `n` is even, then one more time point will be used after
#'   the reference time point than before. Default is "right".
#' @param before Positive integer less than `n`, specifying the number of time
#'   points to use in the sliding window strictly before the reference time
#'   point. For example, setting `before = n-1` would be the same as setting
#'   `align = "right"`. The `before` argument allows for more flexible
#'   specification of alignment than the `align` parameter, and if specified,
#'   overrides `align`. 
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a positive integer and return
#'   an object of class `lubridate::period`. For example, we can use `time_step
#'   = lubridate::hours` in order to set the time step to be one hour (this
#'   would only be meaningful if `time_value` is of class `POSIXct`).
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the new column be stored as a list column? Default
#'   is `FALSE`, in which case a list object returned by `f` would be unnested 
#'   (using `tidyr::unnest()`), and the names of the resulting columns are given
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
#' @details To "slide" means to apply a function or formula over a running
#'   window of `n` time steps, where the unit (the meaning of one time step) is
#'   implicitly defined by the way the `time_value` column treats addition and
#'   subtraction; for example, if the time values are coded as `Date` objects,
#'   then one time step is one day, since `as.Date("2022-01-01") + 1` equals
#'   `as.Date("2022-01-02")`. Alternatively, the time step can be set explicitly
#'   using the `time_step` argument (which if specified would override the
#'   default choice based on `time_value` column). If less than `n` time steps
#'   are available at any given reference time value, then `epi_slide()` still
#'   attempts to perform the computation anyway (it does not require a complete
#'   window). The issue of what to do with partial computations (those run on
#'   incomplete windows) is therefore left up to the user, either through the
#'   specified function or formula `f`, or through post-processing.  
#' 
#' If `f` is missing, then an expression for tidy evaluation can be specified,
#'   for example, as in: 
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), n = 7)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, function(x, ...) mean(x$cases), n = 7,
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
#'  # slide a 7-day trailing average formula on cases
#'   jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), n = 7, 
#'             align = "right") %>% 
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av) 
#'  
#'  # slide a left-aligned 7-day average
#'   jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), n = 7, 
#'             align = "left") %>% 
#'   # rmv a nonessential var. to ensure new col is printed
#'   dplyr::select(-death_rate_7d_av) 
#'  
#'  # nested new columns
#'  jhu_csse_daily_subset %>% 
#'  group_by(geo_value) %>%
#'  epi_slide(a = data.frame(cases_2dav = mean(cases), 
#'                           cases_2dma = mad(cases)),
#'            n = 2, as_list_col = TRUE)
epi_slide = function(x, f, ..., before, after, ref_time_values, time_step, 
                     new_col_name = "slide_value", as_list_col = FALSE,
                     names_sep = "_", all_rows = FALSE) { 
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) Abort("`x` must be of class `epi_df`.")
  
  # Arrange by increasing time_value
  x = arrange(x, time_value)
  
  # If missing, then set ref time values to be everything; else make sure we
  # intersect with observed time values
  if (missing(ref_time_values)) {
    ref_time_values = unique(x$time_value)
  } 
  else {
    ref_time_values = ref_time_values[ref_time_values %in%
                                      unique(x$time_value)] 
  }
  
  # Otherwise set up alignment based on passed before value
  if (before < 0 ||after < 0) {
    Abort("`before` and `after` must be at least 0.")
  }
  
  if (floor(before) < ceiling(before) || floor(after) < ceiling(after)) {
    Abort("`before` and `after` must be integers.")
  }
  
  before_num = before
  after_num = after

  # If a custom time step is specified, then redefine units 
  if (!missing(time_step)) {
    before_num = time_step(before_num)
    after_num = time_step(after_num)
  }

  # Now set up starts and stops for sliding/hopping
  time_range = range(unique(x$time_value))
  starts = in_range(ref_time_values - before_num, time_range)
  stops = in_range(ref_time_values + after_num, time_range)
  
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
