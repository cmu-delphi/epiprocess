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
#'   explanation). If a function, `f` must take `x`, a data frame with the same
#'   column names as the original object; followed by any number of named
#'   arguments; and ending with `...`. If a formula, `f` can operate directly on
#'   columns accessed via `.x$var`, as in `~ mean(.x$var)` to compute a mean of
#'   a column `var` over a sliding window of `n` time steps.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the current argument is
#'   interpreted as an expression for tidy evaluation. See details.  
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 7`, one time step is one day, and the alignment is "right", then to
#'   produce a value on January 7 we apply the given function or formula to data
#'   in between January 1 and 7. Default is 7.
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
#' @param complete Should the slide function be run over complete windows only?
#'   Default is `FALSE`, which allows for computation on partial windows.  
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the new column be stored as a list column? Default
#'   is `FALSE`, in which case a list object returned by `f` would be unnested 
#'   (using `tidyr::unnest()`), and the names of the resulting columns are given
#'   by prepending `new_col_name` to the names of the list elements. 
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a positive integer and return
#'   an object of class `lubridate::period`. For example, we can use `time_step
#'   = lubridate::hours` in order to set the time step to be one hour (this
#'   would only be meaningful if `time_value` is of class `POSIXct`).
#' @return An `epi_df` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the slide values. 
#' 
#' @details To "slide" means to apply a function or formula over a running
#'   window of `n` time steps, where the unit (the meaning of one time step) is
#'   implicitly defined by the way the `time_value` column treats addition and
#'   subtraction; for example, if the time values are coded as `Date` objects,
#'   then one time step is one day, since `as.Date("2022-01-01") + 1` equals
#'   `as.Date("2022-01-02")`. Alternatively, the time step can be set explicitly
#'   using the `time_step` argument (which if specified would override the
#'   default choice based on `time_value` column).
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
#' @importFrom rlang !! abort enquo enquos 
#' @export
epi_slide = function(x, f, ..., n = 7, align = c("right", "center", "left"),
                     before, complete = FALSE, new_col_name = "slide_value", 
                     as_list_col = FALSE, time_step) {
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) abort("`x` must be of class `epi_df`.")
  
  # If before is missing, then use align to set up alignment
  if (missing(before)) {
    align = match.arg(align)
    if (align == "right") {
      before_num = n-1
      after_num = 0
    }
    else if (align == "center") {
      before_num = floor((n-1)/2)
      after_num = ceiling((n-1)/2)
    }
    else {
      before_num = 0
      after_num = n-1
    }
  }
  
  # Otherwise set up alignment based on passed before value
  else {
    if (before < 0 || before > n-1) {
      abort("`before` must be in between 0 and n-1`.")
    }

    before_num = before
    after_num = n-1-before
  }

  # If a custom time step is specified, then redefine units 
  if (!missing(time_step)) {
    before_num = time_step(before_num)
    after_num = time_step(after_num)
  }

  # Convenience function for sliding over just one group
  slide_one_grp = function(.data_group,
                           f, ...,
                           before_num,
                           after_num,
                           complete,
                           new_col_name) {
    slide_values = slider::slide_index(.x = .data_group,
                                       .i = .data_group$time_value,
                                       .f = f, ..., 
                                       .before = before_num,
                                       .after = after_num,
                                       .complete = complete)

    return(mutate(.data_group, !!new_col_name := slide_values))
  }

  # Arrange by increasing time_value, else slide may not work
  x = arrange(x, time_value)

  # If f is not missing, then just go ahead, slide by group
  if (!missing(f)) {
    x = x %>%  
      group_modify(slide_one_grp,
                   f = f, ...,
                   before_num = before_num,
                   after_num = after_num,
                   complete = complete,
                   new_col_name = new_col_name)
  }
  
  # Else interpret ... as an expression for tidy evaluation
  else {
    quos = enquos(...)
    if (length(quos) == 0) {
      abort("If `f` is missing then a computation must be specified via `...`.")
    }
    if (length(quos) > 1) {
      abort("If `f` is missing then only a single computation can be specified via `...`.")
    }
    
    quo = quos[[1]]
    f = function(x, quo, ...) rlang::eval_tidy(quo, x)
    new_col_name = names(rlang::quos_auto_name(quos))
    
    x = x %>%  
      group_modify(slide_one_grp,
                   f = f, quo = quo,
                   before_num = before_num,
                   after_num = after_num,
                   complete = complete,
                   new_col_name = new_col_name)
  }

  # Unnest if we need to, and return
  if (!as_list_col) x = tidyr::unnest(x, !!new_col_name, names_sep = "_")
  return(x)
}
