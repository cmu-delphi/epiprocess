#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the [slide 
#' vignette](https://cmu-delphi.github.io/epitools/articles/slide.html) for
#' examples.
#'
#' @details To "slide" means to apply a function or formula over a running
#'   window of `n` time steps, where the unit (the meaning of one time step) is
#'   determined by the `time_type` field in the metadata: the unit is one day if
#'   `time_type` is either "day-time" or "day", and the unit is one week if
#'   `time_type` is "week". The time step can also be set explicitly using the
#'   `time_step` argument (which if specified would override the default choice
#'   based on the metadata).
#'
#' @param x The `epi_df` object under consideration.
#' @param slide_fun Function or formula to slide over variables in `x`. To
#'   "slide" means to apply a function or formula over a running window of `n`
#'   time steps (where one time step is typically one day or one week; see
#'   details for more explanation). If a function, `slide_fun` must take `x`, a
#'   data frame with the same column names as the original object; followed by
#'   any number of named arguments; and ending with `...`. If a formula,
#'   `slide_fun` can operate directly on columns accessed via `.x$var`, as in `~
#'   mean(.x$var)` to compute a mean of a column `var` over a sliding window of
#'   `n` time steps.
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 5`, one time step is one day, and the alignment is "right", then to 
#'   produce a value on November 5, we apply the given function or formula to
#'   data in between November 1 and 5. Default is 14. 
#' @param align One of "right", "center", or "left", indicating the alignment of
#'   the sliding window relative to the reference time point. If the alignment
#'   is "center" and `n` is even, then one more time point will be used before
#'   the reference time point than after. Default is "right".
#' @param before Optional integer, in the range to 0 to `n-1` (inclusive),
#'   specifying the number of time points to use in the sliding window strictly
#'   before the reference time point. For example, setting `before = n-1` would
#'   be the same as setting `align = "right"`. The current argument allows for
#'   more flexible specification of alignment than the `align` parameter, and if
#'   specified, then it overrides `align`.
#' @param complete Should the slide function be run over complete windows only?
#'   Default is `FALSE`, which allows for computation on partial windows.  
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param new_col_type One of "dbl", "int", "lgl", "chr", or "list", indicating
#'   the data type (tibble abbreviation) for the new column. Default is "dbl".
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. This function must take a positive integer and return an object
#'   of class `lubridate::period`. For example, we can use `time_step =
#'   lubridate::hours` in order to set the time step to be one hour (this would
#'   only be meaningful if `time_value` is of class `POSIXct`, that is, if
#'   `time_type` is "day-time").
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `slide_fun`.  
#' @return An `epi_df` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the slide values. 
#'
#' @details In order to slide a function or formula in a suitable grouped
#'   fashion (for example, per geo value), we can use `group_by()` before the
#'   call to `epi_slide()`.
#' 
#' @importFrom dplyr arrange group_modify mutate  
#' @importFrom lubridate days weeks
#' @importFrom rlang .data abort enquo
#' @export
epi_slide = function(x, slide_fun, n = 14, align = c("right", "center", "left"),
                     before, complete = FALSE, new_col_name = "slide_value",
                     new_col_type = c("dbl", "int", "lgl", "chr", "list"),
                     time_step, ...) { 
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) abort("`x` must be of class `epi_df`.")

  # Which slide_index function?
  new_col_type = match.arg(new_col_type)
  index_fun = switch(new_col_type,
                     "dbl" = slider::slide_index_dbl,
                     "int" = slider::slide_index_int,
                     "lgl" = slider::slide_index_lgl,
                     "chr" = slider::slide_index_chr,
                     "list" = slider::slide_index)

  # What is one time step?
  if (!missing(time_step)) before_fun = time_step
  else if (attributes(x)$metadata$time_type == "week") before_fun = weeks
  else before_fun = days # Use days for time_type = "day" or "day-time"

  # If before is missing, then use align to set up alignment
  if (missing(before)) {
    align = match.arg(align)
    if (align == "right") {
      before_num = before_fun(n-1)
      after_num = 0
    }
    else if (align == "center") {
      before_num = before_fun(ceiling((n-1)/2))
      after_num = before_fun(floor((n-1)/2))
    }
    else {
      before_num = 0
      after_num = before_fun(n-1)
    }
  }
  
  # Otherwise set up alignment based on passed before value
  else {
    if (before < 0 || before > n-1)
      abort("`before` must be in between 0 and n-1`.")

    before_num = before_fun(before)
    after_num = before_fun(n-1-before)
  }

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata 
  
  # Slide per group (in case x is grouped) 
  x = x %>%  
    group_modify(epi_slide_one_grp,
                 index_fun = index_fun,
                 slide_fun = slide_fun,
                 before_num = before_num,
                 after_num = after_num,
                 complete = complete, 
                 new_col_name = new_col_name,
                 ...)

  # Attach the class and metadata and return
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

# Slide over a single group
epi_slide_one_grp = function(.data_group, index_fun, slide_fun, before_num, 
                             after_num, complete, new_col_name, ...) {  
  slide_values = index_fun(.x = .data_group,
                           .i = .data_group$time_value,
                           .f = slide_fun, ..., 
                           .before = before_num,
                           .after = after_num,
                           .complete = complete)
  return(mutate(.data_group, !!new_col_name := slide_values))
}
