#' Slide a function over variables in an `epi_tibble` object
#'
#' Slides a given function over the variables in an `epi_tibble` object. See the
#' [slide vignette](https://cmu-delphi.github.io/epitools/articles/slide.html)
#' for examples.
#'
#' @details To "slide" means to apply a function or formula over a running
#'   window of `n` time steps, where the unit (the meaning of one time step) is
#'   determined by the `time_type` field in the metadata: the unit is one day if
#'   `time_type` is either "day-time" or "day", and the unit is one week if
#'   `time_type` is "week". The time step can also be set explicitly using the
#'   `time_step` argument (which if specified would override the default choice
#'   based on the metadata).
#'
#' @param x The `epi_tibble` object under consideration.
#' @param slide_fun Function or formula to slide over variables in `x`. To
#'   "slide" means to apply a function or formula over a running window of `n`
#'   time steps (where one time step is typically one day or one week; see
#'   details for more explanation). If a function, `slide_fun` must take `x`, a
#'   data frame with the same column names as the original object; followed by
#'   any number of named arguments; and ending with `...`. If a formula,
#'   `slide_fun` can operate directly on columns accessed via `.x$var`, as in `~
#'   mean(.x$var)` to compute a mean of a column `var` over a sliding window of
#'   `n` time steps.
#' @param n Number of time steps to use in the window. For example, if `n = 5`,
#'   one time step is one day, and the alignment is "trailing", then to produce
#'   a value on November 5, we apply the given function or formula to data in
#'   between November 1 and 5. Default is 14.
#' @param align String specifying the alignment of the sliding window relative
#'   to the reference time point; either "trailing" or "centered". The default
#'   is "trailing". If the alignment is "centered" and `n` is even, then one
#'   more observation will be used before the reference time than after the
#'   reference time. 
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
#' @return An `epi_tibble` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the slide values. 
#'
#' @details In order to slide a function or formula in a suitable grouped
#'   fashion (for example, per geo value), we can use `group_by()` before the
#'   call to `epi_slide()`.
#' 
#' @importFrom dplyr arrange group_modify mutate relocate 
#' @importFrom lubridate days weeks
#' @importFrom rlang .data abort enquo
#' @export
epi_slide = function(x, slide_fun, n = 14, align = c("trailing", "centered"),
                     new_col_name = "slide_value",
                     new_col_type = c("dbl", "int", "lgl", "chr", "list"),
                     time_step, ...) { 
  # Check we have an `epi_tibble` object
  if (!inherits(x, "epi_tibble")) abort("`x` be of class `epi_tibble`.")

  # Which slide_index function?
  new_col_type = match.arg(new_col_type)
  slide_index_zzz = switch(new_col_type,
                           "dbl" = slider::slide_index_dbl,
                           "int" = slider::slide_index_int,
                           "lgl" = slider::slide_index_lgl,
                           "chr" = slider::slide_index_chr,
                           "list" = slider::slide_index)

  # What is one time step?
  if (!missing(time_step)) before_fun = time_step
  else if (attributes(x)$metadata$time_type == "week") before_fun = weeks
  else before_fun = days # Use days for time_type = "day" or "day-time"

  # Validate align and get number of time steps before/after 
  align = match.arg(align)
  if (align == "trailing") {
    before_num = before_fun(n - 1)
    after_num = 0
  } else {
    before_num = before_fun(ceiling((n - 1) / 2))
    after_num = before_fun(floor((n - 1) / 2))
  }

  # Slide over a single group
  slide_one_grp = function(.data_group, slide_fun, n, new_col_name, ...) { 
    slide_values = slide_index_zzz(.x = .data_group,
                                   .i = .data_group$time_value,
                                   .f = slide_fun, ..., 
                                   .before = before_num,
                                   .after = after_num)
    return(mutate(.data_group, !!new_col_name := slide_values))
  }

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata 
  
  # Slide per group (in case x is grouped) 
  x = x %>%  
    group_modify(slide_one_grp, slide_fun = slide_fun,
                 n = n, new_col_name = new_col_name, ...) %>%
    relocate(.data$geo_value, .data$time_value)

  # Attach the class and metadata and return
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}
