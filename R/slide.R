#' Slide a function over variables in an `epi_signal` object, grouped by geo
#' value
#'
#' Slides a given function over the variables in an `epi_signal` object, grouped
#' by geo value. See the [slide
#' vignette](https://cmu-delphi.github.io/epitools/articles/slide.html) for
#' examples.
#'
#' @details To "slide" means to apply a function or formula over a trailing
#'   window of `n` time steps, where the unit (the meaning of one time step) is
#'   determined by the `time_type` field in the metadata: it is one day if
#'   `time_type` is either "day-time" or "day", and it is one week if
#'   `time_type` is "week". The time step can also be set explicitly using the
#'   `time_step` argument (which would override the default choice based on the
#'   metadata).
#'
#' @param x The `epi_signal` object under consideration. 
#' @param slide_fun Function or formula to slide over variables in `x`, grouped
#'   by `geo_value`. To "slide" means to apply a function or formula over a
#'   trailing window of `n` time steps (where one time step is typically one day
#'   or one week; see details for more explanation). If a function, `slide_fun`
#'   must take `x`, a data frame with the same column names as the original
#'   object; followed by any number of named arguments; and ending with
#'   `...`. If a formula, `slide_fun` can operate directly on columns accessed
#'   via `.x$var`, as in `~ mean(.x$var)` to compute a trailing mean of a column
#'   `var` over the last `n` time steps.
#' @param n Number of time steps to use in the trailing window. For example, if 
#'   `n = 5`, and one time step is one day, then to produce a value on November
#'   5, we apply the given function or formula to data in between November 1 and
#'   5.  Default is 14.
#' @param new_var_name String indicating the name of the new variable that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_var_name` equal to an existing column name will overwrite this column.  
#' @param new_var_type One of "dbl", "int", "lgl", "chr", or "list", indicating 
#'   the data type (tibble abbreviation) for the new variable. Default is "dbl".   
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. This function must take a positive integer and return an object
#'   of class `lubridate::period`. For example, we can use `time_step =
#'   lubridate::hours` in order to set the time step to be one hour (this would
#'   only be meaningful if `time_value` is of class `POSIXct`, that is, if
#'   `time_type` if "day-time").
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `slide_fun`.  
#' @return An `epi_signal` object given by appending a new column to `x`, named 
#'   according to the `new_var_name` argument, containing the slide values. 
#'
#' @importFrom dplyr arrange group_by group_modify mutate relocate ungroup
#' @importFrom lubridate days weeks
#' @importFrom rlang .data abort
#' @export
slide_by_geo = function(x, slide_fun, n = 14, new_var_name = "slide_value",
                        new_var_type = c("dbl", "int", "lgl", "chr", "list"),
                        time_step, ...) { 
  # Check we have an `epi_signal` object
  if (!inherits(x, "epi_signal")) abort("`x` be of class `epi_signal`.")

  # Which slide_index function?
  new_var_type = match.arg(new_var_type)
  slide_index_zzz = switch(new_var_type,
                           "dbl" = slider::slide_index_dbl,
                           "int" = slider::slide_index_int,
                           "lgl" = slider::slide_index_lgl,
                           "chr" = slider::slide_index_chr,
                           "list" = slider::slide_index)

  # What is one time step?
  if (!missing(time_step)) before_fun = time_step
  else if (attributes(x)$metadata$time_type == "week") before_fun = weeks
  else before_fun = days # Use days for time_type = "day" or "day-time"

  # Slide over a single geo value
  slide_one_geo = function(.data_group, slide_fun, n, new_var_name, ...) { 
    slide_values = slide_index_zzz(.x = .data_group,
                                   .i = .data_group$time_value,
                                   .f = slide_fun, ..., 
                                   .before = before_fun(n-1))
    return(mutate(.data_group, !!new_var_name := slide_values))
  }

  # Save the class and attributes, since dplyr drops them
  cls = class(x)
  metadata = attributes(x)$metadata

  # Apply slide function per group and return
  x = x %>%  
    group_by(geo_value) %>%
    arrange(time_value) %>%
    group_modify(slide_one_geo, slide_fun = slide_fun,
                 n = n, new_var_name = new_var_name, ...) %>%
    ungroup() %>%
    relocate(.data$geo_value, .data$time_value)
  
  # Attach the class and metadata and return
  class(x) = cls
  attributes(x)$metadata = metadata
  return(x)
}
