#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples. Also applies to an `epi_archive` object; see the [issues
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/issues.html) for
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
#' The critical difference between sliding over an `epi_df` versus `epi_archive` 
#'   object is that, with the latter, TODO
#'
#' If `f` is missing, then an expression for tidy evaluation can be specified,
#'   for example, as in: 
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), n = 7)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, f = function(x, ...) mean(x$cases), n = 7,
#'             new_col_name = "cases_7dav")
#'   ```
#'   Thus, to be clear, when the computation is specified via an expression for
#'   tidy evaluation (first example, above), then the name for the new column is 
#'   inferred from the given expression and overrides any name passed explicitly 
#'   through the `new_col_name` argument.
#'
#' @param x The `epi_df` or `epi_archive` object under consideration.
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
#'   `n = 5`, one time step is one day, and the alignment is "right", then to 
#'   produce a value on November 5, we apply the given function or formula to
#'   data in between November 1 and 5. Default is 14. 
#' @param align One of "right", "center", or "left", indicating the alignment of
#'   the sliding window relative to the reference time point. If the alignment
#'   is "center" and `n` is even, then one more time point will be used after
#'   the reference time point than before. Default is "right".
#' @param before Positive integer less than `n`, specifying the number of time
#'   points to use in the sliding window strictly before the reference time
#'   point. For example, setting `before = n-1` would be the same as setting
#'   `align = "right"`. The current argument allows for more flexible
#'   specification of alignment than the `align` parameter, and if specified,
#'   overrides `align`.
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
#' @return An `epi_df` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the slide values. 
#'
#' @details In order to slide a function or formula in a suitable grouped
#'   fashion (for example, per geo value), we can use `group_by()` before the
#'   call to `epi_slide()`.
#' 
#' @export
epi_slide = function(x, f, ..., n = 14, align = c("right", "center", "left"),
                     before, complete = FALSE, new_col_name = "slide_value", 
                     new_col_type = c("dbl", "int", "lgl", "chr", "list"),
                     time_step) { 
  UseMethod("epi_slide")
}

#' @method epi_slide epi_df
#' @importFrom dplyr arrange group_modify mutate pull summarize 
#' @importFrom lubridate days weeks
#' @importFrom rlang abort enquo enquos
#' @export
epi_slide.epi_df = function(x, f, ..., n = 14,
                            align = c("right", "center", "left"), 
                            before, complete = FALSE,
                            new_col_name = "slide_value", 
                            new_col_type = c("dbl", "int", "lgl", "chr",
                                             "list"), 
                            time_step) { 
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
      before_num = before_fun(floor((n-1)/2))
      after_num = before_fun(ceiling((n-1)/2))
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

  # If f is not missing, then just go ahead, slide by group
  if (!missing(f)) {
    x = x %>%  
      group_modify(epi_slide_one_grp,
                   index_fun = index_fun,
                   f = f, ...,
                   before_num = before_num,
                   after_num = after_num,
                   complete = complete,
                   new_col_name = new_col_name)
  }
  
  # Else interpret ... as an expression for tidy evaluation
  else {
    quos = enquos(...)
    if (length(quos) == 0)
      abort("If `f` is missing then a computation must be specified via `...`.") 
    if (length(quos) > 1)
      abort(paste("If `f` is missing then only a single computation can be",
                  "specified via `...`."))
    
    quo = quos[[1]]
    f = function(x, quo, ...) rlang::eval_tidy(quo, x)
    new_col_name = names(rlang::quos_auto_name(quos))
    
    x = x %>%  
      group_modify(epi_slide_one_grp,
                   index_fun = index_fun,
                   f = f, quo = quo,
                   before_num = before_num,
                   after_num = after_num,
                   complete = complete,
                   new_col_name = new_col_name)
  }
  
  # Attach the class and metadata and return
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

# Slide over a single group
epi_slide_one_grp = function(.data_group, index_fun, f, ..., before_num,
                             after_num, complete, new_col_name) {
  slide_values = index_fun(.x = .data_group,
                           .i = .data_group$time_value,
                           .f = f, ..., 
                           .before = before_num,
                           .after = after_num,
                           .complete = complete)
  return(mutate(.data_group, !!new_col_name := slide_values))
}

#' @importFrom rlang .data !! !!! :=
#' @importFrom pipeR %>>%
#' @export
epi_slide.epi_archive = function(x, f, ..., n = 14,
                                 align = c("right", "center", "left"), 
                                 before, complete = FALSE,
                                 new_col_name = "slide_value", 
                                 new_col_type = c("dbl", "int", "lgl", "chr",
                                                  "list"), 
                                 time_step,
                                 issue_step,
                                 issue_to_max_time_value = identity,
                                 issue_range = range(x$issues_with_updates(),
                                                     x$max_issue())) {
  ## TODO test this.

  ## Which map function?
  new_col_type = match.arg(new_col_type)
  map_zzz = switch(new_col_type,
                           "dbl" = purrr::map_dbl,
                           "int" = purrr::map_int,
                           "lgl" = purrr::map_lgl,
                           "chr" = purrr::map_chr,
                           "list" = purrr::map)

  ## It'd be natural to write
  ## ```
  ## seq(issue_range[[1L]], issue_range[[2L]], issue_step(1))
  ## ```
  ## but this produces an error, as do `seq` approaches directly on `Period` objects.
  n.issues = diff(issue_range)
  issues = issue_range[[1L]] + issue_step(0:as.integer(lubridate::as.period(diff(issue_range))/issue_step(1L)))

  ## XXX use x$naming_info()$issue.colname?

  ## TODO detect skipped empty data, e.g., when trying to produce results
  ## expecting 0 initial report latency; warn, error, or produce results

  tibble::tibble(issue := issues) %>>%
    tibble::add_column(
              results.for.issue = . %>>%
                dplyr::pull(issue) %>>%
                purrr::map(function(issue) {
                  max_time_value = issue_to_max_time_value(issue)
                  min_time_value = max_time_value - time_step(n-1L)
                  x$epi_df_as_of(issue) %>>%
                    ## XXX we may want to `complete` the `time_value`s here
                    dplyr::filter(min_time_value <= time_value & time_value <= max_time_value) %>>%
                    dplyr::group_by(geo_value, !!!x$naming_info()$other.key.colnames) %>>%
                    dplyr::group_nest(.key="data") %>>%
                    ## tibble::add_column doesn't seem to work here for some reason
                    dplyr::mutate(!!new_col_name := map_zzz(.data[["data"]], slide_fun)) %>>%
                    tidyr::unnest(data)
                })
            ) %>>%
    tidyr::unnest(results.for.issue) %>>%
    dplyr::group_by(issue, geo_value, !!!x$naming_info()$other.key.colnames)
}
