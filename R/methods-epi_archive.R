#' Generate a snapshot from an `epi_archive` object
#'
#' Generates a snapshot in `epi_df` format from an `epi_archive` object, as of a
#' given version. See the [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x An `epi_archive` object
#' @param max_version Time value specifying the max version to permit in the
#'   snapshot. That is, the snapshot will comprise the unique rows of the
#'   current archive data that represent the most up-to-date signal values, as
#'   of the specified `max_version` (and whose time values are at least
#'   `min_time_value`.)
#' @param min_time_value Time value specifying the min time value to permit in
#'   the snapshot. Default is `-Inf`, which effectively means that there is no
#'   minimum considered.
#' @return An `epi_df` object.
#'
#' @details This is simply a wrapper around the `as_of()` method of the
#'   `epi_archive` class, so if `x` is an `epi_archive` object, then:
#'   ```
#'   epix_as_of(x, max_version = v)
#'   ```
#'   is equivalent to:
#'   ```
#'   x$as_of(max_version = v)
#'   ```
#'
#' @export
#' @examples
#' # warning message of data latency shown
#' epix_as_of(x = archive_cases_dv_subset,
#'            max_version = max(archive_cases_dv_subset$DT$version))
#' 
#' # no warning shown
#' epix_as_of(archive_cases_dv_subset, max_version = as.Date("2020-06-10"))          
epix_as_of = function(x, max_version) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$as_of(max_version))
}

#' Merge two `epi_archive` objects
#'
#' Merges the underlying data tables in two `epi_archive` objects, allows for
#' post-filling of `NA` values by last observation carried forward (LOCF), and
#' **overwrites** the first data table with the merged one. See the [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x,y Two `epi_archive` objects to join together, or more specifically,
#'   whose underlying data tables are to be joined together. The data table in
#'   `x` will be overwritten with the joined data table. For convenience, we
#'   also allow `y` to be passed in directly as a `data.table` (need not be an
#'   `epi_archive` object).
#' @param ... Named arguments to pass to `data.table::merge.data.table()`, which
#'   is used for the join (with all default settings as in this function). For
#'   example, passing `all = TRUE` will perform a full join.
#' @param locf Should LOCF be used after joining on all non-key columns? This
#'   will take the latest version of each signal value and propogate it forward
#'   to fill in gaps that appear after merging. Default is `TRUE`.
#' @param nan Should `NaN` values be treated as `NA` values in the post-filling
#' step?  Default is `NA`, which means that they are treated as `NA` values; if
#    `NaN`, then they are treated as distinct.
#' @return Nothing; the data table in `x` is overwritten with the merged one.
#'
#' @details This is simply a wrapper around the `merge()` method of the
#'   `epi_archive` class, so if `x` and `y` are an `epi_archive` objects, then:
#'   ```
#'   epix_merge(x, y)
#'   ```
#'   is equivalent to:
#'   ```
#'   x$merge(y)
#'   ```
#'
#' @export
#' @examples
#' # create two example epi_archive datasets
#' x <- archive_cases_dv_subset$DT %>%
#'   dplyr::select(geo_value,time_value,version,case_rate_7d_av) %>%
#'   as_epi_archive()
#' y <- archive_cases_dv_subset$DT %>%
#'   dplyr::select(geo_value,time_value,version,percent_cli) %>%
#'   as_epi_archive()
#'
#' # a full join stored in x
#' epix_merge(x, y, all = TRUE)
epix_merge = function(x, y, ..., locf = TRUE, nan = NA) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$merge(y, ..., locf = locf, nan = nan))
}

#' Slide a function over variables in an `epi_archive` object
#'
#' Slides a given function over variables in an `epi_archive` object. This
#' behaves similarly to `epi_slide()`, with the key exception that it is
#' version-aware: the sliding computation at any given reference time t is
#' performed on **data that would have been available as of t**. See the
#' [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x An `epi_archive` object.
#' @param f Function or formula to slide over variables in `x`. To "slide" means
#'   to apply a function or formula over a running window of `n` time steps
#'   (where one time step is typically one day or one week). If a function, `f`
#'   must take `x`, a data frame with the same column names as the original
#'   object; followed by any number of named arguments; and ending with
#'   `...`. If a formula, `f` can operate directly on columns accessed via
#'   `.x$var`, as in `~ mean(.x$var)` to compute a mean of a column `var` over a
#'   sliding window of `n` time steps.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the current argument is
#'   interpreted as an expression for tidy evaluation.
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 7`, and one time step is one day, then to produce a value on January 7
#'   we apply the given function or formula to data in between January 1 and
#'   7. Default is 7.
#' @param group_by The variable(s) to group by before slide computation. If
#'   missing, then the keys in the underlying data table, excluding `time_value`
#'   and `version`, will be used for grouping. To omit a grouping entirely, use
#'   `group_by = NULL`.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
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
#' @param all_rows If `all_rows = TRUE`, then the output will have one row per
#'   combination of grouping variables and unique time values in the underlying
#'   data table. Otherwise, there will be one row in the output for each time
#'   value in `x` that acts as a reference time value. Default is `FALSE`.
#' @return A tibble whose columns are: the grouping variables, `time_value`,
#'   containing the reference time values for the slide computation, and a
#'   column named according to the `new_col_name` argument, containing the slide
#'   values.
#'
#' @details Two key distinctions between inputs to the current function and
#'   `epi_slide()`:
#'   1. `epix_slide()` uses windows that are **always right-aligned** (in
#'   `epi_slide()`, custom alignments could be specified using the `align` or
#'   `before` arguments).
#'   2. `epix_slide()` uses a `group_by` to specify the grouping upfront (in
#'   `epi_slide()`, this would be accomplished by a preceding function call to
#'   `dplyr::group_by()`).
#' Apart from this, the interfaces between `epix_slide()` and `epi_slide()` are
#'   the same.
#'
#' Note that the outputs are a similar but different: `epix_slide()` only
#'   returns the grouping variables, `time_value`, and the new columns from
#'   sliding, whereas `epi_slide()` returns all original variables plus the new
#'   columns from sliding.
#'
#' Furthermore, the current function can be considerably slower than
#'   `epi_slide()`, for two reasons: (1) it must repeatedly fetch
#'   properly-versioned snapshots from the data archive (via its `as_of()`
#'   method), and (2) it performs a "manual" sliding of sorts, and does not
#'   benefit from the highly efficient `slider` package. For this reason, it
#'   should never be used in place of `epi_slide()`, and only used when
#'   version-aware sliding is necessary (as it its purpose).
#'
#' Finally, this is simply a wrapper around the `slide()` method of the
#'   `epi_archive` class, so if `x` is an `epi_archive` object, then:
#'   ```
#'   epix_slide(x, new_var = comp(old_var), n = 120)
#'   ```
#'   is equivalent to:
#'   ```
#'   x$slide(x, new_var = comp(old_var), n = 120)
#'   ```
#'
#' @importFrom rlang enquo
#' @export
#' @examples
#' # these dates are reference time points for the 3 day average sliding window
#' # The resulting epi_archive ends up including data averaged from:
#' # 0 day which has no results, for 2020-06-01
#' # 1 day, for 2020-06-02
#' # 2 days, for the rest of the results
#' # never 3 days dur to data latency
#' 
#' time_values <- seq(as.Date("2020-06-01"),
#'                       as.Date("2020-06-15"),
#'                       by = "1 day")
#' epix_slide(x = archive_cases_dv_subset,
#'            f = ~ mean(.x$case_rate),
#'            n = 3,
#'            group_by = geo_value,
#'            ref_time_values = time_values,
#'            new_col_name = 'case_rate_3d_av')
epix_slide = function(x, f, ..., n = 7, group_by, ref_time_values,
                      time_step, new_col_name = "slide_value",
                      as_list_col = FALSE, names_sep = "_", all_rows = FALSE) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$slide(f, ..., n = n,
                 group_by = enquo(group_by),
                 ref_time_values = ref_time_values,
                 time_step = time_step,
                 new_col_name = new_col_name,
                 as_list_col = as_list_col,
                 names_sep = names_sep,
                 all_rows = all_rows))
}


