#' @param .x An `epi_df` object. If ungrouped, we temporarily group by `geo_value`
#'   and any columns in `other_keys`. If grouped, we make sure the grouping is by
#'   `geo_value` and `other_keys`.
#' @param .window_size The size of the sliding window. The accepted values
#' depend on the type of the `time_value` column in `.x`:
#'
#'   - if time type is `Date` and the cadence is daily, then `.window_size` can
#'     be an integer (which will be interpreted in units of days) or a difftime
#'     with units "days"
#'   - if time type is `Date` and the cadence is weekly, then `.window_size` must
#'     be a `difftime` with units "weeks"
#'   - if time type is a `yearmonth` or an integer, then `.window_size` must be an
#'     integer
#'
#' @param .align The alignment of the sliding window.
#'
#'   - If "right" (default), then the window has its end at the reference time.
#'   This is likely the most common use case, e.g. `.window_size=7` and
#'   `.align="right"` slides over the past week of data.
#'   - If "left", then the window has its start at the reference time.
#'   - If "center", then the window is centered at the reference time. If the
#'   window size is odd, then the window will have floor(window_size/2) points
#'   before and after the reference time; if the window size is even, then the
#'   window will be asymmetric and have one more value before the reference time
#'   than after.
#'
#' @param .ref_time_values The time values at which to compute the slides
#'   values. By default, this is all the unique time values in `.x`.
#' @param .all_rows If `.all_rows = FALSE`, the default, then the output
#'   `epi_df` will have only the rows that had a `time_value` in
#'   `.ref_time_values`. Otherwise, all the rows from `.x` are included by with
#'   a missing value marker (typically NA, but more technically the result of
#'   `vctrs::vec_cast`-ing `NA` to the type of the slide computation output).
#' @return An `epi_df` object with one or more new slide computation columns
#'   added. It will be ungrouped if `.x` was ungrouped, and have the same groups
#'   as `.x` if `.x` was grouped.
