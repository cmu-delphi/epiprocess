#' @param .x The `epi_df` object under consideration, [grouped][dplyr::group_by]
#'   or ungrouped. If ungrouped, all data in `.x` will be treated as part of a
#'   single data group.
#' @param .window_size The size of the sliding window. By default, this is 1,
#' meaning that only the current ref_time_value is included. The accepted values
#' here depend on the `time_value` column:
#'
#'   - if time_type is Date and the cadence is daily, then `.window_size` can be
#'     an integer (which will be interpreted in units of days) or a difftime
#'     with units "days"
#'   - if time_type is Date and the cadence is weekly, then `.window_size` must
#'     be a difftime with units "weeks"
#'   - if time_type is an yearmonth or integer, then `.window_size` must be an
#'     integer
#'
#' @param .align The alignment of the sliding window. If `right` (default), then
#' the window has its end at the reference time; if `center`, then the window is
#' centered at the reference time; if `left`, then the window has its start at
#' the reference time. If the alignment is `center` and the window size is odd,
#' then the window will have floor(window_size/2) points before and after the
#' reference time. If the window size is even, then the window will be
#' asymmetric and have one less value on the right side of the reference time
#' (assuming time increases from left to right).
#' @param .ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param .all_rows If `.all_rows = TRUE`, then all rows of `.x` will be kept in
#'   the output even with `.ref_time_values` provided, with some type of missing
#'   value marker for the slide computation output column(s) for `time_value`s
#'   outside `.ref_time_values`; otherwise, there will be one row for each row in
#'   `.x` that had a `time_value` in `.ref_time_values`. Default is `FALSE`. The
#'   missing value marker is the result of `vctrs::vec_cast`ing `NA` to the type
#'   of the slide computation output.
#' @return An `epi_df` object given by appending one or more new columns to `.x`,
#'   named according to the `.new_col_name` argument.
