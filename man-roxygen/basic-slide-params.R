#' @param x The `epi_df` object under consideration, [grouped][dplyr::group_by]
#'   or ungrouped. If ungrouped, all data in `x` will be treated as part of a
#'   single data group.
#' @param before,after How far `before` and `after` each `ref_time_value` should
#'   the sliding window extend? At least one of these two arguments must be
#'   provided; the other's default will be 0. The accepted values for these
#'   depend on the type of the `time_value` column:
#'
#'   - if it is a Date and the cadence is daily, then they can be integers
#'     (which will be interpreted in units of days) or difftimes with units
#'     "days"
#'   - if it is a Date and the cadence is weekly, then they must be difftimes
#'     with units "weeks"
#'   - if it is an integer, then they must be integers
#'
#'   Endpoints of the window are inclusive. Common settings:
#'
#'   - For trailing/right-aligned windows from `ref_time_value - k` to
#'     `ref_time_value`: either pass `before=k` by itself, or pass `before=k,
#'     after=0`.
#'   - For center-aligned windows from `ref_time_value - k` to
#'     `ref_time_value + k`: pass `before=k, after=k`.
#'   - For leading/left-aligned windows from `ref_time_value` to
#'     `ref_time_value + k`: either pass pass `after=k` by itself,
#'     or pass `before=0, after=k`.
#'
#'   See "Details:" on how missing rows are handled within the window.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_rows If `all_rows = TRUE`, then all rows of `x` will be kept in
#'   the output even with `ref_time_values` provided, with some type of missing
#'   value marker for the slide computation output column(s) for `time_value`s
#'   outside `ref_time_values`; otherwise, there will be one row for each row in
#'   `x` that had a `time_value` in `ref_time_values`. Default is `FALSE`. The
#'   missing value marker is the result of `vctrs::vec_cast`ing `NA` to the type
#'   of the slide computation output. If using `as_list_col = TRUE`, note that
#'   the missing marker is a `NULL` entry in the list column; for certain
#'   operations, you might want to replace these `NULL` entries with a different
#'   `NA` marker.
#' @return An `epi_df` object given by appending one or more new columns to
#'  `x`, named according to the `new_col_name` argument.
