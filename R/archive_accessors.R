# Internal accessor layer for epi_archive data storage.
#
# These functions abstract over the underlying storage representation so
# algorithms can work over multiple backends (data.table, duckplyr, ...).
# Each accessor is an S3 generic dispatching on the archive's backend
# subclass tag (e.g. `epi_archive_dt`). Internal code should prefer these
# accessors over direct `$DT` (or any other backend-specific field).
#
# All functions here are internal (not exported).

#' Get the archive's data as a tibble
#'
#' @param x an `epi_archive`
#' @return a tibble
#' @keywords internal
#' @noRd
archive_tbl <- function(x) UseMethod("archive_tbl")

#' @export
archive_tbl.epi_archive_dt <- function(x) {
  tibble::as_tibble(x$DT)
}

#' Get the archive's data as a dplyr-compatible handle
#'
#' Returns the backend-native object that dplyr verbs operate on. For the
#' data.table backend this is a `dtplyr::lazy_dt`; for duckplyr it's a
#' duckplyr lazy frame. Algorithms should prefer this over `archive_tbl`.
#'
#' @param x an `epi_archive`
#' @return a dplyr-compatible handle
#' @keywords internal
#' @noRd
archive_data <- function(x) UseMethod("archive_data")

#' @export
archive_data.epi_archive_dt <- function(x) {
  dtplyr::lazy_dt(x$DT)
}

#' Get a single column from the archive's data
#'
#' @param x an `epi_archive`
#' @param col a string column name
#' @return a vector
#' @keywords internal
#' @noRd
archive_col <- function(x, col) UseMethod("archive_col")

#' @export
archive_col.epi_archive_dt <- function(x, col) {
  x$DT[[col]]
}

#' Get column names of the archive's data
#'
#' @param x an `epi_archive`
#' @return a character vector
#' @keywords internal
#' @noRd
archive_colnames <- function(x) UseMethod("archive_colnames")

#' @export
archive_colnames.epi_archive_dt <- function(x) {
  names(x$DT)
}

#' Get the number of rows in the archive's data
#'
#' @param x an `epi_archive`
#' @return an integer
#' @keywords internal
#' @noRd
archive_nrow <- function(x) UseMethod("archive_nrow")

#' @export
archive_nrow.epi_archive_dt <- function(x) {
  nrow(x$DT)
}

#' Get the number of columns in the archive's data
#'
#' @param x an `epi_archive`
#' @return an integer
#' @keywords internal
#' @noRd
archive_ncol <- function(x) UseMethod("archive_ncol")

#' @export
archive_ncol.epi_archive_dt <- function(x) {
  ncol(x$DT)
}

#' Check whether the archive's data has any duplicate rows by key
#'
#' @param x an `epi_archive`
#' @return `TRUE` if any key is duplicated, otherwise `FALSE`
#' @keywords internal
#' @noRd
archive_any_duplicated_key <- function(x) UseMethod("archive_any_duplicated_key")

#' @export
archive_any_duplicated_key.epi_archive_dt <- function(x) {
  anyDuplicated(x$DT, by = key_colnames(x)) > 0L
}

#' Deep-copy the archive's underlying data in place
#'
#' Returns a modified archive whose data storage is a deep copy,
#' so mutations won't alias the original.
#'
#' @param x an `epi_archive`
#' @return a modified `epi_archive` (same structure, independent data)
#' @keywords internal
#' @noRd
archive_deep_copy <- function(x) UseMethod("archive_deep_copy")

#' @export
archive_deep_copy.epi_archive_dt <- function(x) {
  x$DT <- data.table::copy(x$DT)
  x
}

#' Filter rows of the archive's data by a logical vector
#'
#' @param x an `epi_archive`
#' @param condition a logical vector of length `archive_nrow(x)`
#' @return a filtered data.table (preserves key)
#' @keywords internal
#' @noRd
archive_filter_rows <- function(x, condition) UseMethod("archive_filter_rows")

#' @export
archive_filter_rows.epi_archive_dt <- function(x, condition) {
  x$DT[condition, ]
}

#' Replace the archive's underlying data
#'
#' Returns a modified archive whose data storage is `data`. The setter is the
#' write-side counterpart to the read accessors; internal call sites should
#' prefer `x <- archive_set_data(x, ...)` over `x$DT <- ...` so a future
#' backend swap touches only this file.
#'
#' Accepts any tabular input (`data.table`, `tibble`, `data.frame`,
#' `dtplyr_step`, and eventually other backend handles) and normalizes it to
#' the backend's storage representation, ensuring the archive's key invariant
#' holds. Does not validate ukey uniqueness or other higher-level invariants;
#' use `validate_epi_archive` for that.
#'
#' @param x an `epi_archive`
#' @param data replacement data, any tabular form
#' @return a modified `epi_archive`
#' @keywords internal
#' @noRd
archive_set_data <- function(x, data) UseMethod("archive_set_data")

#' @export
archive_set_data.epi_archive_dt <- function(x, data) {
  dt <- as.data.table(data)
  ukey <- key_colnames(x)
  if (!identical(key(dt), ukey)) setkeyv(dt, ukey)
  x$DT <- dt
  x
}

#' Left-join with last-version-carried-forward on the trailing key
#'
#' For each row in `left`, find the row in `right` matching all `by` columns
#' exactly except the last, where the match uses LOCF semantics (the latest
#' row whose trailing-`by` value is `<=` the row's). Returns `left` with
#' `right`'s non-`by` columns attached; unmatched lookups produce `NA`.
#'
#' This is a domain primitive — dplyr can express it with
#' `join_by(..., closest(... >= ...))` (1.1.0+), but coverage across backends
#' is uneven, so each backend implements this directly.
#'
#' @param left,right tabular inputs
#' @param by character vector of join columns; the last is the LOCF axis
#' @return a dplyr-compatible handle representing the joined result
#' @keywords internal
archive_locf_join <- function(left, right, by) UseMethod("archive_locf_join")

#' @export
archive_locf_join.default <- function(left, right, by) {
  left_dt <- as.data.table(left)
  right_dt <- as.data.table(right)
  right_nonby <- setdiff(names(right_dt), by)
  result <- data.table::copy(left_dt)
  if (length(right_nonby) > 0L) {
    lookup <- right_dt[left_dt, right_nonby,
      with = FALSE, on = by,
      roll = TRUE, nomatch = NA, allow.cartesian = TRUE
    ]
    data.table::set(result, , right_nonby, lookup)
  }
  dtplyr::lazy_dt(result)
}

#' Get the archive's data as a shallow list of column references
#'
#' Returns a named list whose elements may share references with the archive's
#' storage. Used by tidyeval machinery that needs to detect pointer equality
#' between input and output columns (e.g., the key-invalidation check in
#' `epix_detailed_restricted_mutate`).
#'
#' @param x an `epi_archive`
#' @return a named list of column vectors (may alias storage)
#' @keywords internal
#' @noRd
archive_columns_as_list <- function(x) UseMethod("archive_columns_as_list")

#' @export
archive_columns_as_list.epi_archive_dt <- function(x) {
  as.list(x$DT)
}

#' Build a 0-row tibble with the archive's column names
#'
#' Used as a `data` argument for `tidyselect::eval_select` and related
#' call sites that only need to resolve column names against an archive's
#' schema, without materializing any rows.
#'
#' @param x an `epi_archive`
#' @return a 0-row tibble whose column names match `archive_colnames(x)`
#' @keywords internal
#' @noRd
archive_colmask <- function(x) {
  cols <- archive_colnames(x)
  tibble::new_tibble(
    rlang::set_names(rep(list(logical(0)), length(cols)), cols),
    nrow = 0L
  )
}

#' Check which named columns are factors
#'
#' Avoids materializing the full archive (cf. `archive_tbl`); only the
#' requested column slots are inspected.
#'
#' @param x an `epi_archive`
#' @param cols character vector of column names
#' @return a logical vector parallel to `cols`
#' @keywords internal
#' @noRd
archive_col_is_factor <- function(x, cols) UseMethod("archive_col_is_factor")

#' @export
archive_col_is_factor.epi_archive_dt <- function(x, cols) {
  vapply(cols, function(col) is.factor(x$DT[[col]]), logical(1))
}
