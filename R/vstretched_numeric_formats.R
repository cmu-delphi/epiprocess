# We can speed up some operations by temporarily converting data structures to
# have a time_type with more optimized operations. This file contains one such
# conversion for archives, "vstretched, numeric", which provides the speed of
# numeric time&version columns, but can't be used for all operations; see
# `to_vstretched_numeric_archive_info`.

#' Express time values as numbers of time steps from an origin time value
#'
#' Fast, reversible conversion of `time_value`s to bare integerish; reversed by
#' [`torigin_offsets_to_time_values()`]. Preserves time-time spacing in terms of
#' nsteps.
#'
#' Predefined time origin based on time_value ptype doesn't work (unknown
#' representation wday for weekly), so this requires us to provide one. If even
#' we don't know due to 0 rows in data structure, we can pass a length-0
#' `torigin` of the appropriate ptype, assuming that we'll continue to work with
#' 0-row data structures.
#'
#' @param time_values vector of time values of time type `time_type`.
#' @param torigin either (a) a single non-`NA` time value of time type
#'   `time_type`, or (b) a length-0 time value vector, in which case
#'   `time_values` must also be length 0.
#' @param time_type string.
#' @return bare integerish vector, with length matching that of `time_values`.
#'
#' @keywords internal
time_values_to_torigin_offsets <- function(time_values, torigin, time_type) {
  if (vec_size(torigin) == 0L) {
    if (vec_size(time_values) != 0L) {
      cli_abort("Can't convert time values using a 0-length `torigin`.")
    }
    vec_data(time_values)
  } else {
    time_delta_to_n_steps(time_values - torigin, time_type)
  }
}

#' Inverse of [`time_values_to_torigin_offsets()`]
#'
#' @param torigin_offsets bare integerish numeric vector, as output by
#'   [`time_values_to_torigin_offsets()`], or derived from such output.
#' @param torigin,time_type the `torigin` and `time_type` passed to
#'   [`time_values_to_torigin_offsets()`] used when generating `torigin_offsets`.
#' @return vec; same ptype as `torigin`, same size as `torigin_offsets`.
#'
#' @keywords internal
torigin_offsets_to_time_values <- function(torigin_offsets, torigin, time_type) {
  if (vec_size(torigin) == 0L) {
    if (vec_size(torigin_offsets) != 0L) {
      cli_abort("Can't convert nonempty `torigin_offsets` using a 0-length `torigin`.")
    }
    torigin
  } else {
    # (`vec_cast` to fix up storage mode for bare integer time_values.)
    vec_cast(torigin + n_steps_to_time_delta(torigin_offsets, time_type, "fast"), torigin)
  }
}

#' Express versions as numeric offsets from an origin version
#'
#' Fast conversion of versions to bare numbers; reversed by
#' [`vorigin_offsets_to_versions()`].
#'
#' This just works with the underlying numeric data of `versions`, which should
#' preserve the value of inequalities between versions, but other than that,
#' basically all bets are off: no necessary relationship with torigin_offsets or
#' differences between them, nor anything about original version-version deltas
#' (for latter, `tsibble::yearmonth` backing and POSIXct are challenges).
#'
#' @param versions vector of versions, `clobberable_versions_start`, or
#'   `versions_end`; may contain `NA`s.
#' @param vorigin length-1, non-`NA` vector with the same ptype as `versions`.
#' @return bare numeric vector, same length as `versions`.
#'
#' @keywords internal
versions_to_vorigin_offsets <- function(versions, vorigin) {
  vec_data(versions) - vec_data(vorigin)
}

#' Inverse of [`versions_to_vorigin_offsets()`]
#'
#' @param vorigin_offsets bare numeric vector, as output by
#'   [`versions_to_vorigin_offsets()`], or derived from such output.
#' @param vorigin the `vorigin` passed to [`versions_to_vorigin_offsets()`] used
#'   when generating `vorigin_offsets`.
#' @return vec; same ptype as `vorigin`, same size as `vorigin_offsets`.
#'
#' @keywords internal
vorigin_offsets_to_versions <- function(vorigin_offsets, vorigin) {
  res <- vorigin_offsets + vec_data(vorigin)
  if (is.object(vorigin)) {
    vctrs::vec_restore(res, vorigin)
  } else {
    # Bare integer/double; make sure we match the original storage type (and
    # complain about any lossy conversion back to integer):
    vec_cast(res, vorigin)
  }
}

#' Like [`versions_to_vorigin_offsets()`], but forwarding any `NA`
#'
#' For `epi_archive` version bounds like `clobberable_versions_start`, which use
#' a logical `NA` rather than an `NA` of the `version` class.
#'
#' @keywords internal
version_bound_to_vorigin_offset_bound <- function(version_bound, vorigin) {
  if (is.na(version_bound)) version_bound
  else versions_to_vorigin_offsets(version_bound, vorigin)
}

#' Inverse of [`version_bound_to_vorigin_offset_bound()`]
#'
#' @keywords internal
vorigin_offset_bound_to_version_bound <- function(vorigin_offset_bound, vorigin) {
  if (is.na(vorigin_offset_bound)) vorigin_offset_bound
  else vorigin_offsets_to_versions(vorigin_offset_bound, vorigin)
}

#' Copy a `DT` of an `epi_archive`, replacing its `time_value` and `version` columns
#'
#' The result's other columns are deep copies of `dtbl`'s, so they don't alias
#' `dtbl`. To avoid extra copies, `time_values` and `versions` are used directly
#' as the new columns, not copied, so callers should pass freshly computed
#' vectors that they won't use elsewhere: the result may modify them in place
#' (e.g., the `setkeyv()` below may reorder them).
#'
#' @param dtbl the `DT` of an `epi_archive`.
#' @param time_values,versions vectors with as many entries as `dtbl` has rows,
#'   to become the result's `time_value` and `version` columns.
#' @return a `data.table` with the same column order and key as `dtbl`.
#'
#' @importFrom data.table copy key setcolorder setDT setkeyv
#' @keywords internal
dtbl_with_time_values_versions <- function(dtbl, time_values, versions) {
  other_names <- vec_set_difference(names(dtbl), c("time_value", "version"))
  lst <- lapply(as.list(dtbl)[other_names], copy)
  lst$time_value <- time_values
  lst$version <- versions
  res <- setDT(lst)
  setcolorder(res, names(dtbl))
  setkeyv(res, key(dtbl))
  res
}

#' Convert an archive to/from a "vstretched, numeric" form
#'
#' Arithmetic is slow on some time-related classes. It can be faster to perform
#' operations under integer time_type. These functions provide conversions
#' to/from integer-time-type `epi_archive`s which preserve time-time spacing in
#' terms of nsteps and the ordering of versions, but which do not try to
#' preserve anything about time-version differences or version-version
#' differences; thus "vstretched".
#'
#' `to_vstretched_numeric_archive_info` converts an archive `x` to (i) an
#' archive with integer `time_type` and bare numeric `time_value`s and
#' `version`s, plus (ii) conversion metadata, with (i) and (ii) packed in a
#' named list. `from_vstretched_numeric_archive_info` converts from this format back
#' to the format of `x`.
#'
#' For some verbs, we may need to transform additional arguments using
#' [`time_values_to_torigin_offsets`], [`versions_to_vorigin_offsets`], or
#' [`version_bound_to_vorigin_offset_bound`], forwarding the appropriate extra
#' arguments from the info object's conversion metadata.
#'
#' Verbs that change the number of archive rows from zero to nonzero are
#' incompatible with the "from" transformation.
#'
#' @param x an `epi_archive`
#' @return * Of `to_vstretched_numeric_archive_info`: a list with elements:
#'   * `archive`: the converted `epi_archive`, with `time_type` `"integer"`.
#'   * `conversion`: a list with elements `time_type` (`x$time_type`), `torigin`,
#'     and `vorigin`, to pass to `from_vstretched_numeric_archive_info` or use when
#'     converting other time values and versions.
#'
#' @keywords internal
to_vstretched_numeric_archive_info <- function(x) {
  assert_class(x, "epi_archive")
  time_type <- x$time_type
  dtbl <- x$DT
  # Any time_value works as a `torigin`; take the first since it's fast to
  # access (it's not necessarily the overall min). If there are no rows, use a
  # length-0 `torigin` to record the ptype.
  torigin <- vec_slice(dtbl$time_value, seq_len(min(1L, nrow(dtbl))))
  # `versions_end` is required to be non-NA and have the same class as
  # `dtbl$version`, so is always available as a `vorigin`:
  vorigin <- x$versions_end
  archive <- new_epi_archive(
    dtbl_with_time_values_versions(
      dtbl,
      time_values_to_torigin_offsets(dtbl$time_value, torigin, time_type),
      versions_to_vorigin_offsets(dtbl$version, vorigin)
    ),
    x$geo_type, "integer", x$other_keys,
    version_bound_to_vorigin_offset_bound(x$clobberable_versions_start, vorigin),
    version_bound_to_vorigin_offset_bound(x$versions_end, vorigin)
  )
  list(
    archive = archive,
    conversion = list(time_type = time_type, torigin = torigin, vorigin = vorigin)
  )
}

#' @param transformed_x an `epi_archive` with `time_type` `"integer"` and bare
#'   numeric `time_value`s and `version`s, in the converted form described by
#'   `conversion`; e.g., `to_vstretched_numeric_archive_info(x)$archive` or some
#'   processed version of it. `time_value`s must be integerish.
#' @param conversion the `conversion` element of the
#'   `to_vstretched_numeric_archive_info` output.
#' @return * Of `from_vstretched_numeric_archive_info`: `transformed_x` with
#'   `time_type`, `time_value`, and `version` representations converted back to
#'   those of the original archive.
#'
#' @rdname to_vstretched_numeric_archive_info
#' @keywords internal
from_vstretched_numeric_archive_info <- function(transformed_x, conversion) {
  if (!inherits(transformed_x, "epi_archive")) {
    cli_abort("`transformed_x` must be an `epi_archive`, but it was a {.cls {class(transformed_x)}}")
  }
  dtbl <- transformed_x$DT
  if (!is.numeric(dtbl$time_value) || is.object(dtbl$time_value) ||
        !is.numeric(dtbl$version) || is.object(dtbl$version)) {
    cli_abort("`transformed_x` must have bare numeric `time_value`s and `version`s")
  }
  vorigin <- conversion$vorigin
  new_epi_archive(
    dtbl_with_time_values_versions(
      dtbl,
      torigin_offsets_to_time_values(dtbl$time_value, conversion$torigin, conversion$time_type),
      vorigin_offsets_to_versions(dtbl$version, vorigin)
    ),
    transformed_x$geo_type, conversion$time_type, transformed_x$other_keys,
    vorigin_offset_bound_to_version_bound(transformed_x$clobberable_versions_start, vorigin),
    vorigin_offset_bound_to_version_bound(transformed_x$versions_end, vorigin)
  )
}

#' `epi_df` with its `time_value` column and `time_type` replaced
#'
#' Keeps all other attributes, including grouping, which is assumed not to
#' involve `time_value`.
#'
#' @param x an `epi_df`; if grouped, must not include `time_value` in grouping
#'   (unchecked)
#' @param time_values vector with as many entries as `x` has rows, to become the
#'   result's `time_value` column.
#' @param time_type string; the result's `time_type`.
#' @return an `epi_df`.
#'
#' @keywords internal
edf_with_time_values <- function(x, time_values, time_type) {
  attrs <- attributes(x)
  res <- unclass(x)
  res$time_value <- time_values
  attributes(res) <- attrs
  attr(res, "metadata")$time_type <- time_type
  res
}

#' Convert an `epi_df` to/from a "vstretched, numeric" form
#'
#' Analogue of [`to_vstretched_numeric_archive_info`] for `epi_df`s:
#' `to_vstretched_numeric_edf_info` converts an `epi_df` `x` to (i) an `epi_df`
#' with integer `time_type` and bare numeric `time_value`s, plus (ii) conversion
#' metadata, with (i) and (ii) packed in a named list.
#' `from_vstretched_numeric_edf_info` converts from this format back to the
#' format of `x`.
#'
#' `time_value`s are converted with [`time_values_to_torigin_offsets`]; other
#' time values (e.g., function arguments) can be converted consistently by
#' forwarding `conversion$torigin` and `conversion$time_type` to it, and back
#' with [`torigin_offsets_to_time_values`]. `epi_df`s have no `version` column,
#' and `as_of` is left unchanged (it's not converted to the stretched scale), so
#' verbs shouldn't relate it to the converted `time_value`s.
#'
#' Grouping is preserved, but `x` must not be grouped by `time_value`.
#'
#' Verbs that change the number of rows from zero to nonzero are incompatible
#' with the "from" transformation.
#'
#' @param x an `epi_df`
#' @return * Of `to_vstretched_numeric_edf_info`: a list with elements:
#'   * `edf`: the converted `epi_df`, with `time_type` `"integer"`.
#'   * `conversion`: a list with elements `time_type` (`x`'s `time_type`) and
#'     `torigin`, to pass to `from_vstretched_numeric_edf_info` or use when
#'     converting other time values.
#'
#' @keywords internal
to_vstretched_numeric_edf_info <- function(x) {
  assert_class(x, "epi_df")
  if ("time_value" %in% group_vars(x)) {
    cli_abort("`x` must not be grouped by `time_value`")
  }
  time_type <- attr(x, "metadata")$time_type
  # As in `to_vstretched_numeric_archive_info`:
  torigin <- vec_slice(x$time_value, seq_len(min(1L, nrow(x))))
  edf <- edf_with_time_values(
    x,
    time_values_to_torigin_offsets(x$time_value, torigin, time_type),
    "integer"
  )
  list(
    edf = edf,
    conversion = list(time_type = time_type, torigin = torigin)
  )
}

#' @param transformed_x an `epi_df` with `time_type` `"integer"` and bare
#'   numeric `time_value`s, in the converted form described by `conversion`;
#'   e.g., `to_vstretched_numeric_edf_info(x)$edf` or some processed version of
#'   it.
#' @param conversion the `conversion` element of the
#'   `to_vstretched_numeric_edf_info` output.
#' @return * Of `from_vstretched_numeric_edf_info`: `transformed_x` with
#'   `time_type` and `time_value` representations converted back to those of the
#'   original `epi_df`.
#'
#' @rdname to_vstretched_numeric_edf_info
#' @keywords internal
from_vstretched_numeric_edf_info <- function(transformed_x, conversion) {
  if (!inherits(transformed_x, "epi_df")) {
    cli_abort("`transformed_x` must be an `epi_df`, but it was a {.cls {class(transformed_x)}}")
  }
  if (!is.numeric(transformed_x$time_value) || is.object(transformed_x$time_value)) {
    cli_abort("`transformed_x` must have bare numeric `time_value`s")
  }
  edf_with_time_values(
    transformed_x,
    torigin_offsets_to_time_values(transformed_x$time_value, conversion$torigin, conversion$time_type),
    conversion$time_type
  )
}
