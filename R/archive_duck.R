# DuckDB/duckplyr backend for epi_archive.
#
# Subclass tag: c("epi_archive_duck", "epi_archive"). Storage lives in
# `$duck` as a duckplyr lazy frame; `archive_set_data` stores new query
# plans without materializing, so pipelines compose into one engine query.
#
# Connection model: in-memory only; each archive carries its own duckplyr
# handle (duckplyr manages the underlying duckdb connection).

#' Construct a duckdb-backed `epi_archive`
#'
#' Mirrors [`as_epi_archive`] but stores the underlying data in an in-memory
#' duckdb table via duckplyr. The validation, compactification, and signal
#' processing pipeline is reused; only the storage representation differs.
#'
#' @inheritParams as_epi_archive
#' @return an `epi_archive` with subclass `epi_archive_duck`
#'
#' @seealso [`as_epi_archive`] for the in-memory data.table backend.
#' @export
as_duckdb_epi_archive <- function(x, ...) {
  rlang::check_installed("duckplyr", reason = "for the duckdb-backed epi_archive backend.")
  archive <- as_epi_archive(x, ...)
  as_duckdb_archive(archive)
}

#' Construct a duckdb-backed `epi_archive` from a duckplyr table
#'
#' Wraps an existing duckplyr data frame, or an object that
#' [`duckplyr::as_duckdb_tibble()`] can convert, without first collecting the
#' full table through R. The input must already use canonical archive column
#' names (`geo_value`, `time_value`, and `version`) and wide signal columns;
#' unlike [`as_duckdb_epi_archive()`], this constructor does not perform signal
#' format conversion or compactification.
#'
#' If `geo_type`, `time_type`, or `versions_end` are omitted, they are inferred
#' from the corresponding columns. Those inference steps may read only the
#' needed columns or aggregates; pass them explicitly and set `validate = FALSE`
#' to avoid even those checks when wrapping a trusted DuckDB relation.
#'
#' @param x A duckplyr data frame, or a dplyr lazy table/data frame that
#'   [`duckplyr::as_duckdb_tibble()`] can convert.
#' @param ... Passed to [`duckplyr::as_duckdb_tibble()`] when `x` is not already
#'   a duckplyr data frame.
#' @param geo_type,time_type Optional scalar metadata. If `NULL`, guessed from
#'   `geo_value` and `time_value`, respectively.
#' @param other_keys Character vector of additional key columns.
#' @param clobberable_versions_start,versions_end Version-bound metadata.
#'   `versions_end = NULL` defaults to the maximum observed `version`.
#' @param validate If `TRUE`, validate archive invariants before returning.
#' @return an `epi_archive` with subclass `epi_archive_duck`
#' @export
as_duckdb_epi_archive_from_duck <- function(
  x,
  ...,
  geo_type = NULL,
  time_type = NULL,
  other_keys = character(),
  clobberable_versions_start = NA,
  versions_end = NULL,
  validate = TRUE
) {
  rlang::check_installed("duckplyr", reason = "for the duckdb-backed epi_archive backend.")
  assert_character(other_keys, any.missing = FALSE)
  assert_logical(validate, len = 1L, any.missing = FALSE)

  if (inherits(x, "duckplyr_df")) {
    rlang::check_dots_empty()
    duck <- x
  } else {
    duck <- duckplyr::as_duckdb_tibble(x, ...)
  }

  key_vars <- c("geo_value", other_keys, "time_value", "version")
  if (!all(key_vars %in% colnames(duck))) {
    cli_abort(c(
      "`x` is missing required archive columns: {format_varnames(setdiff(key_vars, colnames(duck)))}.",
      "i" = "Rename columns before calling `as_duckdb_epi_archive_from_duck()`."
    ))
  }
  if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
    cli_abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  }

  if (is.null(geo_type)) {
    geo_type <- guess_geo_type(dplyr::pull(duck, "geo_value"))
  } else {
    assert_string(geo_type)
  }
  if (is.null(time_type)) {
    time_type <- guess_time_type(dplyr::pull(duck, "time_value"))
  } else {
    assert_string(time_type)
  }
  if (is.null(versions_end)) {
    if (identical(nrow(duck), 0L)) {
      cli_abort(
        "`versions_end` must be specified when `x` has no rows.",
        class = "epiprocess__max_version_cannot_be_used"
      )
    }
    versions_end <- duck_col_range(duck, "version")[[2L]]
  }

  archive <- new_duckdb_archive(
    duck = duck,
    geo_type = geo_type,
    time_type = time_type,
    other_keys = other_keys,
    clobberable_versions_start = clobberable_versions_start,
    versions_end = versions_end
  )

  if (validate) {
    version_proxy <- list(version = dplyr::pull(duck, "version"))
    validate_version_bound(clobberable_versions_start, version_proxy, na_ok = TRUE)
    validate_version_bound(versions_end, version_proxy, na_ok = FALSE)
    validate_epi_archive(archive)
  }
  archive
}

#' Construct a duck-backed archive object from already-normalized pieces
#'
#' @keywords internal
#' @noRd
new_duckdb_archive <- function(
  duck,
  geo_type,
  time_type,
  other_keys,
  clobberable_versions_start,
  versions_end
) {
  structure(
    list(
      duck = duck,
      geo_type = geo_type,
      time_type = time_type,
      other_keys = other_keys,
      clobberable_versions_start = clobberable_versions_start,
      versions_end = versions_end
    ),
    class = c("epi_archive_duck", "epi_archive")
  )
}

#' Convert an existing `epi_archive` to the duckdb backend
#'
#' Materializes the source archive's data once, hands it to duckplyr, and
#' returns a new archive tagged `epi_archive_duck`. The source archive is
#' left untouched.
#'
#' @param x an `epi_archive`
#' @return an `epi_archive` with subclass `epi_archive_duck`
#' @keywords internal
#' @noRd
as_duckdb_archive <- function(x) {
  rlang::check_installed("duckplyr", reason = "for the duckdb-backed epi_archive backend.")
  assert_class(x, "epi_archive")
  if (inherits(x, "epi_archive_duck")) {
    return(x)
  }
  new_duckdb_archive(
    duck = duckplyr::as_duckdb_tibble(archive_tbl(x)),
    geo_type = x$geo_type,
    time_type = x$time_type,
    other_keys = x$other_keys,
    clobberable_versions_start = x$clobberable_versions_start,
    versions_end = x$versions_end
  )
}

# ---- backend-preserving construction --------------------------------------

#' Construct a new archive with the same backend as `template`
#'
#' Wraps `as_epi_archive` so backend tag and storage representation are
#' preserved across operations that have to rebuild the archive (e.g.
#' `filter.epi_archive`, `epix_merge`). Without this, those operations
#' would always produce a data.table-backed archive regardless of input.
#'
#' @param template an existing `epi_archive` whose backend should be reused
#' @param x,... passed to the backend's factory
#' @return an `epi_archive` with the same subclass tag as `template`
#' @keywords internal
#' @noRd
as_epi_archive_like <- function(template, x, ...) UseMethod("as_epi_archive_like")

#' @export
as_epi_archive_like.epi_archive_dt <- function(template, x, ...) {
  as_epi_archive(x, ...)
}

#' @export
as_epi_archive_like.epi_archive_duck <- function(template, x, ...) {
  as_duckdb_epi_archive(x, ...)
}

# ---- accessor methods ------------------------------------------------------

#' @export
archive_data.epi_archive_duck <- function(x) {
  x$duck
}

#' @export
archive_set_data.epi_archive_duck <- function(x, data) {
  if (!inherits(data, "duckplyr_df")) {
    data <- duckplyr::as_duckdb_tibble(tibble::as_tibble(data))
  }
  x$duck <- data
  x
}

#' @export
archive_tbl.epi_archive_duck <- function(x) {
  tibble::as_tibble(dplyr::collect(x$duck))
}

#' @export
archive_col.epi_archive_duck <- function(x, col) {
  dplyr::pull(x$duck, !!col)
}

#' @export
archive_col_range.epi_archive_duck <- function(x, col) {
  duck_col_range(x$duck, col)
}

#' Range helper for duckplyr data before it has archive metadata
#'
#' @keywords internal
#' @noRd
duck_col_range <- function(data, col) {
  summary <- data %>%
    dplyr::summarise(
      .min = min(.data[[col]], na.rm = TRUE),
      .max = max(.data[[col]], na.rm = TRUE),
      .missing = sum(is.na(.data[[col]]))
    ) %>%
    dplyr::collect()

  result <- c(summary$.min, summary$.max)
  if (summary$.missing > 0L) {
    result <- result[rep(NA_integer_, 2L)]
  }
  result
}

#' @export
archive_colnames.epi_archive_duck <- function(x) {
  colnames(x$duck)
}

#' @export
archive_nrow.epi_archive_duck <- function(x) {
  nrow(x$duck)
}

#' @export
archive_ncol.epi_archive_duck <- function(x) {
  ncol(x$duck)
}

#' @export
archive_any_duplicated_key.epi_archive_duck <- function(x) {
  key_cols <- key_colnames(x)
  dup_groups <- x$duck %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$n > 1L) %>%
    dplyr::collect()
  nrow(dup_groups) > 0L
}

#' @export
archive_deep_copy.epi_archive_duck <- function(x) {
  # Duckplyr query plans are value-semantic; no aliasing to break.
  x
}

#' @export
archive_filter_rows.epi_archive_duck <- function(x, condition) {
  # Eager mask path: materializes, applies the boolean vector, re-uploads.
  # Call sites that pre-compute a logical vector via `archive_col(...)` fit
  # poorly with the lazy backend; predicate-style call sites should prefer
  # `archive_filter(x, ...)`.
  tbl <- archive_tbl(x)
  duckplyr::as_duckdb_tibble(tbl[condition, , drop = FALSE])
}

#' @importFrom rlang :=
#' @export
archive_locf_join.duckplyr_df <- function(left, right, by) {
  if (!inherits(right, "duckplyr_df")) {
    right <- duckplyr::as_duckdb_tibble(tibble::as_tibble(right))
  }

  right_nonby <- setdiff(colnames(right), by)
  if (length(right_nonby) == 0L) {
    return(left)
  }

  locf_axis <- by[[length(by)]]
  exact_by <- by[-length(by)]
  join_exprs <- c(
    rlang::syms(exact_by),
    list(rlang::expr(closest(!!rlang::sym(locf_axis) >= !!rlang::sym(locf_axis))))   # nolint: object_usage_linter, closest is a dplyr::join_by DSL marker
  )

  # duckplyr lowers rolling dplyr joins to DuckDB ASOF joins. Keep this here
  # rather than in archive code so DT and DuckDB can use their native engines.
  joined <- dplyr::left_join(left, right, by = dplyr::join_by(!!!join_exprs))

  locf_axis_x <- paste0(locf_axis, ".x")
  if (locf_axis_x %in% colnames(joined)) {
    joined <- dplyr::rename(joined, !!locf_axis := !!rlang::sym(locf_axis_x))
  }

  dplyr::select(joined, dplyr::all_of(c(colnames(left), right_nonby)))
}

#' @export
archive_columns_as_list.epi_archive_duck <- function(x) {
  as.list(dplyr::collect(x$duck))
}

#' @export
archive_col_is_factor.epi_archive_duck <- function(x, cols) {
  # duckdb has no factor type; preserved-factor columns would round-trip
  # through duckplyr as character. Always false here.
  stats::setNames(rep(FALSE, length(cols)), cols)
}
