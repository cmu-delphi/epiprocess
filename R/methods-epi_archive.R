#' Convert an archive to a tibble
#'
#' Materializes the full version history of an `epi_archive` as a tibble. This is
#' useful when you need to inspect archive rows directly, independent of the
#' archive's storage backend. For a single as-of snapshot, prefer [epix_as_of()].
#'
#' @param x an `epi_archive`
#' @param ... forwarded to [`tibble::as_tibble`]
#' @return A tibble containing the archive's full version history.
#' @export
as_tibble.epi_archive <- function(x, ...) {
  tibble::as_tibble(archive_tbl(x), ...)
}

#' Generate a snapshot from an `epi_archive` object
#'
#' Generates a snapshot in `epi_df` format from an `epi_archive` object, as of a
#' given version. See the [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x An `epi_archive` object
#' @param version Time value specifying the max version to permit in the
#'   snapshot. That is, the snapshot will comprise the unique rows of the
#'   current archive data that represent the most up-to-date signal values, as
#'   of the specified `version` (and whose time values are at least
#'   `min_time_value`.)
#' @param min_time_value Time value specifying the min time value to permit in
#'   the snapshot. Default is `-Inf`, which effectively means that there is no
#'   minimum considered.
#' @param all_versions If `all_versions = TRUE`, then the output will be in
#'   `epi_archive` format, and contain rows in the specified `time_value` range
#'   having `version <= version`. The resulting object will cover a
#'   potentially narrower `version` and `time_value` range than `x`, depending
#'   on user-provided arguments. Otherwise, there will be one row in the output
#'   for the `version` of each `time_value`. Default is `FALSE`.
#' @param max_version `r lifecycle::badge("deprecated")` please use `version`
#'   argument instead.
#' @return An `epi_df` object.
#'
#' @examples
#' archive_cases_dv_subset_tbl <- tibble::as_tibble(archive_cases_dv_subset)
#' epix_as_of(
#'   archive_cases_dv_subset,
#'   version = max(archive_cases_dv_subset_tbl$version)
#' )
#'
#' range(archive_cases_dv_subset_tbl$version) # 2020-06-02 -- 2021-12-01
#'
#' epix_as_of(archive_cases_dv_subset, as.Date("2020-06-12"))
#'
#' # --- Advanced: ---
#'
#' # When requesting recent versions of a data set, there can be some
#' # reproducibility issues. For example, requesting data as of the current date
#' # may return different values based on whether today's data is available yet
#' # or not. Other factors include the time it takes between data becoming
#' # available and when you download the data, and whether the data provider
#' # will overwrite ("clobber") version data rather than just publishing new
#' # versions. You can include information about these factors by setting the
#' # `clobberable_versions_start` and `versions_end` of an `epi_archive`, in
#' # which case you will get warnings about potential reproducibility issues:
#'
#' archive_cases_dv_subset2 <- as_epi_archive(
#'   archive_cases_dv_subset_tbl,
#'   # Suppose last version with an update could potentially be rewritten
#'   # (a.k.a. "hotfixed", "clobbered", etc.):
#'   clobberable_versions_start = max(archive_cases_dv_subset_tbl$version),
#'   # Suppose today is the following day, and there are no updates out yet:
#'   versions_end = max(archive_cases_dv_subset_tbl$version) + 1L
#' )
#'
#' epix_as_of(archive_cases_dv_subset2, max(archive_cases_dv_subset_tbl$version))
#'
#' @importFrom data.table between key
#' @importFrom checkmate assert_scalar assert_logical assert_class
#' @export
epix_as_of <- function(x, version, min_time_value = -Inf, all_versions = FALSE,
                       max_version = deprecated()) {
  assert_class(x, "epi_archive")

  if (lifecycle::is_present(max_version)) {
    lifecycle::deprecate_warn("0.8.1", "epix_as_of(max_version =)", "epix_as_of(version =)")
    version <- max_version
  }

  other_keys <- x$other_keys

  # Check a few things on version
  if (!identical(class(version), class(archive_col(x, "version")))) {
    cli_abort(
      "`version` must have the same `class` vector as the archive's `version` column."
    )
  }
  assert_scalar(version, na.ok = FALSE)
  if (version > x$versions_end) {
    cli_abort("`version` must be at most `epi_archive$versions_end`.")
  }
  assert_scalar(min_time_value, na.ok = FALSE)
  min_time_value_inf <- is.infinite(min_time_value) && min_time_value < 0
  min_time_value_same_type <- identical(class(min_time_value), class(archive_col(x, "time_value")))
  if (!min_time_value_inf && !min_time_value_same_type) {
    cli_abort("`min_time_value` must be either -Inf or a time_value of the same type and
      class as `epi_archive$time_value`.")
  }
  assert_logical(all_versions, len = 1)
  if (!is.na(x$clobberable_versions_start) && version >= x$clobberable_versions_start) {
    cli_warn(
      'Getting data as of some recent version which could still be
      overwritten (under routine circumstances) without assigning a new
      version number (a.k.a. "clobbered").  Thus, the snapshot that we
      produce here should not be expected to be reproducible later. See
      `?epi_archive` for more info and `?epix_as_of` on how to muffle.',
      class = "epiprocess__snapshot_as_of_clobberable_version"
    )
  }

  # We can't disable nonstandard evaluation nor use the `..` feature in the `i`
  # argument of `[.data.table` below; try to avoid problematic names and abort
  # if we fail to do so:
  .min_time_value <- min_time_value
  .version <- version
  if (any(c(".min_time_value", ".version") %in% archive_colnames(x))) {
    cli_abort("epi_archives can't contain a `.min_time_value` or `.version` column")
  }


  # Filter by version and return
  if (all_versions) {
    # epi_archive is copied into result, so we can modify result directly
    result <- epix_truncate_versions_after(x, version)
    if (!min_time_value_inf) {
      # See below for why we need this branch.
      result <- archive_set_data(result, archive_filter_rows(result, archive_col(result, "time_value") >= .min_time_value))
    }
    return(result)
  }

  # The `min_time_value_inf` branch is needed for `epix_as_of` to work with
  # `yearmonth` time type, where comparing against -Inf yields NA.
  filtered <- archive_data(x) %>% dplyr::filter(version <= .version)
  if (!min_time_value_inf) {
    filtered <- filtered %>% dplyr::filter(time_value >= .min_time_value)
  }
  nonversion_keys <- c("geo_value", other_keys, "time_value")
  # Pick the latest-version row per epikey-time. `arrange(desc(version)) %>%
  # distinct(..., .keep_all=TRUE)` translates portably across dtplyr and
  # duckplyr; `slice_max(order_by=...)` does not (dtplyr's translation calls
  # an unqualified `desc()` that fails to resolve at eval time).
  as_of_epi_df <- filtered %>%
    dplyr::arrange(dplyr::desc(version)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(nonversion_keys)), .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(nonversion_keys))) %>%
    dplyr::collect() %>%
    tibble::as_tibble() %>%
    dplyr::select(-"version") %>%
    as_epi_df(as_of = version, other_keys = other_keys)

  return(as_of_epi_df)
}

#' Get the latest snapshot from an `epi_archive` object.
#'
#' The latest snapshot is the snapshot of the last known version.
#'
#' @param x An `epi_archive` object
#' @return The latest snapshot from an `epi_archive` object
#' @export
epix_as_of_current <- function(x) {
  assert_class(x, "epi_archive")
  x %>% epix_as_of(.$versions_end)
}

#' Set the `versions_end` attribute of an `epi_archive` object
#'
#' An escape hatch for epix_as_of, which does not allow version >
#' `$versions_end`.
#'
#' @param x An `epi_archive` object
#' @param versions_end The new `versions_end` value
#' @return An `epi_archive` object with the updated `versions_end` attribute
#' @export
set_versions_end <- function(x, versions_end) {
  assert_class(x, "epi_archive")
  validate_version_bound(versions_end, list(version = archive_col(x, "version")), na_ok = FALSE)
  x$versions_end <- versions_end
  x
}

#' Fill `epi_archive` unobserved history
#'
#' @description
#' This function fills in missing version history in an `epi_archive` object up
#' to a specified version, updating the `versions_end` field as necessary. Note
#' that the filling is done in a compactified way, see details.
#'
#' @param x An `epi_archive`
#' @param fill_versions_end a scalar of the same class&type as `x$version`: the
#'   version through which to fill in missing version history; the
#'   `epi_archive`'s `versions_end` attribute will be set to this, unless it
#'   already had a later `$versions_end`.
#' @param how Optional; `"na"` or `"locf"`: `"na"` fills missing version history
#'   with `NA`s, `"locf"` fills missing version history with the last version of
#'   each observation carried forward (LOCF). Default is `"na"`.
#' @return An `epi_archive`
#' @details
#' Note that we generally store `epi_archive`'s in a compacted form, meaning
#' that, implciitly, if a version does not exist, but the `version_end`
#' attribute is greater, then it is understood that all the versions in between
#' had the same value as the last observed version. This affects the behavior of
#' this function in the following ways:
#'
#' - if `how = "na"`, then the function will fill in at most one missing version
#'   with `NA` and the rest will be implicit.
#' - if `how = "locf"`, then the function will not fill any values.
#'
#' @importFrom dplyr union_all distinct mutate across
#' @importFrom rlang arg_match
#' @importFrom tidyselect all_of
#' @return An `epi_archive`
#' @export
#' @examples
#' test_date <- as.Date("2020-01-01")
#' ea_orig <- as_epi_archive(data.table::data.table(
#'   geo_value = "ak",
#'   time_value = test_date + c(rep(0L, 5L), 1L),
#'   version = test_date + c(1:5, 2L),
#'   value = 1:6
#' ))
#' epix_fill_through_version(ea_orig, test_date + 8, "na")
#' epix_fill_through_version(ea_orig, test_date + 8, "locf")
epix_fill_through_version <- function(x, fill_versions_end, how = c("na", "locf")) {
  assert_class(x, "epi_archive")
  validate_version_bound(fill_versions_end, list(version = archive_col(x, "version")), na_ok = FALSE)
  how <- arg_match(how)
  if (x$versions_end >= fill_versions_end) {
    return(x)
  }

  next_version_tag <- next_after(x$versions_end)
  if (next_version_tag > fill_versions_end) {
    cli_abort(paste(
      "Apparent problem with {.code next_after} method:",
      "archive contained observations through version {x$versions_end}",
      "and the next possible version was supposed to be {next_version_tag},",
      "but this appeared to jump from a version < {fill_versions_end}",
      "to one > {fill_versions_end}, implying at least one version in between."
    ))
  }

  new_data <- switch(how,
    "na" = {
      # Append a synthetic version at `next_version_tag` whose value columns are
      # NA, one row per epikey-time ever observed. LOCF is built into downstream
      # methods so the "locf" branch needs no extra rows.
      nonversion_keys <- key_colnames(x, exclude = "version")
      value_cols <- setdiff(archive_colnames(x), key_colnames(x))
      next_version_rows <- archive_data(x) %>%
        distinct(across(all_of(nonversion_keys)), .keep_all = TRUE) %>%
        mutate(
          version = next_version_tag,
          # `.x[NA]` keeps each value column's original type (vs. `NA`, which
          # is logical and breaks the typed set-op in `archive_set_data`).
          across(all_of(value_cols), ~ .x[NA])
        )
      union_all(archive_data(x), next_version_rows)
    },
    "locf" = archive_data(x)
  )

  x <- archive_set_data(x, new_data)
  x$versions_end <- fill_versions_end
  x
}


#' Merge two `epi_archive` objects
#'
#' Merges two `epi_archive`s that share a common `geo_value`, `time_value`, and
#' set of key columns. When they also share a common `versions_end`, using
#' `epix_as_of` on the result should be the same as using `epix_as_of` on `x`
#' and `y` individually, then performing a full join of the `DT`s on the
#' non-version key columns (potentially consolidating multiple warnings about
#' clobberable versions). If the `versions_end` values differ, the `sync`
#' parameter controls what is done.
#'
#' @param x,y Two `epi_archive` objects to join together.
#' @param sync Optional; character. The argument that decides how to handle the
#'   situation when one signal has a more recent revision than another signal
#'   for a key that they have both already observed. The options are:
#'
#'   - `"forbid"`: the default and the strictest option, throws an error; this
#'   is likely not what you want, but it is strict to make the user aware of the
#'   issues,
#'   - `"locf"`: carry forward the last observed version of the missing signal
#'   to the new version and use `max(x$versions_end, y$versions_end)` as the
#'   result's `versions_end`,
#'   - `"na"`: fill the unobserved values with `NA`'s (this can be handy when
#'   you know that source data is truly missing upstream and you want to
#'   represent the lack of information accurately, for instance) and use
#'   `max(x$versions_end, y$versions_end)` as the result's `versions_end`,
#'   - `"truncate"`: discard any rows containing update rows for later versions
#'   and use `min(x$versions_end, y$versions_end)` as the result's
#'   `versions_end`.
#'
#' @param compactify Optional; `TRUE` (default), `FALSE`, or `"message"`; should the
#'   result be compactified? See `as_epi_archive()` for details.
#' @param compactify_abs_tol As in [`as_epi_archive()`].
#' @details
#' When merging archives, unless the archives have identical data release
#' patterns, we often have to handle the situation when one signal has a more
#' recent observation for a key than another signal. In this case, we have two
#' options:
#'
#' - if the the other signal has never observed that key, we need to introduce
#' `NA`s in the non-key variables for the missing signal,
#' - if the other signal has observed that key previously, but at an ealier
#' revision date, then we need to decide how to handle the missing value in the
#' more recent signal; the `sync` argument controls this behavior.
#'
#' @return the resulting `epi_archive`
#'
#' @details In all cases, `clobberable_versions_start` will be set to the
#'   earliest version that could be clobbered in either input archive.
#'
#' @examples
#' # Example 1
#' # The s1 signal at August 1st gets revised from 10 to 11 on August 2nd
#' s1 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-02")),
#'   signal1 = c(10, 11, 7)
#' )
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-03", "2024-08-03")),
#'   signal2 = c(2, 3)
#' )
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#' merged <- epix_merge(s1, s2, sync = "locf")
#' tibble::as_tibble(merged)
#'
#' # Example 2
#' # The s1 signal at August 1st gets revised from 12 to 13 on August 3rd
#' s1 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-03")),
#'   version = as.Date(c("2024-08-01", "2024-08-03", "2024-08-03", "2024-08-03")),
#'   signal1 = c(12, 13, 22, 19)
#' )
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-02", "2024-08-02")),
#'   signal2 = c(4, 5),
#' )
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#' merged <- epix_merge(s1, s2, sync = "locf")
#' tibble::as_tibble(merged)
#'
#'
#' # Example 3:
#' s1 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
#'   version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
#'   signal1 = c(14, 11, 9)
#' )
#' # The s2 signal at August 1st gets revised from 3 to 5 on August 3rd
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-02", "2024-08-03", "2024-08-03")),
#'   signal2 = c(3, 5, 2),
#' )
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#' merged <- epix_merge(s1, s2, sync = "locf")
#' tibble::as_tibble(merged)
#' @importFrom dplyr full_join select
#' @export
epix_merge <- function(x, y,
                       sync = c("forbid", "na", "locf", "truncate"),
                       compactify = TRUE, compactify_abs_tol = 0) {
  assert_class(x, "epi_archive")
  assert_class(y, "epi_archive")
  sync <- rlang::arg_match(sync)

  if (!identical(x$geo_type, y$geo_type)) {
    cli_abort("`x` and `y` must have the same `$geo_type`")
  }
  if (!identical(x$time_type, y$time_type)) {
    cli_abort("`x` and `y` must share data type on their `time_value` column.")
  }
  if (!identical(sort(key_colnames(x)), sort(key_colnames(y)))) {
    cli_abort("
            The archives must have the same set of key column names; if the
            key columns represent the same things, just with different
            names, please retry after manually renaming to match; if they
            represent different things (e.g., x has an age breakdown
            but y does not), please retry after processing them to share
            the same key (e.g., by summarizing x to remove the age breakdown,
            or by applying a static age breakdown to y).
          ", class = "epiprocess__epix_merge_x_y_must_have_same_key_set")
  }

  result_clobberable_versions_start <-
    if (all(is.na(c(x$clobberable_versions_start, y$clobberable_versions_start)))) {
      NA
    } else {
      min(c(x$clobberable_versions_start, y$clobberable_versions_start), na.rm = TRUE)
    }

  switch(sync,
    "forbid" = {
      if (!identical(x$versions_end, y$versions_end)) {
        cli_abort(paste(
          "`x` and `y` were not equally up to date version-wise:",
          "`x$versions_end` was not identical to `y$versions_end`;",
          "either ensure that `x` and `y` are equally up to date before merging,",
          "or specify how to deal with this using `sync`"
        ), class = "epiprocess__epix_merge_unresolved_sync")
      }
      new_versions_end <- x$versions_end
      x_synced <- x
      y_synced <- y
    },
    "na" = ,
    "locf" = {
      new_versions_end <- max(c(x$versions_end, y$versions_end))
      x_synced <- epix_fill_through_version(x, new_versions_end, sync)
      y_synced <- epix_fill_through_version(y, new_versions_end, sync)
    },
    "truncate" = {
      new_versions_end <- min(c(x$versions_end, y$versions_end))
      x_synced <- archive_set_data(x, archive_filter_rows(x, archive_col(x, "version") <= new_versions_end))
      y_synced <- archive_set_data(y, archive_filter_rows(y, archive_col(y, "version") <= new_versions_end))
    }
  )

  by <- key_colnames(x_synced) # = some perm of key_colnames(y_synced); version last
  x_nonby <- setdiff(archive_colnames(x_synced), by)
  y_nonby <- setdiff(archive_colnames(y_synced), by)
  if (length(intersect(x_nonby, y_nonby)) != 0L) {
    cli_abort("
            `x` and `y` DTs both have measurement columns named
            {format_chr_with_quotes(intersect(x_nonby, y_nonby))};
            this is currently not supported; please manually fix up first:
            any overlapping columns that can are key-like should be
            incorporated into the key, and other columns should be renamed.
          ", class = "epiprocess__epix_merge_x_y_must_not_have_overlapping_nonby_colnames")
  }
  # by-cols-uniquely-determine-rows is guaranteed by the archive ukey invariant
  # (since `by == key_colnames(x_synced) == ukey`); validated at construction.

  # Row universe: full join of just the key columns from each side.
  result_keys <- full_join(
    archive_data(x_synced) %>% select(all_of(by)),
    archive_data(y_synced) %>% select(all_of(by)),
    by = by
  )

  # Attach each side's non-by (value) columns via LOCF on the trailing key.
  result <- result_keys %>%
    archive_locf_join(archive_data(x_synced) %>% select(all_of(c(by, x_nonby))), by) %>%
    archive_locf_join(archive_data(y_synced) %>% select(all_of(c(by, y_nonby))), by)

  as_epi_archive_like(
    x, as.data.table(result),
    other_keys = setdiff(by, c("geo_value", "time_value", "version")),
    # It'd probably be better to pre-compactify before the merge, and might be
    # guaranteed not to be necessary to compactify the merge result if the
    # inputs are already compactified, but at time of writing we don't have
    # compactify in its own method or field, and it seems like it should be
    # pretty fast anyway.
    compactify = compactify, compactify_abs_tol = compactify_abs_tol,
    clobberable_versions_start = result_clobberable_versions_start,
    versions_end = new_versions_end
  )
}


#' A more detailed but restricted `mutate` for use in `group_by.epi_archive`
#'
#' More detailed: provides the names of the "requested" columns in addition to
#' the output expected from a regular `mutate` method.
#'
#' Restricted: doesn't allow replacing or removing key cols, where a sort is
#' potentially required at best and what the output key should be is unclear at
#' worst. (The originally expected restriction was that the `mutate` parameters
#' not present in `group_by` would not be recognized, but the current
#' implementation just lets `mutate` handle these even anyway, even if they're
#' not part of the regular `group_by` parameters; these arguments would have to
#' be passed by names with dot prefixes, so just hope that the user means to use
#' them here if provided.)
#'
#' This can introduce column-level aliasing in `data.table`s, which isn't really
#' intended in the `data.table` user model but we can make it part of our user
#' model (see
#' https://stackoverflow.com/questions/45925482/make-a-shallow-copy-in-data-table
#' and links).
#'
#' Don't export this without cleaning up language of "mutate" as in side effects
#' vs. "mutate" as in `dplyr::mutate`.
#' @noRd
epix_detailed_restricted_mutate <- function(.data, ...) {
  # `col_modify_recorder_df` is a tidyeval mechanism that captures the named
  # column outputs of a `mutate(...)` call without executing dplyr's normal
  # column-modification. We need a tibble whose columns share references with
  # the archive's storage so the key-invalidation check below can detect
  # replacement (vs. aliasing) by pointer equality.
  in_tbl <- tibble::as_tibble(archive_columns_as_list(.data), .name_repair = "minimal")
  col_modify_cols <- destructure_col_modify_recorder_df(
    mutate(new_col_modify_recorder_df(in_tbl), ...)
  )[["cols"]]

  ukey <- key_colnames(.data)
  invalidated_key_col_is <- which(purrr::map_lgl(ukey, function(key_colname) {
    key_colname %in% names(col_modify_cols) &&
      !rlang::is_reference(in_tbl[[key_colname]], col_modify_cols[[key_colname]])
  }))
  if (length(invalidated_key_col_is) != 0L) {
    rlang::abort(paste_lines(c(
      "Key columns must not be replaced or removed.",
      wrap_varnames(ukey[invalidated_key_col_is], initial = "Flagged key cols: ")
    )))
  }

  out_tbl <- dplyr::dplyr_col_modify(in_tbl, col_modify_cols)
  list(
    archive = archive_set_data(.data, out_tbl),
    request_names = names(col_modify_cols)
  )
}


#' Take each requested (group and) version in an archive, run a computation (e.g., forecast)
#'
#' ... and collect the results. This is useful for more accurately simulating
#' how a forecaster, nowcaster, or other algorithm would have behaved in real
#' time, factoring in reporting latency and data revisions; see
#' \href{https://cmu-delphi.github.io/epipredict/articles/backtesting.html}{`vignette("backtesting",
#' package="epipredict")`} for a walkthrough.
#'
#' This is similar to looping over versions and calling [`epix_as_of`], but has
#' some conveniences such as working naturally with [`grouped_epi_archive`]s,
#' optional time windowing, and syntactic sugar to make things shorter to write.
#'
#' @param .x An [`epi_archive`] or [`grouped_epi_archive`] object. If ungrouped,
#'   all data in `x` will be treated as part of a single data group.
#' @param .f Function, formula, or missing; together with `...` specifies the
#'   computation. The computation will be run on each requested group-version
#'   combination, with a time window filter applied if `.before` is supplied.
#'
#'   If `.f` is a function must have the form `function(x, g, v)` or
#'     `function(x, g, v, <additional configuration args>)`, where
#'
#'     - `x` is an `epi_df` with the same column names as the archive's `DT`,
#'       minus the `version` column. (Or, if `.all_versions = TRUE`, an
#'       `epi_archive` with the requested partial version history.)
#'
#'     - `g` is a one-row tibble containing the values of the grouping variables
#'       for the associated group.
#'
#'     - `v` (length-1) is the associated `version` (one of the requested
#'       `.versions`)
#'
#'     - `<additional configuration args>` are optional; you can add such
#'       arguments to your function and set them by passing them through the
#'       `...` argument to `epix_slide()`.
#'
#'   If a formula, `.f` can operate directly on columns accessed via `.x$var` or
#'   `.$var`, as in `~ mean (.x$var)` to compute a mean of a column `var` for
#'   each group-`ref_time_value` combination. The group key can be accessed via
#'   `.y` or `.group_key`, and the reference time value can be accessed via
#'   `.z`, `.version`, or `.ref_time_value`. If `.f` is missing, then `...` will
#'   specify the computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `.f` is missing, then the `...` is interpreted
#'   as a ["data-masking"][rlang::args_data_masking] expression or expressions
#'   for tidy evaluation; in addition to referring columns directly by name, the
#'   expressions have access to `.data` and `.env` pronouns as in `dplyr` verbs,
#'   and can also refer to `.x` (not the same as the input epi_archive),
#'   `.group_key` and `.version`/`.ref_time_value`. See details for more.
#' @param .before Optional; applies a `time_value` filter before running each
#'   computation. The default is not to apply a `time_value` filter. If
#'   provided, it should be a single integer or difftime that is compatible with
#'   the time_type of the time_value column. If an integer, then the minimum
#'   possible `time_value` included will be that many time steps (according to
#'   the `time_type`) before each requested `.version`. This window endpoint is
#'   inclusive. For example, if `.before = 14`, the `time_type` in the archive
#'   is "day", and the requested `.version` is January 15, then the smallest
#'   possible `time_value` possible in the snapshot will be January 1. Note that
#'   this does not mean that there will be 14 or 15 distinct `time_value`s
#'   actually appearing in the data; for most reporting streams, reporting as of
#'   January 15 won't include `time_value`s all the way through January 14, due
#'   to reporting latency. Unlike `epi_slide()`, `epix_slide()` won't fill in
#'   any missing `time_values` in this window.
#' @param .versions Requested versions on which to run the computation. Each
#'   requested `.version` also serves as the anchor point from which
#'   the `time_value` window specified by `.before` is drawn. If `.versions` is
#'   missing, it will be set to a regularly-spaced sequence of values set to
#'   cover the range of `version`s in the `DT` plus the `versions_end`; the
#'   spacing of values will be guessed (using the GCD of the skips between
#'   values).
#' @param .new_col_name Either `NULL` or a string indicating the name of the new
#'   column that will contain the derived values. The default, `NULL`, will use
#'   the name "slide_value" unless your slide computations output data frames,
#'   in which case they will be unpacked into the constituent columns and the
#'   data frame's column names will be used instead. If the resulting column
#'   name(s) overlap with the column names used for labeling the computations,
#'   which are `group_vars(x)` and `"version"`, then the values for these
#'   columns must be identical to the labels we assign.
#' @param .all_versions (Not the same as `.all_rows` parameter of `epi_slide`.)
#'   If `.all_versions = TRUE`, then the slide computation will be passed the
#'   version history (all versions `<= .version` where `.version` is one of the
#'   requested `.version`s), in `epi_archive` format. Otherwise, the slide
#'   computation will be passed only the most recent `version` for every unique
#'   `time_value`, in `epi_df` format. Default is `FALSE`.
#' @return A tibble whose columns are: the grouping variables (if any),
#'   `time_value`, containing the reference time values for the slide
#'   computation, and a column named according to the `.new_col_name` argument,
#'   containing the slide values. It will be grouped by the grouping variables.
#'
#' @details A few key distinctions between the current function and `epi_slide()`:
#'   1. In `.f` functions for `epix_slide`, one should not assume that the input
#'   data to contain any rows with `time_value` matching the computation's
#'   `.version`, due to reporting latency; for typical epidemiological
#'   surveillance data, observations pertaining to a particular time period
#'   (`time_value`) are first reported `as_of` some instant after that time
#'   period has ended. No time window completion is performed as in
#'   `epi_slide()`.
#'   2. The input class and columns are similar but different: `epix_slide`
#'   (with the default `.all_versions=FALSE`) keeps all columns and the
#'   `epi_df`-ness of the first argument to each computation; `epi_slide` only
#'   provides the grouping variables in the second input, and will convert the
#'   first input into a regular tibble if the grouping variables include the
#'   essential `geo_value` column. (With `.all_versions=TRUE`, `epix_slide`
#'   will provide an `epi_archive` rather than an `epi-df` to each
#'   computation.)
#'   3. The output class and columns are similar but different: `epix_slide()`
#'   returns a tibble containing only the grouping variables, `time_value`, and
#'   the new column(s) from the slide computations, whereas `epi_slide()`
#'   returns an `epi_df` with all original variables plus the new columns from
#'   the slide computations. (Both will mirror the grouping or ungroupedness of
#'   their input, with one exception: `epi_archive`s can have trivial
#'   (zero-variable) groupings, but these will be dropped in `epix_slide`
#'   results as they are not supported by tibbles.)
#'   4. There are no size stability checks or element/row recycling to maintain
#'   size stability in `epix_slide`, unlike in `epi_slide`. (`epix_slide` is
#'   roughly analogous to [`dplyr::group_modify`], while `epi_slide` is roughly
#'   analogous to [`dplyr::mutate`].)
#'   5. `.all_rows` is not supported in `epix_slide`; since the slide
#'   computations are allowed more flexibility in their outputs than in
#'   `epi_slide`, we can't guess a good representation for missing computations
#'   for excluded group-`.ref_time_value` pairs.
#'   6. The `.versions` default for `epix_slide` is based on making an
#'   evenly-spaced sequence out of the `version`s in the `DT` plus the
#'   `versions_end`, rather than all unique `time_value`s.
#'   7. `epix_slide()` computations can refer to the current element of
#'   `.versions` as either `.version` or `.ref_time_value`, while `epi_slide()`
#'   computations refer to the current element of `.ref_time_values` with
#'   `.ref_time_value`.
#'
#' Apart from the above distinctions, the interfaces between `epix_slide()` and
#' `epi_slide()` are the same.
#'
#' @examples
#' library(dplyr)
#'
#' # Request only a small set of versions, for example's sake:
#' requested_versions <-
#'   seq(as.Date("2020-09-02"), as.Date("2020-09-15"), by = "1 day")
#'
#' # Investigate reporting lag of `percent_cli` signal (though normally we'd
#' # probably work off of the dedicated `revision_summary()` function instead):
#' archive_cases_dv_subset %>%
#'   epix_slide(
#'     geowide_percent_cli_max_time = max(time_value[!is.na(percent_cli)]),
#'     geowide_percent_cli_rpt_lag = .version - geowide_percent_cli_max_time,
#'     .versions = requested_versions
#'   )
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     percent_cli_max_time = max(time_value[!is.na(percent_cli)]),
#'     percent_cli_rpt_lag = .version - percent_cli_max_time,
#'     .versions = requested_versions
#'   )
#'
#' # Backtest a forecaster "pseudoprospectively" (i.e., faithfully with respect
#' # to the data version history):
#' case_death_rate_archive %>%
#'   epix_slide(
#'     .versions = as.Date(c("2021-10-01", "2021-10-08")),
#'     function(x, g, v) {
#'       epipredict::arx_forecaster(
#'         x,
#'         outcome = "death_rate",
#'         predictors = c("death_rate_7d_av", "case_rate_7d_av")
#'       )$predictions
#'     }
#'   )
#' # See `vignette("backtesting", package="epipredict")` for a full walkthrough
#' # on backtesting forecasters, including plots, etc.
#'
#' # --- Advanced: ---
#'
#' # `epix_slide` with `all_versions=FALSE` (the default) applies a
#' # version-unaware computation to several versions of the data. We can also
#' # use `.all_versions=TRUE` to apply a version-*aware* computation to several
#' # versions of the data, again looking at characteristics of the data passed
#' # to each computation. In this case, each computation should expect an
#' # `epi_archive` containing the relevant version data:
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     function(x, gk, rtv) {
#'       x_tbl <- tibble::as_tibble(x)
#'       tibble(
#'         versions_start = if (nrow(x_tbl) == 0L) {
#'           "NA (0 rows)"
#'         } else {
#'           toString(min(x_tbl$version))
#'         },
#'         versions_end = x$versions_end,
#'         time_range = if (nrow(x_tbl) == 0L) {
#'           "0 `time_value`s"
#'         } else {
#'           sprintf("%s -- %s", min(x_tbl$time_value), max(x_tbl$time_value))
#'         },
#'         n = nrow(x_tbl),
#'         class1 = class(x)[[1L]]
#'       )
#'     },
#'     .before = 5, .all_versions = TRUE,
#'     .versions = requested_versions
#'   ) %>%
#'   ungroup() %>%
#'   # Focus on one geo_value so we can better see the columns above:
#'   filter(geo_value == "ca") %>%
#'   select(-geo_value)
#'
#' @export
epix_slide <- function(
  .x,
  .f,
  ...,
  .before = Inf,
  .versions = NULL,
  .new_col_name = NULL,
  .all_versions = FALSE
) {
  UseMethod("epix_slide")
}


#' @export
epix_slide.epi_archive <- function(
  .x,
  .f,
  ...,
  .before = Inf,
  .versions = NULL,
  .new_col_name = NULL,
  .all_versions = FALSE
) {
  # For an "ungrouped" slide, treat all rows as belonging to one big
  # group (group by 0 vars), like `dplyr::summarize`, and let the
  # resulting `grouped_epi_archive` handle the slide:
  epix_slide(
    group_by(.x),
    .f,
    ...,
    .before = .before, .versions = .versions,
    .new_col_name = .new_col_name, .all_versions = .all_versions
  ) %>%
    # We want a slide on ungrouped archives to output something
    # ungrouped, rather than retaining the trivial (0-variable)
    # grouping applied above. So we `ungroup()`. However, the current
    # `dplyr` implementation automatically ignores/drops trivial
    # groupings, so this is just a no-op for now.
    ungroup()
}


#' Default value for `ref_time_values` in an `epix_slide`
#'
#' @noRd
epix_slide_versions_default <- function(ea) {
  versions_with_updates <- c(archive_col(ea, "version"), ea$versions_end)
  if (ea$time_type == "yearmonth") {
    min(versions_with_updates) + seq(0, max(versions_with_updates) - min(versions_with_updates), by = 1)
  } else {
    tidyr::full_seq(versions_with_updates, guess_period(versions_with_updates))
  }
}

#' Filter an `epi_archive` object to keep only older versions
#'
#' Generates a filtered `epi_archive` from an `epi_archive` object, keeping
#' only rows with `version` falling on or before a specified date.
#'
#' @param x An `epi_archive` object.
#' @param max_version The latest version to include in the archive.
#' @return An `epi_archive` object
#'
#' @export
epix_truncate_versions_after <- function(x, max_version) {
  UseMethod("epix_truncate_versions_after")
}


#' @rdname epix_truncate_versions_after
#' @export
epix_truncate_versions_after.epi_archive <- function(x, max_version) {
  if (!identical(class(max_version), class(archive_col(x, "version")))) {
    cli_abort("`max_version` must have the same `class` as the archive's `version` column.")
  }
  assert_scalar(max_version, na.ok = FALSE)
  if (max_version > x$versions_end) {
    cli_abort("`max_version` must be at most `epi_archive$versions_end`.")
  }
  x <- archive_set_data(x, archive_filter_rows(x, archive_col(x, "version") <= max_version))
  # (^ this filter operation seems to always copy the DT, even if it
  # keeps every entry; we don't guarantee this behavior in
  # documentation, though, so we could change to alias in this case)
  if (!is.na(x$clobberable_versions_start) && x$clobberable_versions_start > max_version) {
    x$clobberable_versions_start <- NA
  }
  x$versions_end <- max_version
  x
}


# Helpers for `group_by`:

#' Make non-testing mock to get [`dplyr::dplyr_col_modify`] input
#'
#' A workaround for `dplyr:::mutate_cols` not being exported and directly
#' applying test mock libraries likely being impossible (due to mocking another
#' package's S3 generic or method).
#'
#' Use solely with a single call to the [`dplyr::mutate`] function and then
#' `destructure_col_modify_recorder_df`; other applicable operations from
#' [dplyr::dplyr_extending] have not been implemented.
#'
#' @param parent_df the "parent class" data frame to wrap
#' @return a `col_modify_recorder_df`
#'
#' @noRd
new_col_modify_recorder_df <- function(parent_df) {
  assert_class(parent_df, "data.frame")
  `class<-`(parent_df, c("col_modify_recorder_df", class(parent_df)))
}


#' Extract unchanged parent-class data frame from a `new_col_modify_recorder_df`
#'
#' @param col_modify_recorder_df an instance of a `col_modify_recorder_df`
#' @return named list with elements `unchanged_parent_df`, `cols`; `cols` is the
#'   input to [`dplyr::dplyr_col_modify`] that this class was designed to record
#'
#' @noRd
destructure_col_modify_recorder_df <- function(col_modify_recorder_df) {
  assert_class(col_modify_recorder_df, "col_modify_recorder_df")
  list(
    unchanged_parent_df = col_modify_recorder_df %>%
      `attr<-`("epiprocess::col_modify_recorder_df::cols", NULL) %>%
      `class<-`(setdiff(class(.data), "col_modify_recorder_df")),
    cols = attr(col_modify_recorder_df,
      "epiprocess::col_modify_recorder_df::cols",
      exact = TRUE
    )
  )
}


#' `dplyr_col_modify` method that simply records the `cols` argument
#'
#' Must export S3 methods in R >= 4.0, even if they're only designed to be
#' package internals, and must import any corresponding upstream S3 generic
#' functions:
#' @importFrom dplyr dplyr_col_modify
#' @export
#' @noRd
dplyr_col_modify.col_modify_recorder_df <- function(data, cols) {
  if (!is.null(attr(data, "epiprocess::col_modify_recorder_df::cols", exact = TRUE))) {
    cli_abort("`col_modify_recorder_df` can only record `cols` once",
      internal = TRUE
    )
  }
  attr(data, "epiprocess::col_modify_recorder_df::cols") <- cols
  data
}


#' [`dplyr::filter`] for `epi_archive`s
#'
#' @param .data an `epi_archive`
#' @param ... as in [`dplyr::filter`]; using the `version` column is not allowed
#'   unless you use `.format_aware = TRUE`; see details.
#' @param .by as in [`dplyr::filter`]
#' @param .format_aware optional, `TRUE` or `FALSE`; default `FALSE`. See
#'   details.
#'
#' @details
#'
#' By default, using the `version` column or measurement columns is disabled as
#' it's easy to get unexpected results. See if either [`epix_as_of`] or
#' [`epix_slide`] works for any version selection you have in mind: for version
#' selection, see the `version` or `.versions` args, respectively; for
#' measurement column-based filtering, try `filter`ing after `epix_as_of` or
#' inside the `.f` in `epix_slide()`. If they don't cover your use case, then
#' you can set `.format_aware = TRUE` to enable usage of these columns, but be
#' careful to:
#' * Factor in that the archive rows may have been converted into a compact
#'   format based on diffing consecutive versions, and the last version of each
#'   observation in the archive will always be carried forward to future
#'   `version`s`; see details of [`as_epi_archive`].
#' * Set `clobberable_versions_start` and `versions_end` of the result
#'   appropriately after the `filter` call. They will be initialized with the
#'   same values as in `.data`.
#'
#' `dplyr::filter` also has an optional argument `.preserve`, which should not
#' have an impact on (ungrouped) `epi_archive`s, and `grouped_epi_archive`s do
#' not currently support `dplyr::filter`.
#'
#' @examples
#'
#' # Filter to one location and a particular time range:
#' archive_cases_dv_subset %>%
#'   filter(geo_value == "fl", time_value >= as.Date("2020-10-01"))
#'
#' # Convert to weekly by taking the Saturday data for each week, so that
#' # `case_rate_7d_av` represents a Sun--Sat average:
#' archive_cases_dv_subset %>%
#'   filter(as.POSIXlt(time_value)$wday == 6L)
#'
#' # Filtering involving the `version` column or measurement columns requires
#' # extra care. See epix_as_of and epix_slide instead for some common
#' # operations. One semi-common operation that ends up being fairly simple is
#' # treating observations as finalized after some amount of time, and ignoring
#' # any revisions that were made after that point:
#' archive_cases_dv_subset %>%
#'   filter(
#'     version <= time_value + as.difftime(60, units = "days"),
#'     .format_aware = TRUE
#'   )
#'
#' @export
filter.epi_archive <- function(.data, ..., .by = NULL, .format_aware = FALSE) {
  # Eager tibble: user `filter(..., .env$x)` pinning must work, and dtplyr's
  # translation of `.env$` is brittle. `archive_tbl` is the eager accessor.
  in_tbl <- archive_tbl(.data)
  if (.format_aware) {
    out_tbl <- in_tbl %>%
      filter(..., .by = {{ .by }})
  } else {
    measurement_colnames <- setdiff(archive_colnames(.data), key_colnames(.data))
    forbidden_colnames <- c("version", measurement_colnames)
    out_tbl <- in_tbl %>%
      filter(
        # Add our own fake filter arg to the user's ..., to update the data mask
        # to prevent `version` column usage.
        {
          # We should be evaluating inside the data mask. To disable both
          # `version` and `.data$version` etc., we need to go to the ancestor
          # environment containing the data mask's column bindings. This is
          # likely just the parent env, but search to make sure, in a way akin
          # to `<<-`:
          e <- environment()
          while (!identical(e, globalenv()) && !identical(e, emptyenv())) { # nolint:vector_logic_linter
            if ("version" %in% names(e)) {
              # This is where the column bindings are. Replace the forbidden ones.
              # They are expected to be active bindings, so directly
              # assigning has issues; `rm` first.
              rm(list = forbidden_colnames, envir = e)
              eval_env <- new.env(parent = asNamespace("epiprocess")) # see (2) below
              delayedAssign(
                "version",
                cli_abort(c(
                  "Using `version` in `filter.epi_archive` may produce unexpected results.",
                  ">" = "See if `epix_as_of` or `epix_slide` would work instead.",
                  ">" = "If not, see `?filter.epi_archive` details for how to proceed."
                ), class = "epiprocess__filter_archive__used_version"),
                eval.env = eval_env,
                assign.env = e
              )
              for (measurement_colname in measurement_colnames) {
                # Record current `measurement_colname` and set up execution for
                # the promise for the error in its own dedicated environment, so
                # that (1) `for` loop updating its value and `rm` cleanup don't
                # mess things up. We can also (2) prevent changes to data mask
                # ancestry (to involve user's quosure env rather than our
                # quosure env) or contents (from edge case of user binding
                # functions inside the mask) from potentially interfering by
                # setting the promise's execution environment to skip over the
                # data mask.
                eval_env <- new.env(parent = asNamespace("epiprocess"))
                eval_env[["local_measurement_colname"]] <- measurement_colname
                delayedAssign(
                  measurement_colname,
                  cli_abort(c(
                    "Using `{format_varname(local_measurement_colname)}`
                     in `filter.epi_archive` may produce unexpected results.",
                    ">" = "See `?filter.epi_archive` details for how to proceed."
                  ), class = "epiprocess__filter_archive__used_measurement"),
                  eval.env = eval_env,
                  assign.env = e
                )
              }
              break
            }
            e <- parent.env(e)
          }
          # Don't mask similarly-named user objects in ancestor envs:
          rm(list = c("e", "measurement_colname", "eval_env"))
          TRUE
        },
        ...,
        .by = {{ .by }}
      )
  }
  # We could try to re-infer the geo_type, e.g., when filtering from
  # national+state to just state. However, we risk inference failures such as
  # "hrr" -> "hhs" from filtering to hrr 10, or "custom" -> USA-related when
  # working with non-USA data:
  out_geo_type <- .data$geo_type
  if (.data$time_type == "day") {
    # We might be going from daily to weekly; re-infer:
    out_time_type <- guess_time_type(out_tbl$time_value)
  } else {
    # We might be filtering weekly to a single time_value; avoid re-inferring to
    # stay "week". Or in other cases, just skip inferring, as re-inferring is
    # expected to match the input time_type:
    out_time_type <- .data$time_type
  }
  # Even if they narrow down to just a single value of an other_keys column,
  # it's probably still better (& simpler) to treat it as an other_keys column
  # since it still exists in the result:
  out_other_keys <- .data$other_keys
  # `filter` makes no guarantees about not aliasing columns in its result when
  # the filter condition is all TRUE, so don't setDT.
  result <- as_epi_archive_like(
    .data, out_tbl,
    other_keys = out_other_keys,
    compactify = FALSE,
    # Assume version-related metadata unchanged; part of why we want to push
    # back on filter expressions like `.data$version <= .env$as_of`:
    clobberable_versions_start = .data$clobberable_versions_start,
    versions_end = .data$versions_end
  )
  result$geo_type <- out_geo_type
  result$time_type <- out_time_type
  # Filtering down rows while keeping all (ukey) columns should preserve ukey
  # uniqueness.
  result
}
