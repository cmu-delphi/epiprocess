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
#' @param all_versions If `all_versions = TRUE`, then the output will be in
#'   `epi_archive` format, and contain rows in the specified `time_value` range
#'   having `version <= max_version`. The resulting object will cover a
#'   potentially narrower `version` and `time_value` range than `x`, depending
#'   on user-provided arguments. Otherwise, there will be one row in the output
#'   for the `max_version` of each `time_value`. Default is `FALSE`.
#' @return An `epi_df` object.
#'
#' @examples
#' epix_as_of(
#'   archive_cases_dv_subset,
#'   max_version = max(archive_cases_dv_subset$DT$version)
#' )
#'
#' range(archive_cases_dv_subset$DT$version) # 2020-06-02 -- 2021-12-01
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
#'   archive_cases_dv_subset$DT,
#'   # Suppose last version with an update could potentially be rewritten
#'   # (a.k.a. "hotfixed", "clobbered", etc.):
#'   clobberable_versions_start = max(archive_cases_dv_subset$DT$version),
#'   # Suppose today is the following day, and there are no updates out yet:
#'   versions_end = max(archive_cases_dv_subset$DT$version) + 1L,
#'   compactify = TRUE
#' )
#'
#' epix_as_of(archive_cases_dv_subset2, max(archive_cases_dv_subset$DT$version))
#'
#' @importFrom data.table between key
#' @export
epix_as_of <- function(x, max_version, min_time_value = -Inf, all_versions = FALSE) {
  assert_class(x, "epi_archive")

  other_keys <- setdiff(
    key(x$DT),
    c("geo_value", "time_value", "version")
  )
  if (length(other_keys) == 0) other_keys <- NULL

  # Check a few things on max_version
  if (!identical(class(max_version), class(x$DT$version))) {
    cli_abort(
      "`max_version` must have the same `class` vector as `epi_archive$DT$version`."
    )
  }
  if (!identical(typeof(max_version), typeof(x$DT$version))) {
    cli_abort(
      "`max_version` must have the same `typeof` as `epi_archive$DT$version`."
    )
  }
  assert_scalar(max_version, na.ok = FALSE)
  if (max_version > x$versions_end) {
    cli_abort("`max_version` must be at most `epi_archive$versions_end`.")
  }
  assert_logical(all_versions, len = 1)
  if (!is.na(x$clobberable_versions_start) && max_version >= x$clobberable_versions_start) {
    cli_warn(
      'Getting data as of some recent version which could still be
      overwritten (under routine circumstances) without assigning a new
      version number (a.k.a. "clobbered").  Thus, the snapshot that we
      produce here should not be expected to be reproducible later. See
      `?epi_archive` for more info and `?epix_as_of` on how to muffle.',
      class = "epiprocess__snapshot_as_of_clobberable_version"
    )
  }

  # Filter by version and return
  if (all_versions) {
    # epi_archive is copied into result, so we can modify result directly
    result <- epix_truncate_versions_after(x, max_version)
    result$DT <- result$DT[time_value >= min_time_value, ] # nolint: object_usage_linter
    return(result)
  }

  # Make sure to use data.table ways of filtering and selecting
  as_of_epi_df <- x$DT[time_value >= min_time_value & version <= max_version, ] %>% # nolint: object_usage_linter
    unique(
      by = c("geo_value", "time_value", other_keys),
      fromLast = TRUE
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(-"version") %>%
    as_epi_df(
      as_of = max_version,
      additional_metadata = c(
        x$additional_metadata,
        list(other_keys = other_keys)
      )
    )

  return(as_of_epi_df)
}


#' Fill `epi_archive` unobserved history
#'
#' @description
#' Sometimes, due to upstream data pipeline issues, we have to work with a
#' version history that isn't completely up to date, but with functions that
#' expect archives that are completely up to date, or equally as up-to-date as
#' another archive. This function provides one way to approach such mismatches:
#' pretend that we've "observed" additional versions, filling in these versions
#' with NAs or extrapolated values.
#'
#' @param x An `epi_archive`
#' @param fill_versions_end Length-1, same class&type as `x$version`: the
#'   version through which to fill in missing version history; this will be the
#'   result's `$versions_end` unless it already had a later
#'   `$versions_end`.
#' @param how Optional; `"na"` or `"locf"`: `"na"` will fill in any missing
#'   required version history with `NA`s, by inserting (if necessary) an update
#'   immediately after the current `$versions_end` that revises all
#'   existing measurements to be `NA` (this is only supported for `version`
#'   classes with a `next_after` implementation); `"locf"` will fill in missing
#'   version history with the last version of each observation carried forward
#'   (LOCF), by leaving the update `$DT` alone (other `epi_archive` methods are
#'   based on LOCF).  Default is `"na"`.
#'
#' @importFrom data.table copy ":="
#' @importFrom rlang arg_match
#' @return An `epi_archive`
#' @export
epix_fill_through_version <- function(x, fill_versions_end,
                                      how = c("na", "locf")) {
  assert_class(x, "epi_archive")

  validate_version_bound(fill_versions_end, x$DT, na_ok = FALSE)
  how <- arg_match(how)
  if (x$versions_end < fill_versions_end) {
    new_dt <- switch(how,
      "na" = {
        # old DT + a version consisting of all NA observations
        # immediately after the last currently/actually-observed
        # version. Note that this NA-observation version must only be
        # added if `epi_archive` is outdated.
        nonversion_key_cols <- setdiff(key(x$DT), "version")
        nonkey_cols <- setdiff(names(x$DT), key(x$DT))
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
        nonversion_key_vals_ever_recorded <- unique(x$DT, by = nonversion_key_cols)
        # In edge cases, the `unique` result can alias the original
        # DT; detect and copy if necessary:
        if (identical(address(x$DT), address(nonversion_key_vals_ever_recorded))) {
          nonversion_key_vals_ever_recorded <- data.table::copy(nonversion_key_vals_ever_recorded)
        }
        next_version_dt <- nonversion_key_vals_ever_recorded[
          , version := next_version_tag
        ][
          # this makes the class of these columns logical (`NA` is a
          # logical NA; we're relying on the rbind below to convert to
          # the proper class&typeof)
          , (nonkey_cols) := NA
        ]
        # full result DT:
        setkeyv(rbind(x$DT, next_version_dt), key(x$DT))[]
      },
      "locf" = {
        # just the old DT; LOCF is built into other methods:
        x$DT
      }
    )
    new_versions_end <- fill_versions_end
    # Update `epi_archive` all at once with simple, error-free operations +
    # return below:
    x$DT <- new_dt
    x$versions_end <- new_versions_end
  } else {
    # Already sufficiently up to date; nothing to do.
  }
  return(x)
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
#' @param sync Optional; `"forbid"`, `"na"`, `"locf"`, or `"truncate"`; in the
#'   case that `x$versions_end` doesn't match `y$versions_end`, what do we do?:
#'   `"forbid"`: emit an error; "na": use `max(x$versions_end, y$versions_end)`
#'   as the result's `versions_end`, but ensure that, if we request a snapshot
#'   as of a version after `min(x$versions_end, y$versions_end)`, the
#'   observation columns from the less up-to-date archive will be all NAs (i.e.,
#'   imagine there was an update immediately after its `versions_end` which
#'   revised all observations to be `NA`); `"locf"`: use `max(x$versions_end,
#'   y$versions_end)` as the result's `versions_end`, allowing the last version
#'   of each observation to be carried forward to extrapolate unavailable
#'   versions for the less up-to-date input archive (i.e., imagining that in the
#'   less up-to-date archive's data set remained unchanged between its actual
#'   `versions_end` and the other archive's `versions_end`); or `"truncate"`:
#'   use `min(x$versions_end, y$versions_end)` as the result's `versions_end`,
#'   and discard any rows containing update rows for later versions.
#' @param compactify Optional; `TRUE`, `FALSE`, or `NULL`; should the result be
#'   compactified? See `as_epi_archive()` for an explanation of what this means.
#'   Default here is `TRUE`.
#' @return the resulting `epi_archive`
#'
#' @details In all cases, `additional_metadata` will be an empty list, and
#'   `clobberable_versions_start` will be set to the earliest version that could
#'   be clobbered in either input archive.
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
#'
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-03", "2024-08-03")),
#'   signal2 = c(2, 3)
#' )
#'
#'
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#'
#' merged <- epix_merge(s1, s2, sync = "locf")
#' merged[["DT"]]
#'
#' # Example 2
#' # The s1 signal at August 1st gets revised from 12 to 13 on August 3rd
#' s1 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-03")),
#'   version = as.Date(c("2024-08-01", "2024-08-03", "2024-08-03", "2024-08-03")),
#'   signal1 = c(12, 13, 22, 19)
#' )
#'
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-02", "2024-08-02")),
#'   signal2 = c(4, 5),
#' )
#'
#'
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#'
#' merged <- epix_merge(s1, s2, sync = "locf")
#' merged[["DT"]]
#'
#'
#' # Example 3:
#' s1 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
#'   version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
#'   signal1 = c(14, 11, 9)
#' )
#'
#' # The s2 signal at August 1st gets revised from 3 to 5 on August 3rd
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-02", "2024-08-03", "2024-08-03")),
#'   signal2 = c(3, 5, 2),
#' )
#'
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
#'
#' # Some LOCF for signal 1 as signal 2 gets updated
#' merged <- epix_merge(s1, s2, sync = "locf")
#' merged[["DT"]]
#' @importFrom data.table key set setkeyv
#' @export
epix_merge <- function(x, y,
                       sync = c("forbid", "na", "locf", "truncate"),
                       compactify = TRUE) {
  assert_class(x, "epi_archive")
  assert_class(y, "epi_archive")
  sync <- rlang::arg_match(sync)

  if (!identical(x$geo_type, y$geo_type)) {
    cli_abort("`x` and `y` must have the same `$geo_type`")
  }

  if (!identical(x$time_type, y$time_type)) {
    cli_abort("`x` and `y` must share data type on their `time_value` column.")
  }

  if (length(x$additional_metadata) != 0L) {
    cli_warn("x$additional_metadata won't appear in merge result",
      class = "epiprocess__epix_merge_ignores_additional_metadata"
    )
  }
  if (length(y$additional_metadata) != 0L) {
    cli_warn("y$additional_metadata won't appear in merge result",
      class = "epiprocess__epix_merge_ignores_additional_metadata"
    )
  }
  result_additional_metadata <- list()

  result_clobberable_versions_start <-
    if (all(is.na(c(x$clobberable_versions_start, y$clobberable_versions_start)))) {
      NA # (any type of NA is fine here)
    } else {
      min(c(x$clobberable_versions_start, y$clobberable_versions_start), na.rm = TRUE)
    }

  if (sync == "forbid") {
    if (!identical(x$versions_end, y$versions_end)) {
      cli_abort(paste(
        "`x` and `y` were not equally up to date version-wise:",
        "`x$versions_end` was not identical to `y$versions_end`;",
        "either ensure that `x` and `y` are equally up to date before merging,",
        "or specify how to deal with this using `sync`"
      ), class = "epiprocess__epix_merge_unresolved_sync")
    } else {
      new_versions_end <- x$versions_end
      x_dt <- x$DT
      y_dt <- y$DT
    }
  } else if (sync %in% c("na", "locf")) {
    new_versions_end <- max(c(x$versions_end, y$versions_end))
    x_dt <- epix_fill_through_version(x, new_versions_end, sync)$DT
    y_dt <- epix_fill_through_version(y, new_versions_end, sync)$DT
  } else if (sync == "truncate") {
    new_versions_end <- min(c(x$versions_end, y$versions_end))
    x_dt <- x$DT[x[["DT"]][["version"]] <= new_versions_end, names(x$DT), with = FALSE]
    y_dt <- y$DT[y[["DT"]][["version"]] <= new_versions_end, names(y$DT), with = FALSE]
  } else {
    cli_abort("unimplemented")
  }

  # key(x_dt) should be the same as key(x$DT) and key(y_dt) should be the same
  # as key(y$DT). Below, we only use {x,y}_DT in the code (making it easier to
  # split the code into separate functions if we wish), but still refer to
  # {x,y}$DT in the error messages (further relying on this assumption).
  #
  # Check&ensure that the above assumption; if it didn't already hold, we likely
  # have a bug in the preprocessing, a weird/invalid archive as input, and/or a
  # data.table version with different semantics (which may break other parts of
  # our code).
  x_dt_key_as_expected <- identical(key(x$DT), key(x_dt))
  y_dt_key_as_expected <- identical(key(y$DT), key(y_dt))
  if (!x_dt_key_as_expected || !y_dt_key_as_expected) {
    cli_warn("
      `epiprocess` internal warning (please report): pre-processing for
      epix_merge unexpectedly resulted in an intermediate data table (or
      tables) with a different key than the corresponding input archive.
      Manually setting intermediate data table keys to the expected values.
    ", internal = TRUE)
    setkeyv(x_dt, key(x$DT))
    setkeyv(y_dt, key(y$DT))
  }
  # Without some sort of annotations of what various columns represent, we can't
  # do something that makes sense when merging archives with mismatched keys.
  # E.g., even if we assume extra keys represent demographic breakdowns, a
  # sensible default treatment of count-type and rate-type value columns would
  # differ.
  if (!identical(sort(key(x_dt)), sort(key(y_dt)))) {
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
  # `by` cols = result (and each input's) `key` cols, and determine
  # the row set, determined using a full join via `merge`
  #
  # non-`by` cols = "value"-ish cols, and are looked up with last
  #                 version carried forward via rolling joins
  by <- key(x_dt) # = some perm of key(y_dt)
  if (!all(c("geo_value", "time_value", "version") %in% key(x_dt))) {
    cli_abort('Invalid `by`; `by` is currently set to the common `key` of
           the two archives, and is expected to contain
           "geo_value", "time_value", and "version".',
      class = "epiprocess__epi_archive_must_have_required_key_cols"
    )
  }
  if (length(by) < 1L || utils::tail(by, 1L) != "version") {
    cli_abort('Invalid `by`; `by` is currently set to the common `key` of
           the two archives, and is expected to have a "version" as
           the last key col.',
      class = "epiprocess__epi_archive_must_have_version_at_end_of_key"
    )
  }
  x_nonby_colnames <- setdiff(names(x_dt), by)
  y_nonby_colnames <- setdiff(names(y_dt), by)
  if (length(intersect(x_nonby_colnames, y_nonby_colnames)) != 0L) {
    cli_abort("
            `x` and `y` DTs have overlapping non-by column names;
            this is currently not supported; please manually fix up first:
            any overlapping columns that can are key-like should be
            incorporated into the key, and other columns should be renamed.
          ", class = "epiprocess__epix_merge_x_y_must_not_have_overlapping_nonby_colnames")
  }
  x_by_vals <- x_dt[, by, with = FALSE]
  if (anyDuplicated(x_by_vals) != 0L) {
    cli_abort("
            The `by` columns must uniquely determine rows of `x$DT`;
            the `by` is currently set to the common `key` of the two
            archives, so this can be resolved by adding key-like columns
            to `x`'s key (to get a unique key).
          ", class = "epiprocess__epix_merge_by_cols_must_act_as_unique_key")
  }
  y_by_vals <- y_dt[, by, with = FALSE]
  if (anyDuplicated(y_by_vals) != 0L) {
    cli_abort("
            The `by` columns must uniquely determine rows of `y$DT`;
            the `by` is currently set to the common `key` of the two
            archives, so this can be resolved by adding key-like columns
            to `y`'s key (to get a unique key).
          ", class = "epiprocess__epix_merge_by_cols_must_act_as_unique_key")
  }
  result_dt <- merge(x_by_vals, y_by_vals,
    by = by,
    # We must have `all=TRUE` or we may skip updates
    # from x and/or y and corrupt the history
    all = TRUE,
    # We don't want Cartesian products, but the
    # by-is-unique-key check above already ensures
    # this. (Note that `allow.cartesian=FALSE` doesn't
    # actually catch all Cartesian products anyway.)
    # Disable superfluous check:
    allow.cartesian = TRUE
  )
  set(
    result_dt, , x_nonby_colnames,
    x_dt[result_dt[, by, with = FALSE], x_nonby_colnames,
      with = FALSE,
      # It's good practice to specify `on`, and we must
      # explicitly specify `on` if there's a potential key vs.
      # by order mismatch (not possible currently for x
      # with by = key(x$DT), but possible for y):
      on = by,
      # last version carried forward:
      roll = TRUE,
      # requesting non-version key that doesn't exist in the other archive,
      # or before its first version, should result in NA
      nomatch = NA,
      # see note on `allow.cartesian` above; currently have a
      # similar story here.
      allow.cartesian = TRUE
    ]
  )
  set(
    result_dt, , y_nonby_colnames,
    y_dt[result_dt[, by, with = FALSE], y_nonby_colnames,
      with = FALSE,
      on = by,
      roll = TRUE,
      nomatch = NA,
      allow.cartesian = TRUE
    ]
  )
  # The key could be unset in case of a key vs. by order mismatch as
  # noted above. Ensure that we keep it:
  setkeyv(result_dt, by)

  return(as_epi_archive(
    result_dt[], # clear data.table internal invisibility flag if set
    other_keys = setdiff(key(result_dt), c("geo_value", "time_value", "version")),
    additional_metadata = result_additional_metadata,
    # It'd probably be better to pre-compactify before the merge, and might be
    # guaranteed not to be necessary to compactify the merge result if the
    # inputs are already compactified, but at time of writing we don't have
    # compactify in its own method or field, and it seems like it should be
    # pretty fast anyway.
    compactify = compactify,
    clobberable_versions_start = result_clobberable_versions_start,
    versions_end = new_versions_end
  ))
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
  # We don't want to directly use `dplyr::mutate` on the `$DT`, as:
  # - `mutate` behavior, including the output class, changes depending on
  #   whether `dtplyr` < 1.3.0 is loaded and would require post-processing
  # - behavior with `dtplyr` isn't fully compatible
  # - it doesn't give the desired details, and `rlang::exprs_auto_name` does not
  #   appropriately handle the `= NULL` and `= <data.frame>` tidyeval cases
  # Instead:
  # - Use `as.list` to get a shallow copy (undocumented, but apparently
  #   intended, behavior), then `as_tibble` (also shallow, given a list) to get
  #   back to something that will use `dplyr`'s included `mutate` method(s),
  #   then convert this using shallow operations into a `data.table`.
  # - Use `col_modify_recorder_df` to get the desired details.
  in_tbl <- tibble::as_tibble(as.list(.data$DT), .name_repair = "minimal")
  col_modify_cols <-
    destructure_col_modify_recorder_df(
      mutate(new_col_modify_recorder_df(in_tbl), ...)
    )[["cols"]]
  invalidated_key_col_is <-
    which(purrr::map_lgl(key(.data$DT), function(key_colname) {
      key_colname %in% names(col_modify_cols) &&
        !rlang::is_reference(in_tbl[[key_colname]], col_modify_cols[[key_colname]])
    }))
  if (length(invalidated_key_col_is) != 0L) {
    rlang::abort(paste_lines(c(
      "Key columns must not be replaced or removed.",
      wrap_varnames(key(.data$DT)[invalidated_key_col_is],
        initial = "Flagged key cols: "
      )
    )))
  } else {
    # Have `dplyr` do the `dplyr_col_modify`, keeping the column-level-aliasing
    # and must-copy-on-write-if-refcount-more-than-1 model, obtaining a tibble,
    # then convert it into a `data.table`. The key should still be valid
    # (assuming that the user did not explicitly alter `key(.data$DT)` or the
    # columns by reference somehow within `...` tidyeval-style computations, or
    # trigger refcount-1 alterations due to still having >1 refcounts on the
    # columns), set the "sorted" attribute accordingly to prevent attempted
    # sorting (including potential extra copies) or sortedness checking, then
    # `setDT` (rather than `as.data.table`, in order to prevent column copying
    # to establish ownership according to `data.table`'s memory model).
    out_dt <- dplyr::dplyr_col_modify(in_tbl, col_modify_cols) %>%
      data.table::setattr("sorted", data.table::key(.data$DT)) %>%
      data.table::setDT(key = key(.data$DT))
    out_archive <- .data
    out_archive$DT <- out_dt
    request_names <- names(col_modify_cols)
    return(list(
      archive = out_archive,
      request_names = request_names
    ))
    # (We might also consider special-casing when `mutate` hands back something
    # equivalent (in some sense) to the input (probably only encountered when
    # we're dealing with `group_by`), and using just `$DT`, not a shallow copy,
    # in the result, primarily in order to hedge against `as.list` or `setDT`
    # changing their behavior and generating deep copies somehow. This could
    # also prevent storage, and perhaps also generation, of shallow copies, but
    # this seems unlikely to be a major gain unless it helps enable some
    # in-place modifications of refcount-1 columns (although detecting this case
    # seems to be common across `group_by` implementations; maybe there is
    # something there).)
  }
}


#' Slide a function over variables in an `epi_archive` or `grouped_epi_archive`
#'
#' Slides a given function over variables in an `epi_archive` object. This
#' behaves similarly to `epi_slide()`, with the key exception that it is
#' version-aware: the sliding computation at any given reference time t is
#' performed on **data that would have been available as of t**. See the
#' [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x An [`epi_archive`] or [`grouped_epi_archive`] object. If ungrouped,
#'   all data in `x` will be treated as part of a single data group.
#' @param f Function, formula, or missing; together with `...` specifies the
#'   computation to slide. To "slide" means to apply a computation over a
#'   sliding (a.k.a. "rolling") time window for each data group. The window is
#'   determined by the `before` parameter described below. One time step is
#'   typically one day or one week; see [`epi_slide`] details for more
#'   explanation. If a function, `f` must take an `epi_df` with the same
#'   column names as the archive's `DT`, minus the `version` column; followed
#'   by a one-row tibble containing the values of the grouping variables for
#'   the associated group; followed by a reference time value, usually as a
#'   `Date` object; followed by any number of named arguments. If a formula,
#'   `f` can operate directly on columns accessed via `.x$var` or `.$var`, as
#'   in `~ mean (.x$var)` to compute a mean of a column `var` for each
#'   group-`ref_time_value` combination. The group key can be accessed via
#'   `.y` or `.group_key`, and the reference time value can be accessed via
#'   `.z` or `.ref_time_value`. If `f` is missing, then `...` will specify the
#'   computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then `...` is interpreted as an
#'   expression for tidy evaluation; in addition to referring to columns
#'   directly by name, the expression has access to `.data` and `.env` pronouns
#'   as in `dplyr` verbs, and can also refer to the `.group_key` and
#'   `.ref_time_value`. See details of [`epi_slide`].
#' @param before How far `before` each `ref_time_value` should the sliding
#'   window extend? If provided, should be a single, non-NA,
#'   [integer-compatible][vctrs::vec_cast] number of time steps. This window
#'   endpoint is inclusive. For example, if `before = 7`, and one time step is
#'   one day, then to produce a value for a `ref_time_value` of January 8, we
#'   apply the given function or formula to data (for each group present) with
#'   `time_value`s from January 1 onward, as they were reported on January 8.
#'   For typical disease surveillance sources, this will not include any data
#'   with a `time_value` of January 8, and, depending on the amount of reporting
#'   latency, may not include January 7 or even earlier `time_value`s. (If
#'   instead the archive were to hold nowcasts instead of regular surveillance
#'   data, then we would indeed expect data for `time_value` January 8. If it
#'   were to hold forecasts, then we would expect data for `time_value`s after
#'   January 8, and the sliding window would extend as far after each
#'   `ref_time_value` as needed to include all such `time_value`s.)
#' @param ref_time_values Reference time values / versions for sliding
#'   computations; each element of this vector serves both as the anchor point
#'   for the `time_value` window for the computation and the `max_version`
#'   `epix_as_of` which we fetch data in this window. If missing, then this will
#'   set to a regularly-spaced sequence of values set to cover the range of
#'   `version`s in the `DT` plus the `versions_end`; the spacing of values will
#'   be guessed (using the GCD of the skips between values).
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the slide results be held in a list column, or be
#'   [unchopped][tidyr::unchop]/[unnested][tidyr::unnest]? Default is `FALSE`,
#'   in which case a list object returned by `f` would be unnested (using
#'   [`tidyr::unnest()`]), and, if the slide computations output data frames,
#'   the names of the resulting columns are given by prepending `new_col_name`
#'   to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_versions (Not the same as `all_rows` parameter of `epi_slide`.) If
#'   `all_versions = TRUE`, then `f` will be passed the version history (all
#'   `version <= ref_time_value`) for rows having `time_value` between
#'   `ref_time_value - before` and `ref_time_value`. Otherwise, `f` will be
#'   passed only the most recent `version` for every unique `time_value`.
#'   Default is `FALSE`.
#' @return A tibble whose columns are: the grouping variables, `time_value`,
#'   containing the reference time values for the slide computation, and a
#'   column named according to the `new_col_name` argument, containing the slide
#'   values.
#'
#' @details A few key distinctions between the current function and `epi_slide()`:
#'   1. In `f` functions for `epix_slide`, one should not assume that the input
#'   data to contain any rows with `time_value` matching the computation's
#'   `ref_time_value` (accessible via `attributes(<data>)$metadata$as_of`); for
#'   typical epidemiological surveillance data, observations pertaining to a
#'   particular time period (`time_value`) are first reported `as_of` some
#'   instant after that time period has ended.
#'   2. `epix_slide()` doesn't accept an `after` argument; its windows extend
#'   from `before` time steps before a given `ref_time_value` through the last
#'   `time_value` available as of version `ref_time_value` (typically, this
#'   won't include `ref_time_value` itself, as observations about a particular
#'   time interval (e.g., day) are only published after that time interval
#'   ends); `epi_slide` windows extend from `before` time steps before a
#'   `ref_time_value` through `after` time steps after `ref_time_value`.
#'   3. The input class and columns are similar but different: `epix_slide`
#'   (with the default `all_versions=FALSE`) keeps all columns and the
#'   `epi_df`-ness of the first argument to each computation; `epi_slide` only
#'   provides the grouping variables in the second input, and will convert the
#'   first input into a regular tibble if the grouping variables include the
#'   essential `geo_value` column. (With `all_versions=TRUE`, `epix_slide` will
#'   will provide an `epi_archive` rather than an `epi-df` to each
#'   computation.)
#'   4. The output class and columns are similar but different: `epix_slide()`
#'   returns a tibble containing only the grouping variables, `time_value`, and
#'   the new column(s) from the slide computations, whereas `epi_slide()`
#'   returns an `epi_df` with all original variables plus the new columns from
#'   the slide computations. (Both will mirror the grouping or ungroupedness of
#'   their input, with one exception: `epi_archive`s can have trivial
#'   (zero-variable) groupings, but these will be dropped in `epix_slide`
#'   results as they are not supported by tibbles.)
#'   5. There are no size stability checks or element/row recycling to maintain
#'   size stability in `epix_slide`, unlike in `epi_slide`. (`epix_slide` is
#'   roughly analogous to [`dplyr::group_modify`], while `epi_slide` is roughly
#'   analogous to `dplyr::mutate` followed by `dplyr::arrange`) This is detailed
#'   in the "advanced" vignette.
#'   6. `all_rows` is not supported in `epix_slide`; since the slide
#'   computations are allowed more flexibility in their outputs than in
#'   `epi_slide`, we can't guess a good representation for missing computations
#'   for excluded group-`ref_time_value` pairs.
#'   7. The `ref_time_values` default for `epix_slide` is based on making an
#'   evenly-spaced sequence out of the `version`s in the `DT` plus the
#'   `versions_end`, rather than the `time_value`s.
#'
#' Apart from the above distinctions, the interfaces between `epix_slide()` and
#' `epi_slide()` are the same.
#'
#' Furthermore, the current function can be considerably slower than
#'   `epi_slide()`, for two reasons: (1) it must repeatedly fetch
#'   properly-versioned snapshots from the data archive (via `epix_as_of()`),
#'   and (2) it performs a "manual" sliding of sorts, and does not benefit from
#'   the highly efficient `slider` package. For this reason, it should never be
#'   used in place of `epi_slide()`, and only used when version-aware sliding is
#'   necessary (as it its purpose).
#'
#' @examples
#' library(dplyr)
#'
#' # Reference time points for which we want to compute slide values:
#' ref_time_values <- seq(as.Date("2020-06-01"),
#'   as.Date("2020-06-15"),
#'   by = "1 day"
#' )
#'
#' # A simple (but not very useful) example (see the archive vignette for a more
#' # realistic one):
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     f = ~ mean(.x$case_rate_7d_av),
#'     before = 2,
#'     ref_time_values = ref_time_values,
#'     new_col_name = "case_rate_7d_av_recent_av"
#'   ) %>%
#'   ungroup()
#' # We requested time windows that started 2 days before the corresponding time
#' # values. The actual number of `time_value`s in each computation depends on
#' # the reporting latency of the signal and `time_value` range covered by the
#' # archive (2020-06-01 -- 2021-11-30 in this example).  In this case, we have
#' # * 0 `time_value`s, for ref time 2020-06-01 --> the result is automatically
#' #                                                discarded
#' # * 1 `time_value`, for ref time 2020-06-02
#' # * 2 `time_value`s, for the rest of the results
#' # * never the 3 `time_value`s we would get from `epi_slide`, since, because
#' #   of data latency, we'll never have an observation
#' #   `time_value == ref_time_value` as of `ref_time_value`.
#' # The example below shows this type of behavior in more detail.
#'
#' # Examining characteristics of the data passed to each computation with
#' # `all_versions=FALSE`.
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     function(x, gk, rtv) {
#'       tibble(
#'         time_range = if (nrow(x) == 0L) {
#'           "0 `time_value`s"
#'         } else {
#'           sprintf("%s -- %s", min(x$time_value), max(x$time_value))
#'         },
#'         n = nrow(x),
#'         class1 = class(x)[[1L]]
#'       )
#'     },
#'     before = 5, all_versions = FALSE,
#'     ref_time_values = ref_time_values, names_sep = NULL
#'   ) %>%
#'   ungroup() %>%
#'   arrange(geo_value, time_value)
#'
#' # --- Advanced: ---
#'
#' # `epix_slide` with `all_versions=FALSE` (the default) applies a
#' # version-unaware computation to several versions of the data. We can also
#' # use `all_versions=TRUE` to apply a version-*aware* computation to several
#' # versions of the data, again looking at characteristics of the data passed
#' # to each computation. In this case, each computation should expect an
#' # `epi_archive` containing the relevant version data:
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     function(x, gk, rtv) {
#'       tibble(
#'         versions_start = if (nrow(x$DT) == 0L) {
#'           "NA (0 rows)"
#'         } else {
#'           toString(min(x$DT$version))
#'         },
#'         versions_end = x$versions_end,
#'         time_range = if (nrow(x$DT) == 0L) {
#'           "0 `time_value`s"
#'         } else {
#'           sprintf("%s -- %s", min(x$DT$time_value), max(x$DT$time_value))
#'         },
#'         n = nrow(x$DT),
#'         class1 = class(x)[[1L]]
#'       )
#'     },
#'     before = 5, all_versions = TRUE,
#'     ref_time_values = ref_time_values, names_sep = NULL
#'   ) %>%
#'   ungroup() %>%
#'   # Focus on one geo_value so we can better see the columns above:
#'   filter(geo_value == "ca") %>%
#'   select(-geo_value)
#'
#' @export
epix_slide <- function(
    x,
    f,
    ...,
    before = Inf,
    ref_time_values = NULL,
    new_col_name = "slide_value",
    as_list_col = FALSE,
    names_sep = "_",
    all_versions = FALSE) {
  UseMethod("epix_slide")
}


#' @rdname epix_slide
#' @export
epix_slide.epi_archive <- function(
    x,
    f,
    ...,
    before = Inf,
    ref_time_values = NULL,
    new_col_name = "slide_value",
    as_list_col = FALSE,
    names_sep = "_",
    all_versions = FALSE) {
  # For an "ungrouped" slide, treat all rows as belonging to one big
  # group (group by 0 vars), like `dplyr::summarize`, and let the
  # resulting `grouped_epi_archive` handle the slide:
  epix_slide(
    group_by(x),
    f,
    ...,
    before = before, ref_time_values = ref_time_values, new_col_name = new_col_name,
    as_list_col = as_list_col, names_sep = names_sep,
    all_versions = all_versions
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
epix_slide_ref_time_values_default <- function(ea) {
  versions_with_updates <- c(ea$DT$version, ea$versions_end)
  ref_time_values <- tidyr::full_seq(versions_with_updates, guess_period(versions_with_updates))
  return(ref_time_values)
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
  if (!identical(class(max_version), class(x$DT$version))) {
    cli_abort("`max_version` must have the same `class` as `epi_archive$DT$version`.")
  }
  if (!identical(typeof(max_version), typeof(x$DT$version))) {
    cli_abort("`max_version` must have the same `typeof` as `epi_archive$DT$version`.")
  }
  assert_scalar(max_version, na.ok = FALSE)
  if (max_version > x$versions_end) {
    cli_abort("`max_version` must be at most `epi_archive$versions_end`.")
  }
  x$DT <- x$DT[x$DT$version <= max_version, colnames(x$DT), with = FALSE]
  # (^ this filter operation seems to always copy the DT, even if it
  # keeps every entry; we don't guarantee this behavior in
  # documentation, though, so we could change to alias in this case)
  if (!is.na(x$clobberable_versions_start) && x$clobberable_versions_start > max_version) {
    x$clobberable_versions_start <- NA
  }
  x$versions_end <- max_version
  return(x)
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
