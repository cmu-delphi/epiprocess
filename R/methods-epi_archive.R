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
#' epix_as_of(
#'   archive_cases_dv_subset,
#'   version = max(archive_cases_dv_subset$DT$version)
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
#'   versions_end = max(archive_cases_dv_subset$DT$version) + 1L
#' )
#'
#' epix_as_of(archive_cases_dv_subset2, max(archive_cases_dv_subset$DT$version))
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

  other_keys <- setdiff(
    key(x$DT),
    c("geo_value", "time_value", "version")
  )

  # Check a few things on version
  if (!identical(class(version), class(x$DT$version))) {
    cli_abort(
      "`version` must have the same `class` vector as `epi_archive$DT$version`."
    )
  }
  assert_scalar(version, na.ok = FALSE)
  if (version > x$versions_end) {
    cli_abort("`version` must be at most `epi_archive$versions_end`.")
  }
  assert_scalar(min_time_value, na.ok = FALSE)
  min_time_value_inf <- is.infinite(min_time_value) && min_time_value < 0
  min_time_value_same_type <- identical(class(min_time_value), class(x$DT$time_value))
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

  # Filter by version and return
  if (all_versions) {
    # epi_archive is copied into result, so we can modify result directly
    result <- epix_truncate_versions_after(x, version)
    if (!min_time_value_inf) {
      # See below for why we need this branch.
      filter_mask <- result$DT$time_value >= min_time_value
      result$DT <- result$DT[filter_mask, ] # nolint: object_usage_linter
    }
    return(result)
  }

  # Make sure to use data.table ways of filtering and selecting
  if (min_time_value_inf) {
    # This branch is needed for `epix_as_of` to work with `yearmonth` time type
    # to avoid time_value > .min_time_value, which is NA for `yearmonth`.
    filter_mask <- x$DT$version <= version
  } else {
    filter_mask <- x$DT$time_value >= min_time_value & x$DT$version <= version
  }
  as_of_epi_df <- x$DT[filter_mask, ] %>%
    unique(by = c("geo_value", "time_value", other_keys), fromLast = TRUE) %>%
    as.data.frame() %>%
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
  validate_version_bound(versions_end, x$DT, na_ok = FALSE)
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
#' @importFrom data.table copy ":="
#' @importFrom rlang arg_match
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
#' s2 <- tibble::tibble(
#'   geo_value = c("ca", "ca"),
#'   time_value = as.Date(c("2024-08-01", "2024-08-02")),
#'   version = as.Date(c("2024-08-02", "2024-08-02")),
#'   signal2 = c(4, 5),
#' )
#' s1 <- s1 %>% as_epi_archive()
#' s2 <- s2 %>% as_epi_archive()
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
#' merged[["DT"]]
#' @importFrom data.table key set setkeyv
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
            `x` and `y` DTs both have measurement columns named
            {format_chr_with_quotes(intersect(x_nonby_colnames, y_nonby_colnames))};
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
    # It'd probably be better to pre-compactify before the merge, and might be
    # guaranteed not to be necessary to compactify the merge result if the
    # inputs are already compactified, but at time of writing we don't have
    # compactify in its own method or field, and it seems like it should be
    # pretty fast anyway.
    compactify = compactify, compactify_abs_tol = compactify_abs_tol,
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
    list(
      archive = out_archive,
      request_names = request_names
    )
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
#' performed on **data that would have been available as of t**. This function
#' is intended for use in accurate backtesting of models; see
#' `vignette("backtesting", package="epipredict")` for a walkthrough.
#'
#' @param .x An [`epi_archive`] or [`grouped_epi_archive`] object. If ungrouped,
#'   all data in `x` will be treated as part of a single data group.
#' @param .f Function, formula, or missing; together with `...` specifies the
#'   computation to slide. To "slide" means to apply a computation over a
#'   sliding (a.k.a. "rolling") time window for each data group. The window is
#'   determined by the `.before` parameter (see details for more). If a
#'   function, `.f` must have the form `function(x, g, t, ...)`, where
#'
#'   - "x" is an epi_df with the same column names as the archive's `DT`, minus
#'     the `version` column
#'   - "g" is a one-row tibble containing the values of the grouping variables
#'   for the associated group
#'   - "t" is the ref_time_value for the current window
#'   - "..." are additional arguments
#'
#'   If a formula, `.f` can operate directly on columns accessed via `.x$var` or
#'   `.$var`, as in `~ mean (.x$var)` to compute a mean of a column `var` for
#'   each group-`ref_time_value` combination. The group key can be accessed via
#'   `.y` or `.group_key`, and the reference time value can be accessed via `.z`
#'   or `.ref_time_value`. If `.f` is missing, then `...` will specify the
#'   computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `.f` is missing, then the `...` is interpreted
#'   as a ["data-masking"][rlang::args_data_masking] expression or expressions
#'   for tidy evaluation; in addition to referring columns directly by name, the
#'   expressions have access to `.data` and `.env` pronouns as in `dplyr` verbs,
#'   and can also refer to `.x` (not the same as the input epi_archive),
#'   `.group_key`, and `.ref_time_value`. See details for more.
#' @param .before How many time values before the `.ref_time_value`
#'   should each snapshot handed to the function `.f` contain? If provided, it
#'   should be a single value that is compatible with the time_type of the
#'   time_value column (more below), but most commonly an integer. This window
#'   endpoint is inclusive. For example, if `.before = 7`, `time_type`
#'   in the archive is "day", and the `.ref_time_value` is January 8, then the
#'   smallest time_value in the snapshot will be January 1. If missing, then the
#'   default is no limit on the time values, so the full snapshot is given.
#' @param .versions Reference time values / versions for sliding
#'   computations; each element of this vector serves both as the anchor point
#'   for the `time_value` window for the computation and the `max_version`
#'   `epix_as_of` which we fetch data in this window. If missing, then this will
#'   set to a regularly-spaced sequence of values set to cover the range of
#'   `version`s in the `DT` plus the `versions_end`; the spacing of values will
#'   be guessed (using the GCD of the skips between values).
#' @param .new_col_name Either `NULL` or a string indicating the name of the new
#'   column that will contain the derived values. The default, `NULL`, will use
#'   the name "slide_value" unless your slide computations output data frames,
#'   in which case they will be unpacked into the constituent columns and those
#'   names used. If the resulting column name(s) overlap with the column names
#'   used for labeling the computations, which are `group_vars(x)` and
#'   `"version"`, then the values for these columns must be identical to the
#'   labels we assign.
#' @param .all_versions (Not the same as `.all_rows` parameter of `epi_slide`.)
#'   If `.all_versions = TRUE`, then the slide computation will be passed the
#'   version history (all `version <= .version` where `.version` is one of the
#'   requested `.versions`) for rows having a `time_value` of at least `.version
#'   - before`. Otherwise, the slide computation will be passed only the most
#'   recent `version` for every unique `time_value`. Default is `FALSE`.
#' @return A tibble whose columns are: the grouping variables (if any),
#'   `time_value`, containing the reference time values for the slide
#'   computation, and a column named according to the `.new_col_name` argument,
#'   containing the slide values. It will be grouped by the grouping variables.
#'
#' @details A few key distinctions between the current function and `epi_slide()`:
#'   1. In `.f` functions for `epix_slide`, one should not assume that the input
#'   data to contain any rows with `time_value` matching the computation's
#'   `.ref_time_value` (accessible via `attributes(<data>)$metadata$as_of`); for
#'   typical epidemiological surveillance data, observations pertaining to a
#'   particular time period (`time_value`) are first reported `as_of` some
#'   instant after that time period has ended.
#'   2. The input class and columns are similar but different: `epix_slide`
#'   (with the default `.all_versions=FALSE`) keeps all columns and the
#'   `epi_df`-ness of the first argument to each computation; `epi_slide` only
#'   provides the grouping variables in the second input, and will convert the
#'   first input into a regular tibble if the grouping variables include the
#'   essential `geo_value` column. (With .all_versions=TRUE`, `epix_slide` will
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
#'   analogous to `dplyr::mutate` followed by `dplyr::arrange`) This is detailed
#'   in the "advanced" vignette.
#'   5. `.all_rows` is not supported in `epix_slide`; since the slide
#'   computations are allowed more flexibility in their outputs than in
#'   `epi_slide`, we can't guess a good representation for missing computations
#'   for excluded group-`.ref_time_value` pairs.
#'   76. The `.versions` default for `epix_slide` is based on making an
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
#' versions <- seq(as.Date("2020-06-02"),
#'   as.Date("2020-06-15"),
#'   by = "1 day"
#' )
#'
#' # A simple (but not very useful) example (see the archive vignette for a more
#' # realistic one):
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     .f = ~ mean(.x$case_rate_7d_av),
#'     .before = 2,
#'     .versions = versions,
#'     .new_col_name = "case_rate_7d_av_recent_av"
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
#' #   `time_value == .ref_time_value` as of `.ref_time_value`.
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
#'     .before = 5, .all_versions = FALSE,
#'     .versions = versions
#'   ) %>%
#'   ungroup() %>%
#'   arrange(geo_value, version)
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
#'     .before = 5, .all_versions = TRUE,
#'     .versions = versions
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
    .all_versions = FALSE) {
  UseMethod("epix_slide")
}


#' @rdname epix_slide
#' @export
epix_slide.epi_archive <- function(
    .x,
    .f,
    ...,
    .before = Inf,
    .versions = NULL,
    .new_col_name = NULL,
    .all_versions = FALSE) {
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
  versions_with_updates <- c(ea$DT$version, ea$versions_end)
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
  if (!identical(class(max_version), class(x$DT$version))) {
    cli_abort("`max_version` must have the same `class` as `epi_archive$DT$version`.")
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
#' * Factor in that `.data$DT` may have been converted into a compact format
#'   based on diffing consecutive versions, and the last version of each
#'   observation in `.data$DT` will always be carried forward to future
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
  in_tbl <- tibble::as_tibble(as.list(.data$DT), .name_repair = "minimal")
  if (.format_aware) {
    out_tbl <- in_tbl %>%
      filter(..., .by = {{ .by }})
  } else {
    measurement_colnames <- setdiff(names(.data$DT), key_colnames(.data))
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
  out_dtbl <- as.data.table(out_tbl, key = c("geo_value", "time_value", out_other_keys, "version"))
  result <- new_epi_archive(
    out_dtbl,
    out_geo_type, out_time_type, out_other_keys,
    # Assume version-related metadata unchanged; part of why we want to push
    # back on filter expressions like `.data$version <= .env$as_of`:
    .data$clobberable_versions_start, .data$versions_end
  )
  # Filtering down rows while keeping all (ukey) columns should preserve ukey
  # uniqueness.
  result
}
