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
#' @export 
#' @examples
#'
#' range(archive_cases_dv_subset$DT$version) # 2020-06-02 -- 2021-12-01
#'
#' epix_as_of(x = archive_cases_dv_subset,
#'            max_version = as.Date("2020-06-12"))
#'
#' # When fetching a snapshot as of the latest version with update data in the
#' # archive, a warning is issued by default, as this update data might not yet
#' # be finalized (for example, if data versions are labeled with dates, these
#' # versions might be overwritten throughout the corresponding days with
#' # additional data or "hotfixes" of erroroneous data; when we build an archive
#' # based on database queries, the latest available update might still be
#' # subject to change, but previous versions should be finalized). We can
#' # muffle such warnings with the following pattern:
#' withCallingHandlers({
#'   epix_as_of(x = archive_cases_dv_subset,
#'              max_version = max(archive_cases_dv_subset$DT$version))
#' }, epiprocess__snapshot_as_of_clobberable_version = function(wrn) invokeRestart("muffleWarning"))
#' # Since R 4.0, there is a `globalCallingHandlers` function that can be used
#' # to globally toggle these warnings.
epix_as_of = function(x, max_version, min_time_value = -Inf, all_versions = FALSE) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$as_of(max_version, min_time_value, all_versions = all_versions))
}

#' `epi_archive` with unobserved history filled in (won't mutate, might alias)
#'
#' Sometimes, due to upstream data pipeline issues, we have to work with a
#' version history that isn't completely up to date, but with functions that
#' expect archives that are completely up to date, or equally as up-to-date as
#' another archive. This function provides one way to approach such mismatches:
#' pretend that we've "observed" additional versions, filling in these versions
#' with NAs or extrapolated values.
#'
#' '`epix_fill_through_version` will not mutate its `x` argument, but its result
#'  might alias fields of `x` (e.g., mutating the result's `DT` might mutate
#'  `x$DT`). The R6 method variant, `x$fill_through_version`, will mutate `x` to
#'  give the result, but might reseat its fields (e.g., references to the old
#'  `x$DT` might not be updated by this function or subsequent operations on
#'  `x`), and returns the updated `x` [invisibly][base::invisible].
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
#' @return An `epi_archive`
epix_fill_through_version = function(x, fill_versions_end,
                                     how=c("na", "locf")) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  # Enclosing parentheses drop the invisibility flag. See description above of
  # potential mutation and aliasing behavior.
  ( x$clone()$fill_through_version(fill_versions_end, how=how) )
}

#' Merge two `epi_archive` objects
#'
#' Merges two `epi_archive`s that share a common `geo_value`, `time_value`, and
#' set of key columns. When they also share a common `versions_end`,
#' using `$as_of` on the result should be the same as using `$as_of` on `x` and
#' `y` individually, then performing a full join of the `DT`s on the non-version
#' key columns (potentially consolidating multiple warnings about clobberable
#' versions). If the `versions_end` values differ, the
#' `sync` parameter controls what is done.
#'
#' This function, [`epix_merge`], does not mutate its inputs and will not alias
#' either archive's `DT`, but may alias other fields; `x$merge` will overwrite
#' `x` with the result of the merge, reseating its `DT` and several other fields
#' (making them point to different objects), but avoiding mutation of the
#' contents of the old `DT` (only relevant if you have another reference to the
#' old `DT` in another object).
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
#'   compactified? See [`as_epi_archive`] for an explanation of what this means.
#'   Default here is `TRUE`.
#' @return the resulting `epi_archive`
#'
#' @details In all cases, `additional_metadata` will be an empty list, and
#'   `clobberable_versions_start` will be set to the earliest version that could
#'   be clobbered in either input archive.
#'
#' @examples
#' # create two example epi_archive datasets
#' x <- archive_cases_dv_subset$DT %>%
#'   dplyr::select(geo_value,time_value,version,case_rate_7d_av) %>%
#'   as_epi_archive(compactify=TRUE)
#' y <- archive_cases_dv_subset$DT %>%
#'   dplyr::select(geo_value,time_value,version,percent_cli) %>%
#'   as_epi_archive(compactify=TRUE)
#' # merge results stored in a third object:
#' xy = epix_merge(x, y)
#' # vs. mutating x to hold the merge result:
#' x$merge(y)
#'
#' @importFrom data.table key set setkeyv
#' @export
epix_merge = function(x, y,
                      sync = c("forbid","na","locf","truncate"),
                      compactify = TRUE) {
  if (!inherits(x, "epi_archive")) {
    Abort("`x` must be of class `epi_archive`.")
  }

  if (!inherits(y, "epi_archive")) {
    Abort("`y` must be of class `epi_archive`.")
  }

  sync <- rlang::arg_match(sync)

  if (!identical(x$geo_type, y$geo_type)) {
    Abort("`x` and `y` must have the same `$geo_type`")
  }

  if (!identical(x$time_type, y$time_type)) {
    Abort("`x` and `y` must have the same `$time_type`")
  }

  if (length(x$additional_metadata) != 0L) {
    Warn("x$additional_metadata won't appear in merge result",
         class = "epiprocess__epix_merge_ignores_additional_metadata")
  }
  if (length(y$additional_metadata) != 0L) {
    Warn("y$additional_metadata won't appear in merge result",
         class = "epiprocess__epix_merge_ignores_additional_metadata")
  }
  result_additional_metadata = list()

  result_clobberable_versions_start =
    if (all(is.na(c(x$clobberable_versions_start, y$clobberable_versions_start)))) {
      NA # (any type of NA is fine here)
    } else {
      Min(c(x$clobberable_versions_start, y$clobberable_versions_start))
    }

  # The actual merge below may not succeed 100% of the time, so do this
  # preprocessing using non-mutating (but potentially aliasing) functions. This
  # approach potentially uses more memory, but won't leave behind a
  # partially-mutated `x` on failure.
  if (sync == "forbid") {
    if (!identical(x$versions_end, y$versions_end)) {
      Abort(paste(
        "`x` and `y` were not equally up to date version-wise:",
        "`x$versions_end` was not identical to `y$versions_end`;",
        "either ensure that `x` and `y` are equally up to date before merging,",
        "or specify how to deal with this using `sync`"
      ), class="epiprocess__epix_merge_unresolved_sync")
    } else {
      new_versions_end = x$versions_end
      x_DT = x$DT
      y_DT = y$DT
    }
  } else if (sync %in% c("na", "locf")) {
    new_versions_end = max(x$versions_end, y$versions_end)
    x_DT = epix_fill_through_version(x, new_versions_end, sync)$DT
    y_DT = epix_fill_through_version(y, new_versions_end, sync)$DT
  } else if (sync == "truncate") {
    new_versions_end = min(x$versions_end, y$versions_end)
    x_DT = x$DT[x[["DT"]][["version"]] <= new_versions_end, names(x$DT), with=FALSE]
    y_DT = y$DT[y[["DT"]][["version"]] <= new_versions_end, names(y$DT), with=FALSE]
  } else Abort("unimplemented")

  # key(x_DT) should be the same as key(x$DT) and key(y_DT) should be the same
  # as key(y$DT). Below, we only use {x,y}_DT in the code (making it easier to
  # split the code into separate functions if we wish), but still refer to
  # {x,y}$DT in the error messages (further relying on this assumption).
  #
  # Check&ensure that the above assumption; if it didn't already hold, we likely
  # have a bug in the preprocessing, a weird/invalid archive as input, and/or a
  # data.table version with different semantics (which may break other parts of
  # our code).
  x_DT_key_as_expected = identical(key(x$DT), key(x_DT))
  y_DT_key_as_expected = identical(key(y$DT), key(y_DT))
  if (!x_DT_key_as_expected || !y_DT_key_as_expected) {
    Warn("
      `epiprocess` internal warning (please report): pre-processing for
      epix_merge unexpectedly resulted in an intermediate data table (or
      tables) with a different key than the corresponding input archive.
      Manually setting intermediate data table keys to the expected values.
    ", internal=TRUE)
    setkeyv(x_DT, key(x$DT))
    setkeyv(y_DT, key(y$DT))
  }
  # Without some sort of annotations of what various columns represent, we can't
  # do something that makes sense when merging archives with mismatched keys.
  # E.g., even if we assume extra keys represent demographic breakdowns, a
  # sensible default treatment of count-type and rate-type value columns would
  # differ.
  if (!identical(sort(key(x_DT)), sort(key(y_DT)))) {
    Abort("
            The archives must have the same set of key column names; if the
            key columns represent the same things, just with different
            names, please retry after manually renaming to match; if they
            represent different things (e.g., x has an age breakdown
            but y does not), please retry after processing them to share
            the same key (e.g., by summarizing x to remove the age breakdown,
            or by applying a static age breakdown to y).
          ", class="epiprocess__epix_merge_x_y_must_have_same_key_set")
  }
  # `by` cols = result (and each input's) `key` cols, and determine
  # the row set, determined using a full join via `merge`
  #
  # non-`by` cols = "value"-ish cols, and are looked up with last
  #                 version carried forward via rolling joins
  by = key(x_DT) # = some perm of key(y_DT)
  if (!all(c("geo_value","time_value","version") %in% key(x_DT))) {
    Abort('Invalid `by`; `by` is currently set to the common `key` of
           the two archives, and is expected to contain
           "geo_value", "time_value", and "version".',
          class="epiprocess__epi_archive_must_have_required_key_cols")
  }
  if (length(by) < 1L || utils::tail(by, 1L) != "version") {
    Abort('Invalid `by`; `by` is currently set to the common `key` of
           the two archives, and is expected to have a "version" as
           the last key col.',
          class="epiprocess__epi_archive_must_have_version_at_end_of_key")
  }
  x_nonby_colnames = setdiff(names(x_DT), by)
  y_nonby_colnames = setdiff(names(y_DT), by)
  if (length(intersect(x_nonby_colnames, y_nonby_colnames)) != 0L) {
    Abort("
            `x` and `y` DTs have overlapping non-by column names;
            this is currently not supported; please manually fix up first:
            any overlapping columns that can are key-like should be
            incorporated into the key, and other columns should be renamed.
          ", class="epiprocess__epix_merge_x_y_must_not_have_overlapping_nonby_colnames")
  }
  x_by_vals = x_DT[, by, with=FALSE]
  if (anyDuplicated(x_by_vals) != 0L) {
    Abort("
            The `by` columns must uniquely determine rows of `x$DT`;
            the `by` is currently set to the common `key` of the two
            archives, so this can be resolved by adding key-like columns
            to `x`'s key (to get a unique key).
          ", class="epiprocess__epix_merge_by_cols_must_act_as_unique_key")
  }
  y_by_vals = y_DT[, by, with=FALSE]
  if (anyDuplicated(y_by_vals) != 0L) {
    Abort("
            The `by` columns must uniquely determine rows of `y$DT`;
            the `by` is currently set to the common `key` of the two
            archives, so this can be resolved by adding key-like columns
            to `y`'s key (to get a unique key).
          ", class="epiprocess__epix_merge_by_cols_must_act_as_unique_key")
  }
  result_DT = merge(x_by_vals, y_by_vals, by=by,
                    # We must have `all=TRUE` or we may skip updates
                    # from x and/or y and corrupt the history
                    all=TRUE,
                    # We don't want Cartesian products, but the
                    # by-is-unique-key check above already ensures
                    # this. (Note that `allow.cartesian=FALSE` doesn't
                    # actually catch all Cartesian products anyway.)
                    # Disable superfluous check:
                    allow.cartesian=TRUE)
  set(result_DT,, x_nonby_colnames,
      x_DT[result_DT[, by, with=FALSE], x_nonby_colnames, with=FALSE,
           # It's good practice to specify `on`, and we must
           # explicitly specify `on` if there's a potential key vs.
           # by order mismatch (not possible currently for x
           # with by = key(x$DT), but possible for y):
           on = by,
           # last version carried forward:
           roll=TRUE,
           # requesting non-version key that doesn't exist in the other archive,
           # or before its first version, should result in NA
           nomatch=NA,
           # see note on `allow.cartesian` above; currently have a
           # similar story here.
           allow.cartesian=TRUE])
  set(result_DT,,  y_nonby_colnames,
      y_DT[result_DT[, by, with=FALSE], y_nonby_colnames, with=FALSE,
           on = by,
           roll=TRUE,
           nomatch=NA,
           allow.cartesian=TRUE])
  # The key could be unset in case of a key vs. by order mismatch as
  # noted above. Ensure that we keep it:
  setkeyv(result_DT, by)

  return (as_epi_archive(
    result_DT[], # clear data.table internal invisibility flag if set
    geo_type = x$geo_type,
    time_type = x$time_type,
    other_keys = setdiff(key(result_DT), c("geo_value","time_value","version")),
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
new_col_modify_recorder_df = function(parent_df) {
  if (!inherits(parent_df, "data.frame")) {
    Abort('`parent_df` must inherit class `"data.frame"`',
          internal=TRUE)
  }
  `class<-`(parent_df, c("col_modify_recorder_df", class(parent_df)))
}

#' Extract unchanged parent-class data frame from a `new_col_modify_recorder_df`
#'
#' @param col_modify_recorder_df an instance of a `col_modify_recorder_df`
#' @return named list with elements `unchanged_parent_df`, `cols`; `cols` is the
#'   input to [`dplyr::dplyr_col_modify`] that this class was designed to record
#'
#' @noRd
destructure_col_modify_recorder_df = function(col_modify_recorder_df) {
  if (!inherits(col_modify_recorder_df, "col_modify_recorder_df")) {
    Abort('`col_modify_recorder_df` must inherit class `"col_modify_recorder_df"`',
          internal=TRUE)
  }
  list(
    unchanged_parent_df = col_modify_recorder_df %>%
      `attr<-`("epiprocess::col_modify_recorder_df::cols", NULL) %>%
      `class<-`(setdiff(class(.), "col_modify_recorder_df")),
    cols = attr(col_modify_recorder_df,
                "epiprocess::col_modify_recorder_df::cols", exact=TRUE)
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
dplyr_col_modify.col_modify_recorder_df = function(data, cols) {
  if (!is.null(attr(data, "epiprocess::col_modify_recorder_df::cols", exact=TRUE))) {
    Abort("`col_modify_recorder_df` can only record `cols` once",
          internal=TRUE)
  }
  attr(data, "epiprocess::col_modify_recorder_df::cols") <- cols
  data
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
epix_detailed_restricted_mutate = function(.data, ...) {
  # We don't want to directly use `dplyr::mutate` on the `$DT`, as:
  # - this likely copies the entire table
  # - `mutate` behavior, including the output class, changes depending on
  #   whether `dtplyr` is loaded and would require post-processing
  # - behavior with `dtplyr` isn't fully compatible
  # - it doesn't give the desired details, and `rlang::exprs_auto_name` does not
  #   appropriately handle the `= NULL` and `= <data.frame>` tidyeval cases
  # Instead:
  # - Use `as.list` to get a shallow copy (undocumented, but apparently
  #   intended, behavior), then `as_tibble` (also shallow, given a list) to get
  #   back to something that will use `dplyr`'s included `mutate` method(s),
  #   then convert this using shallow operations into a `data.table`.
  # - Use `col_modify_recorder_df` to get the desired details.
  in_tbl = tibble::as_tibble(as.list(.data$DT), .name_repair="minimal")
  col_modify_cols =
    destructure_col_modify_recorder_df(
      mutate(new_col_modify_recorder_df(in_tbl), ...)
    )[["cols"]]
  invalidated_key_col_is =
    which(purrr::map_lgl(key(.data$DT), function(key_colname) {
      key_colname %in% names(col_modify_cols) &&
        !rlang::is_reference(in_tbl[[key_colname]], col_modify_cols[[key_colname]])
    }))
  if (length(invalidated_key_col_is) != 0L) {
    rlang::abort(paste_lines(c(
      "Key columns must not be replaced or removed.",
      wrap_varnames(key(.data$DT)[invalidated_key_col_is],
                    initial="Flagged key cols: ")
    )))
  } else {
    # Have `dplyr` do the `dplyr_col_modify`, keeping the column-level-aliasing
    # and must-copy-on-write-if-refcount-more-than-1 model, obtaining a tibble,
    # then `setDT`-ing it in place to be a `data.table`. The key should still be
    # valid (assuming that the user did not explicitly alter `key(.data$DT)` or
    # the columns by reference somehow within `...` tidyeval-style computations,
    # or trigger refcount-1 alterations due to still having >1 refcounts on the
    # columns), so in between, set the "sorted" attribute accordingly to prevent
    # attempted sorting (including potential extra copies) or sortedness
    # checking, then `setDT`.
    out_DT = dplyr::dplyr_col_modify(in_tbl, col_modify_cols) # tibble
    data.table::setattr(out_DT, "sorted", data.table::key(.data$DT))
    data.table::setDT(out_DT, key=key(.data$DT))
    out_archive = .data$clone()
    out_archive$DT <- out_DT
    request_names = names(col_modify_cols)
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

#' `group_by` and related methods for `epi_archive`, `grouped_epi_archive`
#'
#' @param .data An `epi_archive` or `grouped_epi_archive`
#' @param ... Similar to [`dplyr::group_by`] (see "Details:" for edge cases);
#' * For `group_by`: unquoted variable name(s) or other
#'   ["data masking"][dplyr::dplyr_data_masking] expression(s). It's possible to
#'   use [`dplyr::mutate`]-like syntax here to calculate new columns on which to
#'   perform grouping, but note that, if you are regrouping an already-grouped
#'   `.data` object, the calculations will be carried out ignoring such grouping
#'   (same as [in dplyr][dplyr::group_by]).
#' * For `ungroup`: either
#'   * empty, in order to remove the grouping and output an `epi_archive`; or
#'   * variable name(s) or other ["tidy-select"][dplyr::dplyr_tidy_select]
#'     expression(s), in order to remove the matching variables from the list of
#'     grouping variables, and output another `grouped_epi_archive`.
#' @param .add Boolean. If `FALSE`, the default, the output will be grouped by
#'   the variable selection from `...` only; if `TRUE`, the output will be
#'   grouped by the current grouping variables plus the variable selection from
#'   `...`.
#' @param .drop As described in [`dplyr::group_by`]; determines treatment of factor
#'   columns.
#' @param x For `groups` or `ungroup`: a `grouped_epi_archive`; for
#'   `is_grouped_epi_archive`: any object
#' @param .tbl (For `group_by_drop_default`:) an `epi_archive` or
#'   `grouped_epi_archive` (`epi_archive` dispatches to the S3 default method;
#'   `grouped_epi_archive` dispatches its own S3 method)
#'
#' @details
#'
#' To match `dplyr`, `group_by` allows "data masking" (also referred to as
#' "tidy evaluation") expressions `...`, not just column names, in a way similar
#' to `mutate`. Note that replacing or removing key columns with these
#' expressions is disabled.
#'
#' `archive %>% group_by()` and other expressions that group or regroup by zero
#' columns (indicating that all rows should be treated as part of one large
#' group) will output a `grouped_epi_archive`, in order to enable the use of
#' `grouped_epi_archive` methods on the result. This is in slight contrast to
#' the same operations on tibbles and grouped tibbles, which will *not* output a
#' `grouped_df` in these circumstances.
#'
#' Using `group_by` with `.add=FALSE` to override the existing grouping is
#' disabled; instead, `ungroup` first then `group_by`.
#'
#' Mutation and aliasing: `group_by` tries to use a shallow copy of the `DT`,
#' introducing column-level aliasing between its input and its result. This
#' doesn't follow the general model for most `data.table` operations, which
#' seems to be that, given an nonaliased (i.e., unique) pointer to a
#' `data.table` object, its pointers to its columns should also be nonaliased.
#' If you mutate any of the columns of either the input or result, first ensure
#' that it is fine if columns of the other are also mutated, but do not rely on
#' such behavior to occur. Additionally, never perform mutation on the key
#' columns at all (except for strictly increasing transformations), as this will
#' invalidate sortedness assumptions about the rows.
#'
#' `group_by_drop_default` on (ungrouped) `epi_archive`s is expected to dispatch
#' to `group_by_drop_default.default` (but there is a dedicated method for
#' `grouped_epi_archive`s).
#'
#' @examples
#'
#' grouped_archive = archive_cases_dv_subset %>% group_by(geo_value)
#'
#' # `print` for metadata and method listing:
#' grouped_archive %>% print()
#'
#' # The primary use for grouping is to perform a grouped `epix_slide`:
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(f = ~ mean(.x$case_rate_7d_av),
#'              before = 2,
#'              ref_time_values = as.Date("2020-06-11") + 0:2,
#'              new_col_name = 'case_rate_3d_av') %>%
#'   ungroup()
#'
#' # -----------------------------------------------------------------
#'
#' # Advanced: some other features of dplyr grouping are implemented:
#'
#' library(dplyr)
#' toy_archive =
#'   tribble(
#'     ~geo_value,  ~age_group,  ~time_value,     ~version, ~value,
#'           "us",     "adult", "2000-01-01", "2000-01-02",    121,
#'           "us", "pediatric", "2000-01-02", "2000-01-03",      5, # (addition)
#'           "us",     "adult", "2000-01-01", "2000-01-03",    125, # (revision)
#'           "us",     "adult", "2000-01-02", "2000-01-03",    130  # (addition)
#'   ) %>%
#'   mutate(age_group = ordered(age_group, c("pediatric", "adult")),
#'          time_value = as.Date(time_value),
#'          version = as.Date(version)) %>%
#'   as_epi_archive(other_keys = "age_group")
#'
#' # The following are equivalent:
#' toy_archive %>% group_by(geo_value, age_group)
#' toy_archive %>% group_by(geo_value) %>% group_by(age_group, .add=TRUE)
#' grouping_cols = c("geo_value", "age_group")
#' toy_archive %>% group_by(across(all_of(grouping_cols)))
#'
#' # And these are equivalent:
#' toy_archive %>% group_by(geo_value)
#' toy_archive %>% group_by(geo_value, age_group) %>% ungroup(age_group)
#'
#' # To get the grouping variable names as a `list` of `name`s (a.k.a. symbols):
#' toy_archive %>% group_by(geo_value) %>% groups()
#'
#' # `.drop = FALSE` is supported in a sense; `f` is called on 0-row inputs for
#' # the missing groups identified by `dplyr`, but the row-recycling rules will
#' # exclude the corresponding outputs of `f` from the output of the slide:
#' all.equal(
#'   toy_archive %>%
#'     group_by(geo_value, age_group, .drop=FALSE) %>%
#'     epix_slide(f = ~ sum(.x$value), before = 20) %>%
#'     ungroup(),
#'   toy_archive %>%
#'     group_by(geo_value, age_group, .drop=TRUE) %>%
#'     epix_slide(f = ~ sum(.x$value), before = 20) %>%
#'     ungroup()
#' )
#'
#' @importFrom dplyr group_by
#' @export
#'
#' @aliases grouped_epi_archive
group_by.epi_archive = function(.data, ..., .add=FALSE, .drop=dplyr::group_by_drop_default(.data)) {
  # `add` makes no difference; this is an ungrouped `epi_archive`.
  detailed_mutate = epix_detailed_restricted_mutate(.data, ...)
  grouped_epi_archive$new(detailed_mutate[["archive"]],
                          detailed_mutate[["request_names"]],
                          drop = .drop)
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
#'   explanation. If a function, `f` must take `x`, an `epi_df` with the same
#'   column names as the archive's `DT`, minus the `version` column; followed by
#'   `g`, a one-row tibble containing the values of the grouping variables for
#'   the associated group; followed by any number of named arguments. If a
#'   formula, `f` can operate directly on columns accessed via `.x$var`, as in
#'   `~ mean(.x$var)` to compute a mean of a column `var` for each
#'   `ref_time_value`-group combination. If `f` is missing, then `...` will
#'   specify the computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then `...` is interpreted as an
#'   expression for tidy evaluation. See details of [`epi_slide`].
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
#'   `as_of` which we fetch data in this window. If missing, then this will set
#'   to a regularly-spaced sequence of values set to cover the range of
#'   `version`s in the `DT` plus the `versions_end`; the spacing of values will
#'   be guessed (using the GCD of the skips between values).
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a positive integer and return
#'   an object of class `lubridate::period`. For example, we can use `time_step
#'   = lubridate::hours` in order to set the time step to be one hour (this
#'   would only be meaningful if `time_value` is of class `POSIXct`).
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col If the computations return data frames, should the slide
#'   result hold these in a single list column or try to unnest them? Default is
#'   `FALSE`, in which case a list object returned by `f` would be unnested
#'   (using [`tidyr::unnest()`]), and the names of the resulting columns are given
#'   by prepending `new_col_name` to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_rows If `all_rows = TRUE`, then the output will have one row per
#'   combination of grouping variables and unique time values in the underlying
#'   data table. Otherwise, there will be one row in the output for each time
#'   value in `x` that acts as a reference time value. Default is `FALSE`.
#' @param all_versions If `all_versions = TRUE`, then `f` will be passed the
#'   version history (all `version <= ref_time_value`) for rows having
#'   `time_value` between `ref_time_value - before` and `ref_time_value`.
#'   Otherwise, `f` will be passed only the most recent `version` for every
#'   unique `time_value`. Default is `FALSE`.
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
#'   keeps all columns and the `epi_df`-ness of the first input to the
#'   computation; `epi_slide` only provides the grouping variables in the second
#'   input, and will convert the first input into a regular tibble if the
#'   grouping variables include the essential `geo_value` column.
#'   4. The output class and columns are similar but different: `epix_slide()`
#'   returns a tibble containing only the grouping variables, `time_value`, and
#'   the new column(s) from the slide computation `f`, whereas `epi_slide()`
#'   returns an `epi_df` with all original variables plus the new columns from
#'   the slide computation.
#'   5. Unless grouping by `geo_value` and all `other_keys`, there will be
#'   row-recyling behavior meant to resemble `epi_slide`'s results, based on the
#'   distinct combinations of `geo_value`, `time_value`, and all `other_keys`
#'   present in the version data with `time_value` matching one of the
#'   `ref_time_values`. However, due to reporting latency or reporting dropping
#'   in and out, this may not exactly match the behavior of "corresponding"
#'   `epi_df`s.
#'   6. Similar to the row recyling, while `all_rows=TRUE` is designed to mimic
#'   `epi_slide` by completing based on distinct combinations of `geo_value`,
#'   `time_value`, and all `other_keys` present in the version data with
#'   `time_value` matching one of the `ref_time_values`, this can have unexpected
#'   behaviors due reporting latency or reporting dropping in and out.
#'   7. The `ref_time_values` default for `epix_slide` is based on making an
#'   evenly-spaced sequence out of the `version`s in the `DT` plus the
#'   `versions_end`, rather than the `time_value`s.
#' Apart from this, the interfaces between `epix_slide()` and `epi_slide()` are
#' the same.
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
#'   `epi_archive` and `grouped_epi_archive` classes, so if `x` is an
#'   object of either of these classes, then:
#'   ```
#'   epix_slide(x, new_var = comp(old_var), before = 119)
#'   ```
#'   is equivalent to:
#'   ```
#'   x$slide(new_var = comp(old_var), before = 119)
#'   ```
#'
#' @examples
#' library(dplyr)
#'
#' # Reference time points for which we want to compute slide values:
#' ref_time_values <- seq(as.Date("2020-06-01"),
#'                        as.Date("2020-06-15"),
#'                        by = "1 day")
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(f = ~ mean(.x$case_rate_7d_av),
#'              before = 2,
#'              ref_time_values = ref_time_values,
#'              new_col_name = 'case_rate_7d_av_recent_av') %>%
#'   ungroup()
#' # We requested time windows that started 2 days before the corresponding time
#' # values. The actual number of `time_value`s in each computation depends on
#' # the reporting latency of the signal and `time_value` range covered by the
#' # archive (2020-06-01 -- 2021-11-30 in this example).  In this case, we have
#' # 0 `time_value`s, for ref time 2020-06-01 --> the result is automatically discarded
#' # 1 `time_value`, for ref time 2020-06-02
#' # 2 `time_value`s, for the rest of the results
#' # never 3 `time_value`s, due to data latency
#'
#'
#'
#' # --- Advanced: ---
#'
#' # `epix_slide` with `all_versions=FALSE` (the default) applies a
#' # version-unaware computation to several versions of the data. We can also
#' # use `all_versions=TRUE` to apply a version-*aware* computation to several
#' # versions of the data. In this case, each computation should expect an
#' # `epi_archive` containing the relevant version data:
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     function(x, g) {
#'       tibble(
#'         versions_end = max(x$versions_end),
#'         time_range = if(nrow(x$DT) == 0L) {
#'           "0 `time_value`s"
#'         } else {
#'           sprintf("%s -- %s", min(x$DT$time_value), max(x$DT$time_value))
#'         },
#'         class1 = class(x)[[1L]]
#'       )
#'     },
#'     before = 2, all_versions = TRUE,
#'     ref_time_values = ref_time_values, names_sep=NULL) %>%
#'   ungroup() %>%
#'   arrange(geo_value, time_value)
#'
#' @importFrom rlang enquo !!!
#' @export
epix_slide = function(x, f, ..., before, ref_time_values,
                      time_step, new_col_name = "slide_value",
                      as_list_col = FALSE, names_sep = "_",
                      all_rows = FALSE, all_versions = FALSE) {
  if (!is_epi_archive(x, grouped_okay=TRUE)) {
    Abort("`x` must be of class `epi_archive` or `grouped_epi_archive`.")
  }
  return(x$slide(f, ..., before = before,
                 ref_time_values = ref_time_values,
                 time_step = time_step,
                 new_col_name = new_col_name,
                 as_list_col = as_list_col,
                 names_sep = names_sep,
                 all_rows = all_rows,
                 all_versions = all_versions
                 ))
}

#' Filter an `epi_archive` object to keep only older versions
#'
#' Generates a filtered `epi_archive` from an `epi_archive` object, keeping
#' only rows with `version` falling on or before a specified date.
#'
#' @param x An `epi_archive` object
#' @param max_version Time value specifying the max version to permit in the
#'   filtered archive. That is, the output archive will comprise rows of the
#'   current archive data having `version` less than or equal to the
#'   specified `max_version`
#' @return An `epi_archive` object
#'
#' @export
epix_truncate_versions_after = function(x, max_version) {
  UseMethod("epix_truncate_versions_after")
}

#' @export
epix_truncate_versions_after.epi_archive = function(x, max_version) {
  return ((x$clone()$truncate_versions_after(max_version)))
  # ^ second set of parens drops invisibility
}
