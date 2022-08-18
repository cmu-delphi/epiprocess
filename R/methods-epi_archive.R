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
epix_as_of = function(x, max_version, min_time_value = -Inf) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$as_of(max_version, min_time_value))
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
#' @param fill_versions_end Length-1, same class&type as `%s$version`: the
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
#' @importFrom data.table key set
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
    x_DT = x$DT[x[["DT"]][["version"]] <= new_versions_end, with=FALSE]
    y_DT = y$DT[y[["DT"]][["version"]] <= new_versions_end, with=FALSE]
  } else Abort("unimplemented")

  if (!identical(key(x$DT), key(x_DT)) || !identical(key(y$DT), key(y_DT))) {
    Abort("preprocessing of data tables in merge changed the key unexpectedly",
          internal=TRUE)
  }
  ## key(x_DT) should be the same as key(x$DT) and key(y_DT) should be the same
  ## as key(y$DT). If we want to break this function into parts it makes sense
  ## to use {x,y}_DT below, but this makes the error checks and messages look a
  ## little weird and rely on the key-matching assumption above.
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

#' Slide a function over variables in an `epi_archive` object
#'
#' Slides a given function over variables in an `epi_archive` object. This
#' behaves similarly to `epi_slide()`, with the key exception that it is
#' version-aware: the sliding computation at any given reference time t is
#' performed on **data that would have been available as of t**. See the
#' [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @param x An `epi_archive` object.
#' @param f Function or formula to slide over variables in `x`. To "slide" means
#'   to apply a function or formula over a running window of `n` time steps
#'   (where one time step is typically one day or one week). If a function, `f`
#'   must take `x`, a data frame with the same column names as the original
#'   object; followed by any number of named arguments; and ending with
#'   `...`. If a formula, `f` can operate directly on columns accessed via
#'   `.x$var`, as in `~ mean(.x$var)` to compute a mean of a column `var` over a
#'   sliding window of `n` time steps.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `f`. Alternatively, if `f` is missing, then the current argument is
#'   interpreted as an expression for tidy evaluation.
#' @param before Number of time steps to use in the running window.
#' For example, if
#'   `before = 7`, and one time step is one day, then to produce a
#'   value on January 7
#'   we apply the given function or formula to data in between January 1 and
#'   7.
#' @param group_by The variable(s) to group by before slide computation. If
#'   missing, then the keys in the underlying data table, excluding `time_value`
#'   and `version`, will be used for grouping. To omit a grouping entirely, use
#'   `group_by = NULL`.
#' @param ref_time_values Time values for sliding computations, meaning, each
#'   element of this vector serves as the reference time point for one sliding
#'   window. If missing, then this will be set to all unique time values in the
#'   underlying data table, by default.
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   `time_value` column. This function must take a positive integer and return
#'   an object of class `lubridate::period`. For example, we can use `time_step
#'   = lubridate::hours` in order to set the time step to be one hour (this
#'   would only be meaningful if `time_value` is of class `POSIXct`).
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param as_list_col Should the new column be stored as a list column? Default
#'   is `FALSE`, in which case a list object returned by `f` would be unnested
#'   (using `tidyr::unnest()`), and the names of the resulting columns are given
#'   by prepending `new_col_name` to the names of the list elements.
#' @param names_sep String specifying the separator to use in `tidyr::unnest()`
#'   when `as_list_col = FALSE`. Default is "_". Using `NULL` drops the prefix
#'   from `new_col_name` entirely.
#' @param all_rows If `all_rows = TRUE`, then the output will have one row per
#'   combination of grouping variables and unique time values in the underlying
#'   data table. Otherwise, there will be one row in the output for each time
#'   value in `x` that acts as a reference time value. Default is `FALSE`.
#' @return A tibble whose columns are: the grouping variables, `time_value`,
#'   containing the reference time values for the slide computation, and a
#'   column named according to the `new_col_name` argument, containing the slide
#'   values.
#'
#' @details Two key distinctions between inputs to the current function and
#'   `epi_slide()`:
#'   1. `epix_slide()` uses windows that are **always right-aligned** (in
#'   `epi_slide()`, custom alignments could be specified using the `align` or
#'   `before` arguments).
#'   2. `epix_slide()` uses a `group_by` to specify the grouping upfront (in
#'   `epi_slide()`, this would be accomplished by a preceding function call to
#'   `dplyr::group_by()`).
#' Apart from this, the interfaces between `epix_slide()` and `epi_slide()` are
#'   the same.
#'
#' Note that the outputs are a similar but different: `epix_slide()` only
#'   returns the grouping variables, `time_value`, and the new columns from
#'   sliding, whereas `epi_slide()` returns all original variables plus the new
#'   columns from sliding.
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
#'   `epi_archive` class, so if `x` is an `epi_archive` object, then:
#'   ```
#'   epix_slide(x, new_var = comp(old_var), before = 120)
#'   ```
#'   is equivalent to:
#'   ```
#'   x$slide(new_var = comp(old_var), before = 120)
#'   ```
#'
#' @importFrom rlang enquo
#' @export
#' @examples
#' # these dates are reference time points for the 3 day average sliding window
#' # The resulting epi_archive ends up including data averaged from:
#' # 0 day which has no results, for 2020-06-01
#' # 1 day, for 2020-06-02
#' # 2 days, for the rest of the results
#' # never 3 days due to data latency
#' 
#' versions <- seq(as.Date("2020-06-01"),
#'                       as.Date("2020-06-15"),
#'                       by = "1 day")
#' epix_slide(x = archive_cases_dv_subset,
#'            f = ~ mean(.x$case_rate_7d_av),
#'            before = 3,
#'            group_by = geo_value,
#'            ref_time_values = versions,
#'            new_col_name = 'case_rate_3d_av')
epix_slide = function(x, f, ..., before, group_by, ref_time_values,
                      time_step, new_col_name = "devslide_value",
                      as_list_col = FALSE, names_sep = "_", all_rows = FALSE) {
  if (!inherits(x, "epi_archive")) Abort("`x` must be of class `epi_archive`.")
  return(x$slide(f, ...,
                 before = before,
                 group_by = {{group_by}},
                 ref_time_values = ref_time_values,
                 time_step = time_step,
                 new_col_name = new_col_name,
                 as_list_col = as_list_col,
                 names_sep = names_sep,
                 all_rows = all_rows))
}


