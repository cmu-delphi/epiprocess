# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable_aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable_aware <- TRUE

#' Validate a version bound arg
#'
#' Expected to be used on `clobberable_versions_start`, `versions_end`,
#' and similar arguments. Some additional context-specific checks may be needed.
#'
#' @param version_bound the version bound to validate
#' @param x a data frame containing a version column with which to check
#'   compatibility
#' @param na_ok Boolean; is `NA` an acceptable "bound"? (If so, `NA` will
#'   have a special context-dependent meaning.)
#' @param version_bound_arg optional string; what to call the version bound in
#'   error messages
#'
#' @section Side effects: raises an error if version bound appears invalid
#'
#' @noRd
validate_version_bound <- function(version_bound, x, na_ok = FALSE,
                                   version_bound_arg = rlang::caller_arg(version_bound),
                                   x_arg = rlang::caller_arg(version_bound)) {
  if (is.null(version_bound)) {
    cli_abort(
      "{version_bound_arg} cannot be NULL"
    )
  }
  if (na_ok && is.na(version_bound)) {
    return(invisible(NULL))
  }
  if (!test_set_equal(class(version_bound), class(x[["version"]]))) {
    cli_abort(
      "{version_bound_arg} must have the same classes as x$version,
        which is {class(x$version)}",
    )
  }
  if (!test_set_equal(typeof(version_bound), typeof(x[["version"]]))) {
    cli_abort(
      "{version_bound_arg} must have the same types as x$version,
        which is {typeof(x$version)}",
    )
  }

  return(invisible(NULL))
}

#' `max(x$version)`, with error if `x` has 0 rows
#'
#' Exported to make defaults more easily copyable.
#'
#' @param x `x` argument of [`as_epi_archive`]
#'
#' @return `max(x$version)` if it has any rows; raises error if it has 0 rows or
#'   an `NA` version value
#'
#' @export
max_version_with_row_in <- function(x) {
  if (nrow(x) == 0L) {
    cli_abort(
      "`nrow(x)==0L`, representing a data set history with no row up through the
       latest observed version, but we don't have a sensible guess at what version
       that is, or whether any of the empty versions might be clobbered in the
       future; if we use `x` to form an `epi_archive`, then
       `clobberable_versions_start` and `versions_end` must be manually specified.",
      class = "epiprocess__max_version_cannot_be_used"
    )
  } else {
    version_col <- purrr::pluck(x, "version") # error not NULL if doesn't exist
    if (anyNA(version_col)) {
      cli_abort("version values cannot be NA",
        class = "epiprocess__version_values_must_not_be_na"
      )
    } else {
      version_bound <- max(version_col)
    }
  }
  version_bound
}

#' Get the next possible value greater than `x` of the same type
#'
#' @param x the starting "value"(s)
#' @return same class, typeof, and length as `x`
#'
#' @export
next_after <- function(x) UseMethod("next_after")

#' @export
next_after.integer <- function(x) x + 1L

#' @export
next_after.Date <- function(x) x + 1L

#' Epi Archive
#' @title `epi_archive` object
#'
#' @description An `epi_archive` is an R6 class which contains a data table
#'   along with several relevant pieces of metadata. The data table can be seen
#'   as the full archive (version history) for some signal variables of
#'   interest.
#'
#' @details An `epi_archive` is an R6 class which contains a data table `DT`, of
#'   class `data.table` from the `data.table` package, with (at least) the
#'   following columns:
#'
#' * `geo_value`: the geographic value associated with each row of measurements.
#' * `time_value`: the time value associated with each row of measurements.
#' * `version`: the time value specifying the version for each row of
#'   measurements. For example, if in a given row the `version` is January 15,
#'   2022 and `time_value` is January 14, 2022, then this row contains the
#'   measurements of the data for January 14, 2022 that were available one day
#'   later.
#'
#' The data table `DT` has key variables `geo_value`, `time_value`, `version`,
#'   as well as any others (these can be specified when instantiating the
#'   `epi_archive` object via the `other_keys` argument, and/or set by operating
#'   on `DT` directly). Refer to the documentation for [as_epi_archive()] for
#'   information and examples of relevant parameter names for an `epi_archive` object.
#'   Note that there can only be a single row per unique combination of
#'   key variables, and thus the key variables are critical for figuring out how
#'   to generate a snapshot of data from the archive, as of a given version.
#'
#' In general, the last version of each observation is carried forward (LOCF) to
#'   fill in data between recorded versions, and between the last recorded
#'   update and the `versions_end`. One consequence is that the `DT`
#'   doesn't have to contain a full snapshot of every version (although this
#'   generally works), but can instead contain only the rows that are new or
#'   changed from the previous version (see `compactify`, which does this
#'   automatically). Currently, deletions must be represented as revising a row
#'   to a special state (e.g., making the entries `NA` or including a special
#'   column that flags the data as removed and performing some kind of
#'   post-processing), and the archive is unaware of what this state is. Note
#'   that `NA`s *can* be introduced by `epi_archive` methods for other reasons,
#'   e.g., in [`epix_fill_through_version`] and [`epix_merge`], if requested, to
#'   represent potential update data that we do not yet have access to; or in
#'   [`epix_merge`] to represent the "value" of an observation before the
#'   version in which it was first released, or if no version of that
#'   observation appears in the archive data at allies
#'
#' @section Metadata:
#' The following pieces of metadata are included as fields in an `epi_archive`
#'   object:
#'
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `additional_metadata`: list of additional metadata for the data archive.
#'
#' Unlike an `epi_df` object, metadata for an `epi_archive` object `x` can be
#'   accessed (and altered) directly, as in `x$geo_type` or `x$time_type`,
#'   etc. Like an `epi_df` object, the `geo_type` and `time_type` fields in the
#'   metadata of an `epi_archive` object are not currently used by any
#'   downstream functions in the `epiprocess` package, and serve only as useful
#'   bits of information to convey about the data set at hand.
#'
#' @section Generating Snapshots:
#' An `epi_archive` object can be used to generate a snapshot of the data in
#'   `epi_df` format, which represents the most up-to-date values of the signal
#'   variables, as of the specified version. This is accomplished by calling the
#'   `as_of()` method for an `epi_archive` object `x`. More details on this
#'   method are documented in the wrapper function [`epix_as_of()`].
#'
#' @section Sliding Computations:
#' We can run a sliding computation over an `epi_archive` object, much like
#'   `epi_slide()` does for an `epi_df` object. This is accomplished by calling
#'   the `slide()` method for an `epi_archive` object, which works similarly to
#'   the way `epi_slide()` works for an `epi_df` object, but with one key
#'   difference: it is version-aware. That is, for an `epi_archive` object, the
#'   sliding computation at any given reference time point t is performed on
#'   **data that would have been available as of t**. More details on `slide()`
#'   are documented in the wrapper function [`epix_slide()`].
#'
#' @export
#' @examples
#' tib <- tibble::tibble(
#'   geo_value = rep(c("ca", "hi"), each = 5),
#'   time_value = rep(seq(as.Date("2020-01-01"),
#'     by = 1, length.out = 5
#'   ), times = 2),
#'   version = rep(seq(as.Date("2020-01-02"),
#'     by = 1, length.out = 5
#'   ), times = 2),
#'   value = rnorm(10, mean = 2, sd = 1)
#' )
#'
#' toy_epi_archive <- tib %>% new_epi_archive(
#'   geo_type = "state",
#'   time_type = "day"
#' )
#' toy_epi_archive
#' @name epi_archive
epi_archive <- function(...) {
  new_epi_archive(...)
}

#' New epi archive
#' @description Creates a new `epi_archive` object.
#' @param x A data.frame, data.table, or tibble, with columns `geo_value`,
#'   `time_value`, `version`, and then any additional number of columns.
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `geo_type` and `time_type`
#'   fields; named entries from the passed list or will be included as well.
#' @param compactify Optional; Boolean or `NULL`: should we remove rows that are
#'   considered redundant for the purposes of `epi_archive`'s built-in methods
#'   such as `as_of`? As these methods use the last version of each observation
#'   carried forward (LOCF) to interpolate between the version data provided,
#'   rows that don't change these LOCF results can potentially be omitted to
#'   save space while maintaining the same behavior (with the help of the
#'   `clobberable_versions_start` and `versions_end` fields in some edge cases).
#'   `TRUE` will remove these rows, `FALSE` will not, and missing or `NULL` will
#'   remove these rows and issue a warning. Generally, this can be set to
#'   `TRUE`, but if you directly inspect or edit the fields of the `epi_archive`
#'   such as its `DT`, or rely on redundant updates to achieve a certain
#'   behavior of the `ref_time_values` default in `epix_slide`, you will have to
#'   determine whether `compactify=TRUE` will produce the desired results. If
#'   compactification here is removing a large proportion of the rows, this may
#'   indicate a potential for space, time, or bandwidth savings upstream the
#'   data pipeline, e.g., by avoiding fetching, storing, or processing these
#'   rows of `x`.
#' @param clobberable_versions_start Optional; as in [`as_epi_archive`]
#' @param versions_end Optional; as in [`as_epi_archive`]
#' @return An `epi_archive` object.
#' @importFrom data.table as.data.table key setkeyv
#' @importFrom dplyr if_any if_all everything
#'
#' @details
#' Refer to the documentation for [as_epi_archive()] for more information
#' and examples of parameter names.
#' @name epi_archive
#' @export
new_epi_archive <- function(
    x,
    geo_type = NULL,
    time_type = NULL,
    other_keys = NULL,
    additional_metadata = NULL,
    compactify = NULL,
    clobberable_versions_start = NA,
    versions_end = NULL) {
  assert_data_frame(x)
  if (!test_subset(c("geo_value", "time_value", "version"), names(x))) {
    cli_abort(
      "Columns `geo_value`, `time_value`, and `version` must be present in `x`."
    )
  }
  if (anyMissing(x$version)) {
    cli_abort("Column `version` must not contain missing values.")
  }

  # If geo type is missing, then try to guess it
  if (missing(geo_type) || is.null(geo_type)) {
    geo_type <- guess_geo_type(x$geo_value)
  }

  # If time type is missing, then try to guess it
  if (missing(time_type) || is.null(time_type)) {
    time_type <- guess_time_type(x$time_value)
  }

  # Finish off with small checks on keys variables and metadata
  if (missing(other_keys)) other_keys <- NULL
  if (missing(additional_metadata) || is.null(additional_metadata)) additional_metadata <- list()
  if (!test_subset(other_keys, names(x))) {
    cli_abort("`other_keys` must be contained in the column names of `x`.")
  }
  if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
    cli_abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  }
  if (any(names(additional_metadata) %in% c("geo_type", "time_type"))) {
    cli_warn("`additional_metadata` names overlap with existing metadata fields \"geo_type\", \"time_type\".")
  }

  # Conduct checks and apply defaults for `compactify`
  if (missing(compactify)) {
    compactify <- NULL
  }
  assert_logical(compactify, len = 1, null.ok = TRUE)

  # Apply defaults and conduct checks for
  # `clobberable_versions_start`, `versions_end`:
  if (missing(clobberable_versions_start)) {
    clobberable_versions_start <- NA
  }
  if (missing(versions_end) || is.null(versions_end)) {
    versions_end <- max_version_with_row_in(x)
  }
  validate_version_bound(clobberable_versions_start, x, na_ok = TRUE)
  validate_version_bound(versions_end, x, na_ok = FALSE)
  if (nrow(x) > 0L && versions_end < max(x[["version"]])) {
    cli_abort(
      sprintf(
        "`versions_end` was %s, but `x` contained
                             updates for a later version or versions, up through %s",
        versions_end, max(x[["version"]])
      ),
      class = "epiprocess__versions_end_earlier_than_updates"
    )
  }
  if (!is.na(clobberable_versions_start) && clobberable_versions_start > versions_end) {
    cli_abort(
      sprintf(
        "`versions_end` was %s, but a `clobberable_versions_start`
                             of %s indicated that there were later observed versions",
        versions_end, clobberable_versions_start
      ),
      class = "epiprocess__versions_end_earlier_than_clobberable_versions_start"
    )
  }

  # --- End of validation and replacing missing args with defaults ---

  # Create the data table; if x was an un-keyed data.table itself,
  # then the call to as.data.table() will fail to set keys, so we
  # need to check this, then do it manually if needed
  key_vars <- c("geo_value", "time_value", other_keys, "version")
  DT <- as.data.table(x, key = key_vars) # nolint: object_name_linter
  if (!identical(key_vars, key(DT))) setkeyv(DT, cols = key_vars)

  maybe_first_duplicate_key_row_index <- anyDuplicated(DT, by = key(DT))
  if (maybe_first_duplicate_key_row_index != 0L) {
    cli_abort("`x` must have one row per unique combination of the key variables. If you
            have additional key variables other than `geo_value`, `time_value`, and
            `version`, such as an age group column, please specify them in `other_keys`.
            Otherwise, check for duplicate rows and/or conflicting values for the same
            measurement.",
      class = "epiprocess__epi_archive_requires_unique_key"
    )
  }

  # Checks to see if a value in a vector is LOCF
  is_locf <- function(vec) {
    dplyr::if_else(!is.na(vec) & !is.na(dplyr::lag(vec)),
      vec == dplyr::lag(vec),
      is.na(vec) & is.na(dplyr::lag(vec))
    )
  }

  # LOCF is defined by a row where all values except for the version
  # differ from their respective lag values

  # Checks for LOCF's in a data frame
  rm_locf <- function(df) {
    dplyr::filter(df, if_any(c(everything(), -version), ~ !is_locf(.)))
  }

  # Keeps LOCF values, such as to be printed
  keep_locf <- function(df) {
    dplyr::filter(df, if_all(c(everything(), -version), ~ is_locf(.)))
  }

  # Runs compactify on data frame
  if (is.null(compactify) || compactify == TRUE) {
    elim <- keep_locf(DT)
    DT <- rm_locf(DT) # nolint: object_name_linter
  } else {
    # Create empty data frame for nrow(elim) to be 0
    elim <- tibble::tibble()
  }

  # Warns about redundant rows
  if (is.null(compactify) && nrow(elim) > 0) {
    warning_intro <- cli::format_inline(
      "Found rows that appear redundant based on
            last (version of each) observation carried forward;
            these rows have been removed to 'compactify' and save space:",
      keep_whitespace = FALSE
    )
    warning_data <- paste(collapse = "\n", capture.output(print(elim, topn = 3L, nrows = 7L)))
    warning_outro <- cli::format_inline(
      "Built-in `epi_archive` functionality should be unaffected,
            but results may change if you work directly with its fields (such as `DT`).
            See `?as_epi_archive` for details.
            To silence this warning but keep compactification,
            you can pass `compactify=TRUE` when constructing the archive.",
      keep_whitespace = FALSE
    )
    warning_message <- paste(sep = "\n", warning_intro, warning_data, warning_outro)
    rlang::warn(warning_message, class = "epiprocess__compactify_default_removed_rows")
  }

  structure(
    list(
      DT = DT,
      geo_type = geo_type,
      time_type = time_type,
      additional_metadata = additional_metadata,
      clobberable_versions_start = clobberable_versions_start,
      versions_end = versions_end,
      private = list()
    ),
    class = "epi_archive"
  )
}

#' Print information about an `epi_archive` object
#' @param class Boolean; whether to print the class label header
#' @param methods Boolean; whether to print all available methods of
#'   the archive
#' @importFrom cli cli_inform
#' @export
print.epi_archive <- function(epi_archive, class = TRUE, methods = TRUE) {
  cli_inform(
    c(
      ">" = if (class) "An `epi_archive` object, with metadata:",
      "i" = if (length(setdiff(key(epi_archive$DT), c("geo_value", "time_value", "version"))) > 0) {
        "Non-standard DT keys: {setdiff(key(epi_archive$DT), c('geo_value', 'time_value', 'version'))}"
      },
      "i" = "Min/max time values: {min(epi_archive$DT$time_value)} / {max(epi_archive$DT$time_value)}",
      "i" = "First/last version with update: {min(epi_archive$DT$version)} / {max(epi_archive$DT$version)}",
      "i" = if (!is.na(epi_archive$clobberable_versions_start)) {
        "Clobberable versions start: {epi_archive$clobberable_versions_start}"
      },
      "i" = "Versions end: {epi_archive$versions_end}",
      "i" = "A preview of the table ({nrow(epi_archive$DT)} rows x {ncol(epi_archive$DT)} columns):"
    )
  )

  return(invisible(epi_archive$DT %>% print()))
}


#' @export
as_of <- function(x, ...) {
  UseMethod("as_of")
}


#' As of epi_archive
#' @description Generates a snapshot in `epi_df` format as of a given version.
#'   See the documentation for the wrapper function [`epix_as_of()`] for
#'   details. The parameter descriptions below are copied from there
#' @param epi_archive An `epi_archive` object
#' @param max_version Version specifying the max version to permit in the
#'   snapshot. That is, the snapshot will comprise the unique rows of the
#'   current archive data that represent the most up-to-date signal values, as
#'   of the specified `max_version` (and whose `time_value`s are at least
#'   `min_time_value`).
#' @param min_time_value Time value specifying the min `time_value` to permit in
#'   the snapshot. Default is `-Inf`, which effectively means that there is no
#'   minimum considered.
#' @param all_versions Boolean; If `all_versions = TRUE`, then the output will be in
#'   `epi_archive` format, and contain rows in the specified `time_value` range
#'   having `version <= max_version`. The resulting object will cover a
#'   potentially narrower `version` and `time_value` range than `x`, depending
#'   on user-provided arguments. Otherwise, there will be one row in the output
#'   for the `max_version` of each `time_value`. Default is `FALSE`.
#' @importFrom data.table between key
#' @export
as_of.epi_archive <- function(epi_archive, max_version, min_time_value = -Inf, all_versions = FALSE) {
  other_keys <- setdiff(
    key(epi_archive$DT),
    c("geo_value", "time_value", "version")
  )
  if (length(other_keys) == 0) other_keys <- NULL

  # Check a few things on max_version
  if (!test_set_equal(class(max_version), class(epi_archive$DT$version))) {
    cli_abort(
      "`max_version` must have the same classes as `epi_archive$DT$version`."
    )
  }
  if (!test_set_equal(typeof(max_version), typeof(epi_archive$DT$version))) {
    cli_abort(
      "`max_version` must have the same types as `epi_archive$DT$version`."
    )
  }
  assert_scalar(max_version, na.ok = FALSE)
  if (max_version > epi_archive$versions_end) {
    cli_abort("`max_version` must be at most `epi_archive$versions_end`.")
  }
  assert_logical(all_versions, len = 1)
  if (!is.na(epi_archive$clobberable_versions_start) && max_version >= epi_archive$clobberable_versions_start) {
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
    result <- epix_truncate_versions_after(epi_archive, max_version)
    result$DT <- result$DT[time_value >= min_time_value, ]
    return(result)
  }

  # Make sure to use data.table ways of filtering and selecting
  as_of_epi_df <- epi_archive$DT[time_value >= min_time_value & version <= max_version, ] %>%
    unique(
      by = c("geo_value", "time_value", other_keys),
      fromLast = TRUE
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(-"version") %>%
    as_epi_df(
      geo_type = epi_archive$geo_type,
      time_type = epi_archive$time_type,
      as_of = max_version,
      additional_metadata = c(epi_archive$additional_metadata,
        other_keys = other_keys
      )
    )

  return(as_of_epi_df)
}


#' @export
fill_through_version <- function(x, ...) {
  UseMethod("fill_through_version")
}


#' Fill through version
#' @description Fill in unobserved history using requested scheme by mutating
#'   the given object and potentially reseating its fields. See
#'   [`epix_fill_through_version`], which doesn't mutate the input archive but
#'   might alias its fields.
#'
#' @param epi_archive an `epi_archive` object
#' @param fill_versions_end as in [`epix_fill_through_version`]
#' @param how as in [`epix_fill_through_version`]
#'
#' @importFrom data.table key setkeyv := address copy
#' @importFrom rlang arg_match
fill_through_version.epi_archive <- function(
    epi_archive,
    fill_versions_end,
    how = c("na", "locf")) {
  validate_version_bound(fill_versions_end, epi_archive$DT, na_ok = FALSE)
  how <- arg_match(how)
  if (epi_archive$versions_end < fill_versions_end) {
    new_dt <- switch(how,
      "na" = {
        # old DT + a version consisting of all NA observations
        # immediately after the last currently/actually-observed
        # version. Note that this NA-observation version must only be
        # added if `epi_archive` is outdated.
        nonversion_key_cols <- setdiff(key(epi_archive$DT), "version")
        nonkey_cols <- setdiff(names(epi_archive$DT), key(epi_archive$DT))
        next_version_tag <- next_after(epi_archive$versions_end)
        if (next_version_tag > fill_versions_end) {
          cli_abort(sprintf(paste(
            "Apparent problem with `next_after` method:",
            "archive contained observations through version %s",
            "and the next possible version was supposed to be %s,",
            "but this appeared to jump from a version < %3$s",
            "to one > %3$s, implying at least one version in between."
          ), epi_archive$versions_end, next_version_tag, fill_versions_end))
        }
        nonversion_key_vals_ever_recorded <- unique(epi_archive$DT, by = nonversion_key_cols)
        # In edge cases, the `unique` result can alias the original
        # DT; detect and copy if necessary:
        if (identical(address(epi_archive$DT), address(nonversion_key_vals_ever_recorded))) {
          nonversion_key_vals_ever_recorded <- copy(nonversion_key_vals_ever_recorded)
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
        setkeyv(rbind(epi_archive$DT, next_version_dt), key(epi_archive$DT))[]
      },
      "locf" = {
        # just the old DT; LOCF is built into other methods:
        epi_archive$DT
      }
    )
    new_versions_end <- fill_versions_end
    # Update `epi_archive` all at once with simple, error-free operations +
    # return below:
    epi_archive$DT <- new_dt
    epi_archive$versions_end <- new_versions_end
  } else {
    # Already sufficiently up to date; nothing to do.
  }
  return(invisible(epi_archive))
}


#' @export
truncate_versions_after <- function(x, ...) {
  UseMethod("truncate_versions_after")
}


#' Truncate versions after
#' @description Filter to keep only older versions, mutating the archive by
#'   potentially reseating but not mutating some fields. `DT` is likely, but not
#'   guaranteed, to be copied. Returns the mutated archive
#'   [invisibly][base::invisible].
#' @param epi_archive as in [`epix_truncate_versions_after`]
#' @param max_version as in [`epix_truncate_versions_after`]
truncate_versions_after.epi_archive <- function(
    epi_archive,
    max_version) {
  if (!test_set_equal(class(max_version), class(epi_archive$DT$version))) {
    cli_abort("`max_version` must have the same classes as `epi_archive$DT$version`.")
  }
  if (!test_set_equal(typeof(max_version), typeof(epi_archive$DT$version))) {
    cli_abort("`max_version` must have the same types as `epi_archive$DT$version`.")
  }
  assert_scalar(max_version, na.ok = FALSE)
  if (max_version > epi_archive$versions_end) {
    cli_abort("`max_version` must be at most `epi_archive$versions_end`.")
  }
  epi_archive$DT <- epi_archive$DT[epi_archive$DT$version <= max_version, colnames(epi_archive$DT), with = FALSE]
  # (^ this filter operation seems to always copy the DT, even if it
  # keeps every entry; we don't guarantee this behavior in
  # documentation, though, so we could change to alias in this case)
  if (!is.na(epi_archive$clobberable_versions_start) && epi_archive$clobberable_versions_start > max_version) {
    epi_archive$clobberable_versions_start <- NA
  }
  epi_archive$versions_end <- max_version
  return(invisible(epi_archive))
}


#' Merge epi archive
#' @description Merges another `epi_archive` with the current one, mutating the
#'   current one by reseating its `DT` and several other fields, but avoiding
#'   mutation of the old `DT`; returns the current archive
#'   [invisibly][base::invisible]. See [`epix_merge`] for a full description
#'   of the non-R6-method version, which does not mutate either archive, and
#'   does not alias either archive's `DT`.a
#' @param x as in [`epix_merge`]
#' @param y as in [`epix_merge`]
#' @param sync as in [`epix_merge`]
#' @param compactify as in [`epix_merge`]
merge_epi_archive <- function(
    x,
    y,
    sync = c("forbid", "na", "locf", "truncate"),
    compactify = TRUE) {
  result <- epix_merge(x, y,
    sync = sync,
    compactify = compactify
  )

  if (length(x$private_fields) != 0L) {
    cli_abort("expected no private fields in x",
      internal = TRUE
    )
  }

  # Mutate fields all at once, trying to avoid any potential errors:
  for (field_name in names(x$public_fields)) {
    x[[field_name]] <- result[[field_name]]
  }

  return(invisible(x))
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
#' @param .drop As described in [`dplyr::group_by`]; determines treatment of
#'   factor columns.
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
#' grouped_archive <- archive_cases_dv_subset %>% group_by(geo_value)
#'
#' # `print` for metadata and method listing:
#' grouped_archive %>% print()
#'
#' # The primary use for grouping is to perform a grouped `epix_slide`:
#'
#' archive_cases_dv_subset %>%
#'   group_by(geo_value) %>%
#'   epix_slide(
#'     f = ~ mean(.x$case_rate_7d_av),
#'     before = 2,
#'     ref_time_values = as.Date("2020-06-11") + 0:2,
#'     new_col_name = "case_rate_3d_av"
#'   ) %>%
#'   ungroup()
#'
#' # -----------------------------------------------------------------
#'
#' # Advanced: some other features of dplyr grouping are implemented:
#'
#' library(dplyr)
#' toy_archive <-
#'   tribble(
#'     ~geo_value, ~age_group, ~time_value, ~version, ~value,
#'     "us", "adult", "2000-01-01", "2000-01-02", 121,
#'     "us", "pediatric", "2000-01-02", "2000-01-03", 5, # (addition)
#'     "us", "adult", "2000-01-01", "2000-01-03", 125, # (revision)
#'     "us", "adult", "2000-01-02", "2000-01-03", 130 # (addition)
#'   ) %>%
#'   mutate(
#'     age_group = ordered(age_group, c("pediatric", "adult")),
#'     time_value = as.Date(time_value),
#'     version = as.Date(version)
#'   ) %>%
#'   as_epi_archive(other_keys = "age_group")
#'
#' # The following are equivalent:
#' toy_archive %>% group_by(geo_value, age_group)
#' toy_archive %>%
#'   group_by(geo_value) %>%
#'   group_by(age_group, .add = TRUE)
#' grouping_cols <- c("geo_value", "age_group")
#' toy_archive %>% group_by(across(all_of(grouping_cols)))
#'
#' # And these are equivalent:
#' toy_archive %>% group_by(geo_value)
#' toy_archive %>%
#'   group_by(geo_value, age_group) %>%
#'   ungroup(age_group)
#'
#' # To get the grouping variable names as a `list` of `name`s (a.k.a. symbols):
#' toy_archive %>%
#'   group_by(geo_value) %>%
#'   groups()
#'
#' toy_archive %>%
#'   group_by(geo_value, age_group, .drop = FALSE) %>%
#'   epix_slide(f = ~ sum(.x$value), before = 20) %>%
#'   ungroup()
#'
#' @importFrom dplyr group_by
#' @export
#'
#' @aliases grouped_epi_archive
group_by.epi_archive <- function(epi_archive, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(epi_archive)) {
  # `add` makes no difference; this is an ungrouped `epi_archive`.
  detailed_mutate <- epix_detailed_restricted_mutate(epi_archive, ...)
  assert_logical(.drop)
  if (!.drop) {
    grouping_cols <- as.list(detailed_mutate[["archive"]][["DT"]])[detailed_mutate[["request_names"]]]
    grouping_col_is_factor <- purrr::map_lgl(grouping_cols, is.factor)
    # ^ Use `as.list` to try to avoid any possibility of a deep copy.
    if (!any(grouping_col_is_factor)) {
      cli_warn(
        "`.drop=FALSE` but there are no factor grouping columns;
        did you mean to convert one of the columns to a factor beforehand?",
        class = "epiprocess__group_by_epi_archive__drop_FALSE_no_factors"
      )
    } else if (any(diff(grouping_col_is_factor) == -1L)) {
      cli_warn(
        "`.drop=FALSE` but there are one or more non-factor grouping columns listed
        after a factor grouping column; this may produce groups with `NA`s for these
        columns; see https://github.com/tidyverse/dplyr/issues/5369#issuecomment-683762553;
        depending on how you want completion to work, you might instead want to convert all
        grouping columns to factors beforehand, specify the non-factor grouping columns first,
        or use `.drop=TRUE` and add a call to `tidyr::complete`.",
        class = "epiprocess__group_by_epi_archive__drop_FALSE_nonfactor_after_factor"
      )
    }
  }
  new_grouped_epi_archive(detailed_mutate[["archive"]],
    detailed_mutate[["request_names"]],
    drop = .drop
  )
}


#' @export
slide <- function(.data, ...) {
  UseMethod("slide")
}


#' Slide over epi archive
#' @description Slides a given function over variables in an `epi_archive`
#'   object. See the documentation for the wrapper function [`epix_slide()`] for
#'   details. The parameter descriptions below are copied from there
#' @importFrom data.table key
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
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
slide.epi_archive <- function(epi_archive, f, ..., before, ref_time_values,
                              time_step, new_col_name = "slide_value",
                              as_list_col = FALSE, names_sep = "_",
                              all_versions = FALSE) {
  # For an "ungrouped" slide, treat all rows as belonging to one big
  # group (group by 0 vars), like `dplyr::summarize`, and let the
  # resulting `grouped_epi_archive` handle the slide:
  slide(
    group_by(epi_archive),
    f,
    ...,
    before = before, ref_time_values = ref_time_values,
    time_step = time_step, new_col_name = new_col_name,
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


#' Convert to `epi_archive` format
#'
#' Converts a data frame, data table, or tibble into an `epi_archive`
#' object. See the [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples. The parameter descriptions below are copied from there
#'
#' @param x A data frame, data table, or tibble, with columns `geo_value`,
#'   `time_value`, `version`, and then any additional number of columns.
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `geo_type` and `time_type`
#'   fields; named entries from the passed list or will be included as well.
#' @param compactify Optional; Boolean or `NULL`: should we remove rows that are
#'   considered redundant for the purposes of `epi_archive`'s built-in methods
#'   such as `as_of`? As these methods use the last version of each observation
#'   carried forward (LOCF) to interpolate between the version data provided,
#'   rows that don't change these LOCF results can potentially be omitted to
#'   save space. `TRUE` will remove these rows, `FALSE` will not, and missing or
#'   `NULL` will remove these rows and issue a warning. Generally, this can be
#'   set to `TRUE`, but if you directly inspect or edit the fields of the
#'   `epi_archive` such as its `DT`, you will have to determine whether
#'   `compactify=TRUE` will produce the desired results. If compactification
#'   here is removing a large proportion of the rows, this may indicate a
#'   potential for space, time, or bandwidth savings upstream the data pipeline,
#'   e.g., when fetching, storing, or preparing the input data `x`
#' @param clobberable_versions_start Optional; `length`-1; either a value of the
#'   same `class` and `typeof` as `x$version`, or an `NA` of any `class` and
#'   `typeof`: specifically, either (a) the earliest version that could be
#'   subject to "clobbering" (being overwritten with different update data, but
#'   using the *same* version tag as the old update data), or (b) `NA`, to
#'   indicate that no versions are clobberable. There are a variety of reasons
#'   why versions could be clobberable under routine circumstances, such as (a)
#'   today's version of one/all of the columns being published after initially
#'   being filled with `NA` or LOCF, (b) a buggy version of today's data being
#'   published but then fixed and republished later in the day, or (c) data
#'   pipeline delays (e.g., publisher uploading, periodic scraping, database
#'   syncing, periodic fetching, etc.) that make events (a) or (b) reflected
#'   later in the day (or even on a different day) than expected; potential
#'   causes vary between different data pipelines. The default value is `NA`,
#'   which doesn't consider any versions to be clobberable. Another setting that
#'   may be appropriate for some pipelines is `max_version_with_row_in(x)`.
#' @param versions_end Optional; length-1, same `class` and `typeof` as
#'   `x$version`: what is the last version we have observed? The default is
#'   `max_version_with_row_in(x)`, but values greater than this could also be
#'   valid, and would indicate that we observed additional versions of the data
#'   beyond `max(x$version)`, but they all contained empty updates. (The default
#'   value of `clobberable_versions_start` does not fully trust these empty
#'   updates, and assumes that any version `>= max(x$version)` could be
#'   clobbered.) If `nrow(x) == 0`, then this argument is mandatory.
#' @return An `epi_archive` object.
#'
#' @details This simply a wrapper around the `new()` method of the `epi_archive`
#'   class, so for example:
#'   ```
#'   x <- as_epi_archive(df, geo_type = "state", time_type = "day")
#'   ```
#'   would be equivalent to:
#'   ```
#'   x <- epi_archive$new(df, geo_type = "state", time_type = "day")
#'   ```
#'
#' @export
#' @examples
#' # Simple ex. with necessary keys
#' tib <- tibble::tibble(
#'   geo_value = rep(c("ca", "hi"), each = 5),
#'   time_value = rep(seq(as.Date("2020-01-01"),
#'     by = 1, length.out = 5
#'   ), times = 2),
#'   version = rep(seq(as.Date("2020-01-02"),
#'     by = 1, length.out = 5
#'   ), times = 2),
#'   value = rnorm(10, mean = 2, sd = 1)
#' )
#'
#' toy_epi_archive <- tib %>% as_epi_archive(
#'   geo_type = "state",
#'   time_type = "day"
#' )
#' toy_epi_archive
#'
#' # Ex. with an additional key for county
#' df <- data.frame(
#'   geo_value = c(replicate(2, "ca"), replicate(2, "fl")),
#'   county = c(1, 3, 2, 5),
#'   time_value = c(
#'     "2020-06-01",
#'     "2020-06-02",
#'     "2020-06-01",
#'     "2020-06-02"
#'   ),
#'   version = c(
#'     "2020-06-02",
#'     "2020-06-03",
#'     "2020-06-02",
#'     "2020-06-03"
#'   ),
#'   cases = c(1, 2, 3, 4),
#'   cases_rate = c(0.01, 0.02, 0.01, 0.05)
#' )
#'
#' x <- df %>% as_epi_archive(
#'   geo_type = "state",
#'   time_type = "day",
#'   other_keys = "county"
#' )
as_epi_archive <- function(x, geo_type, time_type, other_keys,
                           additional_metadata = list(),
                           compactify = NULL,
                           clobberable_versions_start = NA,
                           versions_end = max_version_with_row_in(x)) {
  new_epi_archive(
    x, geo_type, time_type, other_keys, additional_metadata,
    compactify, clobberable_versions_start, versions_end
  )
}

#' Test for `epi_archive` format
#'
#' @param x An object.
#' @param grouped_okay Optional; Boolean; should a `grouped_epi_archive` also
#'   count? Default is `FALSE`.
#' @return `TRUE` if the object inherits from `epi_archive`.
#'
#' @export
#' @examples
#' is_epi_archive(jhu_csse_daily_subset) # FALSE (this is an epi_df, not epi_archive)
#' is_epi_archive(archive_cases_dv_subset) # TRUE
#'
#' # By default, grouped_epi_archives don't count as epi_archives, as they may
#' # support a different set of operations from regular `epi_archives`. This
#' # behavior can be controlled by `grouped_okay`.
#' grouped_archive <- archive_cases_dv_subset %>% group_by(geo_value)
#' is_epi_archive(grouped_archive) # FALSE
#' is_epi_archive(grouped_archive, grouped_okay = TRUE) # TRUE
#'
#' @seealso [`is_grouped_epi_archive`]
is_epi_archive <- function(x, grouped_okay = FALSE) {
  inherits(x, "epi_archive") || grouped_okay && inherits(x, "grouped_epi_archive")
}


#' @export
clone <- function(x, ...) {
  UseMethod("clone")
}


#' @export
clone.epi_archive <- function(epi_archive) {
  epi_archive$DT <- copy(epi_archive$DT)
  return(epi_archive)
}
