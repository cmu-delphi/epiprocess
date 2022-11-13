# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable.aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable.aware = TRUE

#' Validate a version bound arg
#'
#' Expected to be used on `clobberable_versions_start`, `versions_end`,
#' and similar arguments. Some additional context-specific checks may be needed.
#'
#' @param version_bound the version bound to validate
#' @param x a data frame containing a version column with which to check
#'   compatibility
#' @param na_ok Boolean; is `NULL` an acceptable "bound"? (If so, `NULL` will
#'   have a special context-dependent meaning.)
#' @param version_bound_arg optional string; what to call the version bound in
#'   error messages
#'
#' @section Side effects: raises an error if version bound appears invalid
#'
#' @noRd
validate_version_bound = function(version_bound, x, na_ok,
                                  version_bound_arg = rlang::caller_arg(version_bound),
                                  x_arg = rlang::caller_arg(version_bound)) {
  # We might want some (optional?) validation here to detect internal bugs.
  if (length(version_bound) != 1L) {
    # Check for length-1-ness fairly early so we don't have to worry as much
    # about our `if`s receiving non-length-1 "Boolean"s.
    Abort(sprintf("`version_bound` must have length 1, but instead was length %d",
                  length(version_bound)),
          class=sprintf("epiprocess__%s_is_not_length_1", version_bound_arg))
  } else if (is.na(version_bound)) {
    # Check for NA before class&type, as any-class&type NA should be fine for
    # our purposes, and some version classes&types might not have their own NA
    # value to pass in.
    if (na_ok) {
      # Looks like a valid version bound; exit without error.
      return(invisible(NULL))
    } else {
      Abort(sprintf(
        '`%s` must not satisfy `is.na` (NAs are not allowed for this kind of version bound)',
        version_bound_arg
      ), class=sprintf("epiprocess__%s_is_na", version_bound_arg))
    }
  } else  if (!identical(class(version_bound), class(x[["version"]])) ||
                !identical(typeof(version_bound), typeof(x[["version"]]))) {
    Abort(sprintf(
      '`class(%1$s)` must be identical to `class(%2$s)` and `typeof(%1$s)` must be identical to `typeof(%2$s)`',
      version_bound_arg,
      # '{x_arg}[["version"]]' except adding parentheses if needed:
      rlang::expr_deparse(rlang::new_call(
        quote(`[[`), rlang::pairlist2(rlang::parse_expr(x_arg), "version")
      ))
    ), class=sprintf("epiprocess__%s_has_invalid_class_or_typeof", version_bound_arg))
  } else {
    # Looks like a valid version bound; exit without error.
    return(invisible(NULL))
  }
}

#' Default arg helper: `max(x$version)`, with error if `x` has 0 rows
#'
#' Exported to make defaults more easily copyable.
#'
#' @param x `x` argument of [`as_epi_archive`]
#'
#' @return `max(x$version)` if it has any rows; raises error if it has 0 rows or
#'   an `NA` version value
#'
#' @export
max_version_with_row_in = function(x) {
  if (nrow(x) == 0L) {
    Abort(sprintf("`nrow(x)==0L`, representing a data set history with no row up through the latest observed version, but we don't have a sensible guess at what version that is, or whether any of the empty versions might be clobbered in the future; if we use `x` to form an `epi_archive`, then `clobberable_versions_start` and `versions_end` must be manually specified."),
          class="epiprocess__max_version_cannot_be_used")
  } else {
    version_col = purrr::pluck(x, "version") # error not NULL if doesn't exist
    if (anyNA(version_col)) {
      Abort("version values cannot be NA",
            class="epiprocess__version_values_must_not_be_na")
    } else {
      version_bound <- max(version_col)
    }
  }
}

#' Get the next possible value greater than `x` of the same type
#'
#' @param x the starting "value"(s)
#' @return same class, typeof, and length as `x`
#'
#' @export
next_after = function(x) UseMethod("next_after")

#' @export
next_after.integer = function(x) x + 1L

#' @export
next_after.Date = function(x) x + 1L

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
#'   observation appears in the archive data at all.
#'
#' **A word of caution:** R6 objects, unlike most other objects in R, have
#'   reference semantics. A primary consequence of this is that objects are not
#'   copied when modified. You can read more about this in Hadley Wickham's
#'   [Advanced R](https://adv-r.hadley.nz/r6.html#r6-semantics) book. In order
#'   to construct a modified archive while keeping the original intact, first
#'   make a clone using the `$clone` method, then overwrite the clone's `DT`
#'   field with `data.table::copy(clone$DT)`, and finally perform the
#'   modifications on the clone.
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
#' @importFrom R6 R6Class
#' @export
#' @examples
#' tib <- tibble::tibble(
#'   geo_value = rep(c("ca", "hi"), each = 5),
#'   time_value = rep(seq(as.Date("2020-01-01"), 
#'                        by = 1, length.out = 5), times = 2),
#'   version = rep(seq(as.Date("2020-01-02"), 
#'                     by = 1, length.out = 5), times = 2),
#'   value = rnorm(10, mean = 2, sd = 1)
#' )
#' 
#' toy_epi_archive <- tib %>% epi_archive$new(geo_type = "state", 
#'                                            time_type = "day")
#' toy_epi_archive 
epi_archive =
  R6::R6Class(
        classname = "epi_archive",
        #####
        public = list(
          DT = NULL,
          geo_type = NULL,
          time_type = NULL,
          additional_metadata = NULL,
          clobberable_versions_start = NULL,
          versions_end = NULL,
#' @description Creates a new `epi_archive` object.
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
#'   save space while maintaining the same behavior (with the help of the
#'   `clobberable_versions_start` and `versions_end` fields in some
#'   edge cases). `TRUE` will remove these rows, `FALSE` will not, and missing
#'   or `NULL` will remove these rows and issue a warning. Generally, this can
#'   be set to `TRUE`, but if you directly inspect or edit the fields of the
#'   `epi_archive` such as its `DT`, you will have to determine whether
#'   `compactify=TRUE` will produce the desired results. If compactification
#'   here is removing a large proportion of the rows, this may indicate a
#'   potential for space, time, or bandwidth savings upstream the data pipeline,
#'   e.g., when fetching, storing, or preparing the input data `x`
#' @param clobberable_versions_start Optional; as in [`as_epi_archive`]
#' @param versions_end Optional; as in [`as_epi_archive`]
#' @return An `epi_archive` object.
#' @importFrom data.table as.data.table key setkeyv
#' 
#' @details 
#' Refer to the documentation for [as_epi_archive()] for more information 
#' and examples of parameter names.
          initialize = function(x, geo_type, time_type, other_keys,
                                additional_metadata, compactify,
                                clobberable_versions_start, versions_end) {
            # Check that we have a data frame
            if (!is.data.frame(x)) {
              Abort("`x` must be a data frame.")
            }
                  
            # Check that we have geo_value, time_value, version columns
            if (!("geo_value" %in% names(x))) {
              Abort("`x` must contain a `geo_value` column.")
            }
            if (!("time_value" %in% names(x))) {
              Abort("`x` must contain a `time_value` column.")
            }
            if (!("version" %in% names(x))) {
              Abort("`x` must contain a `version` column.")
            }
            if (anyNA(x$version)) {
              Abort("`x$version` must not contain `NA`s",
                    class = "epiprocess__version_values_must_not_be_na")
            }
              
            # If geo type is missing, then try to guess it
            if (missing(geo_type)) {
              geo_type = guess_geo_type(x$geo_value)
            }

            # If time type is missing, then try to guess it
            if (missing(time_type)) {
              time_type = guess_time_type(x$time_value)
            }
            
            # Finish off with small checks on keys variables and metadata
            if (missing(other_keys)) other_keys = NULL
            if (missing(additional_metadata)) additional_metadata = list()
            if (!all(other_keys %in% names(x))) {
              Abort("`other_keys` must be contained in the column names of `x`.")
            }
            if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
              Abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
            }
            if (any(names(additional_metadata) %in%
                    c("geo_type", "time_type"))) {
              Warn("`additional_metadata` names overlap with existing metadata fields \"geo_type\", \"time_type\".")
            }

            # Conduct checks and apply defaults for `compactify`
            if (missing(compactify)) {
              compactify = NULL
            } else if (!rlang::is_bool(compactify) &&
                         !rlang::is_null(compactify)) {
              Abort("compactify must be boolean or null.")
            }

            # Apply defaults and conduct checks and apply defaults for
            # `clobberable_versions_start`, `versions_end`:
            if (missing(clobberable_versions_start)) {
              clobberable_versions_start <- max_version_with_row_in(x)
            }
            if (missing(versions_end)) {
              versions_end <- max_version_with_row_in(x)
            }
            validate_version_bound(clobberable_versions_start, x, na_ok=TRUE)
            validate_version_bound(versions_end, x, na_ok=FALSE)
            if (nrow(x) > 0L && versions_end < max(x[["version"]])) {
              Abort(sprintf("`versions_end` was %s, but `x` contained
                             updates for a later version or versions, up through %s",
                            versions_end, max(x[["version"]])),
                    class="epiprocess__versions_end_earlier_than_updates")
            }
            if (!is.na(clobberable_versions_start) && clobberable_versions_start > versions_end) {
              Abort(sprintf("`versions_end` was %s, but a `clobberable_versions_start`
                             of %s indicated that there were later observed versions",
                            versions_end, clobberable_versions_start),
                    class="epiprocess__versions_end_earlier_than_clobberable_versions_start")
            }

            # --- End of validation and replacing missing args with defaults ---

            # Create the data table; if x was an un-keyed data.table itself,
            # then the call to as.data.table() will fail to set keys, so we
            # need to check this, then do it manually if needed
            key_vars = c("geo_value", "time_value", other_keys, "version")
            DT = as.data.table(x, key = key_vars)
            if (!identical(key_vars, key(DT))) setkeyv(DT, cols = key_vars)
            
            # Checks to see if a value in a vector is LOCF
            is_locf <- function(vec) {
              dplyr::if_else(!is.na(vec) & !is.na(dplyr::lag(vec)),
                     vec == dplyr::lag(vec),
                     is.na(vec) & is.na(dplyr::lag(vec)))
            }
            
            # LOCF is defined by a row where all values except for the version
            # differ from their respective lag values
            
            # Checks for LOCF's in a data frame
            rm_locf <- function(df) {
             dplyr::filter(df,if_any(c(everything(),-version),~ !is_locf(.))) 
            }
            
            # Keeps LOCF values, such as to be printed
            keep_locf <- function(df) {
              dplyr::filter(df,if_all(c(everything(),-version),~ is_locf(.))) 
            }
            
            # Runs compactify on data frame
            if (is.null(compactify) || compactify == TRUE) {
              elim = keep_locf(DT)
              DT = rm_locf(DT)
            } else {
              # Create empty data frame for nrow(elim) to be 0
              elim = tibble::tibble()
            }
            
            # Warns about redundant rows
            if (is.null(compactify) && nrow(elim) > 0) {
              warning_intro <- break_str(paste(
                'Found rows that appear redundant based on',
                'last (version of each) observation carried forward;',
                'these rows have been removed to "compactify" and save space:'
              ))
              
              warning_data <- paste(collapse="\n", capture.output(print(elim, topn=3L, nrows=7L)))
              
              warning_outro <- break_str(paste(
                "Built-in `epi_archive` functionality should be unaffected,",
                "but results may change if you work directly with its fields (such as `DT`).",
                "See `?as_epi_archive` for details.",
                "To silence this warning but keep compactification,",
                "you can pass `compactify=TRUE` when constructing the archive."
              ))
              
              warning_message <- paste(sep="\n", warning_intro, warning_data, warning_outro)
              
              rlang::warn(warning_message, class="epiprocess__compactify_default_removed_rows")
            }
            
            # Instantiate all self variables
            self$DT = DT
            self$geo_type = geo_type
            self$time_type = time_type
            self$additional_metadata = additional_metadata
            self$clobberable_versions_start = clobberable_versions_start
            self$versions_end = versions_end
          },
          print = function() {
            cat("An `epi_archive` object, with metadata:\n")
            cat(sprintf("* %-9s = %s\n", "geo_type", self$geo_type))
            cat(sprintf("* %-9s = %s\n", "time_type", self$time_type))
            if (!is.null(self$additional_metadata)) { 
              sapply(self$additional_metadata, function(m) { 
                cat(sprintf("* %-9s = %s\n", names(m), m))
              })
            }
            cat("----------\n")
            if (length(self$DT$time_value) == 0 || all(is.na(self$DT$time_value))) {
              min_time = max_time = NA
            } else {
              min_time = Min(self$DT$time_value)
              max_time = Max(self$DT$time_value)
            }
            cat(sprintf("* %-14s = %s\n", "min time value", min_time))
            cat(sprintf("* %-14s = %s\n", "max time value", max_time))
            cat(sprintf("* %-14s = %s\n", "first version with update",
                        min(self$DT$version)))
            cat(sprintf("* %-14s = %s\n", "last version with update",
                        max(self$DT$version)))
            if (is.na(self$clobberable_versions_start)) {
              cat("No clobberable versions\n")
            } else {
              cat(sprintf("* %-14s = %s\n", "clobberable versions start",
                          self$clobberable_versions_start))
            }
            cat(sprintf("* %-14s = %s\n", "versions end",
                        self$versions_end))
            cat("----------\n")
            cat(sprintf("Data archive (stored in DT field): %i x %i\n", 
                        nrow(self$DT), ncol(self$DT)))
            cat("----------\n")
            cat(sprintf("Columns in DT: %s\n", paste(ifelse(length(
              colnames(self$DT)) <= 4, paste(colnames(self$DT), collapse = ", "), 
              paste(paste(colnames(self$DT)[1:4], collapse = ", "), "and", 
              length(colnames(self$DT)[5:length(colnames(self$DT))]), "more columns")))))
            cat("----------\n")
            cat(sprintf("Public methods: %s\n",
                        paste(names(epi_archive$public_methods),
                              collapse = ", ")),"\n")

          },
          #####
#' @description Generates a snapshot in `epi_df` format as of a given version.
#'   See the documentation for the wrapper function [`epix_as_of()`] for details.
#' @importFrom data.table between key
          as_of = function(max_version, min_time_value = -Inf) {
            # Self max version and other keys
            other_keys = setdiff(key(self$DT),
                                 c("geo_value", "time_value", "version"))
            if (length(other_keys) == 0) other_keys = NULL
            
            # Check a few things on max_version
            if (!identical(class(max_version), class(self$DT$version)) ||
                  !identical(typeof(max_version), typeof(self$DT$version))) {
              Abort("`max_version` and `DT$version` must have same `class` and `typeof`.")
            }
            if (length(max_version) != 1) {
              Abort("`max_version` cannot be a vector.")
            }
            if (is.na(max_version)) {
              Abort("`max_version` must not be NA.")
            }
            if (max_version > self$versions_end) {
              Abort("`max_version` must be at most `self$versions_end`.")
            }
            if (!is.na(self$clobberable_versions_start) && max_version >= self$clobberable_versions_start) {
              Warn('Getting data as of some "clobberable" version that might be hotfixed, synced, or otherwise replaced later with different data using the same version tag.  Thus, the snapshot that we produce here might not be reproducible later. See `?epi_archive` for more info and `?epix_as_of` on how to muffle.',
                   class="epiprocess__snapshot_as_of_clobberable_version")
            }
            
            # Filter by version and return
            return(
              # Make sure to use data.table ways of filtering and selecting
              self$DT[time_value >= min_time_value &
                        version <= max_version, ] %>%
              unique(by = c("geo_value", "time_value", other_keys),
                     fromLast = TRUE) %>%
              tibble::as_tibble() %>% 
              dplyr::select(-.data$version) %>%
              as_epi_df(geo_type = self$geo_type,
                        time_type = self$time_type,
                        as_of = max_version,
                        additional_metadata = c(self$additional_metadata,
                                                other_keys = other_keys))
            )
          },          
          #####
#' @description Fill in unobserved history using requested scheme by mutating
#'   `self` and potentially reseating its fields. See
#'   [`epix_fill_through_version`] for a full description of the non-R6-method
#'   version, which doesn't mutate the input archive but might alias its fields.
#'
#' @param fill_versions_end as in [`epix_fill_through_version`]
#' @param how as in [`epix_fill_through_version`]
#'
#' @importFrom data.table key setkeyv := address copy
#' @importFrom rlang arg_match
          fill_through_version = function(fill_versions_end,
                                          how=c("na", "locf")) {
            validate_version_bound(fill_versions_end, self$DT, na_ok=FALSE)
            how <- arg_match(how)
            if (self$versions_end < fill_versions_end) {
              new_DT = switch(
                how,
                "na" = {
                  # old DT + a version consisting of all NA observations
                  # immediately after the last currently/actually-observed
                  # version. Note that this NA-observation version must only be
                  # added if `self` is outdated.
                  nonversion_key_cols = setdiff(key(self$DT), "version")
                  nonkey_cols = setdiff(names(self$DT), key(self$DT))
                  next_version_tag = next_after(self$versions_end)
                  if (next_version_tag > fill_versions_end) {
                    Abort(sprintf(paste(
                      "Apparent problem with `next_after` implementation:",
                      "archive contained observations through version %s",
                      "and the next possible version was supposed to be %s,",
                      "but this appeared to jump from a version < %3$s",
                      "to one > %3$s, implying at least one version in between."
                    ), self$versions_end, next_version_tag, fill_versions_end))
                  }
                  nonversion_key_vals_ever_recorded = unique(self$DT, by=nonversion_key_cols)
                  # In edge cases, the `unique` result can alias the original
                  # DT; detect and copy if necessary:
                  if (identical(address(self$DT), address(nonversion_key_vals_ever_recorded))) {
                    nonversion_key_vals_ever_recorded <- copy(nonversion_key_vals_ever_recorded)
                  }
                  next_version_DT = nonversion_key_vals_ever_recorded[
                  , version := next_version_tag][
                    # this makes the class of these columns logical (`NA` is a
                    # logical NA; we're relying on the rbind below to convert to
                    # the proper class&typeof)
                  , (nonkey_cols) := NA]
                  # full result DT:
                  setkeyv(rbind(self$DT, next_version_DT), key(self$DT))[]
                },
                "locf" = {
                  # just the old DT; LOCF is built into other methods:
                  self$DT
                }
              )
              new_versions_end = fill_versions_end
              # Update `self` all at once with simple, error-free operations +
              # return below:
              self$DT <- new_DT
              self$versions_end <- new_versions_end
            } else {
              # Already sufficiently up to date; nothing to do.
            }
            return (invisible(self))
          },
          #####
#' @description Merges another `epi_archive` with the current one, mutating the
#'   current one by reseating its `DT` and several other fields, but avoiding
#'   mutation of the old `DT`; returns the current archive
#'   [invisibly][base::invisible]. See [`epix_merge`] for a full description
#'   of the non-R6-method version, which does not mutate either archive, and
#'   does not alias either archive's `DT`.
#' @param y as in [`epix_merge`]
#' @param sync as in [`epix_merge`]
#' @param compactify as in [`epix_merge`]
          merge = function(y, sync = c("forbid","na","locf","truncate"), compactify = TRUE) {
            result = epix_merge(self, y,
                                sync = sync,
                                compactify = compactify)

            if (length(epi_archive$private_fields) != 0L) {
              Abort("expected no private fields in epi_archive",
                    internal=TRUE)
            }

            # Mutate fields all at once, trying to avoid any potential errors:
            for (field_name in names(epi_archive$public_fields)) {
              self[[field_name]] <- result[[field_name]]
            }

            return (invisible(self))
          },
          #####
#' @description Slides a given function over variables in an `epi_archive`
#'   object. See the documentation for the wrapper function [`epix_slide()`] for
#'   details. 
#' @importFrom data.table key
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
          slide = function(f, ..., before, group_by, ref_time_values, 
                           time_step, new_col_name = "slide_value",
                           as_list_col = FALSE, names_sep = "_",
                           all_rows = FALSE) { 
            # If missing, then set ref time values to be everything; else make
            # sure we intersect with observed time values 
            if (missing(ref_time_values)) {
              ref_time_values = unique(self$DT$time_value)
            }
            else {
              ref_time_values = ref_time_values[ref_time_values %in%
                                                unique(self$DT$time_value)]
            }

            # Validate and pre-process `before`:
            if (missing(before)) {
              Abort("`before` is required (and must be passed by name);
                     if you did not want to apply a sliding window but rather
                     to map `as_of` and `f` across various `ref_time_values`,
                     pass a large `before` value (e.g., if time steps are days,
                     `before=365000`).")
            }
            before <- vctrs::vec_cast(before, integer())
            if (length(before) != 1L || is.na(before) || before < 0L) {
              Abort("`before` must be length-1, non-NA, non-negative")
            }

            # If a custom time step is specified, then redefine units 
            if (!missing(time_step)) before <- time_step(before)
            
            # What to group by? If missing, set according to internal keys;
            # otherwise, tidyselect.
            if (quo_is_missing(enquo(group_by))) {
              group_by <- syms(setdiff(key(self$DT), c("time_value", "version")))
            } else {
              group_by <- syms(names(eval_select(enquo(group_by), self$DT)))
            }
            
            # Symbolize column name
            new_col = sym(new_col_name)

            # Key variable names, apart from time value and version
            key_vars = setdiff(key(self$DT), c("time_value", "version"))
            
            # Computation for one group, one time value
            comp_one_grp = function(.data_group,
                                    f, ..., 
                                    time_value,
                                    key_vars,
                                    new_col) {
              # Carry out the specified computation 
              comp_value = f(.data_group, ...)

              # Count the number of appearances of the reference time value.
              # Note: ideally, we want to directly count occurrences of the ref
              # time value but due to latency, this will often not appear in the
              # data group. So we count the number of unique key values, outside 
              # of the time value column
              count = sum(!duplicated(.data_group[, key_vars]))

              # If we get back an atomic vector
              if (is.atomic(comp_value)) {
                if (length(comp_value) == 1) {
                  comp_value = rep(comp_value, count)
                }
                # If not a singleton, should be the right length, else abort
                else if (length(comp_value) != count) {
                  Abort("If the slide computation returns an atomic vector, then it must have a single element, or else one element per appearance of the reference time value in the local window.")
                }
              }

              # If we get back a data frame
              else if (is.data.frame(comp_value)) {
                if (nrow(comp_value) == 1) {
                  comp_value = rep(list(comp_value), count)
                }
                # If not a single row, should be the right length, else abort
                else if (nrow(comp_value) != count) {
                  Abort("If the slide computation returns a data frame, then it must have a single row, or else one row per appearance of the reference time value in the local window.")
                }
                # Make into a list
                else {
                  comp_value = split(comp_value, 1:nrow(comp_value))
                }
              }

              # If neither an atomic vector data frame, then abort
              else {
                Abort("The slide computation must return an atomic vector or a data frame.")
              }
 
              # Note that we've already recycled comp value to make size stable,
              # so tibble() will just recycle time value appropriately
              return(tibble::tibble(time_value = time_value, 
                                    !!new_col := comp_value))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              if (rlang::is_formula(f)) f = rlang::as_function(f)
              
              x = purrr::map_dfr(ref_time_values, function(ref_time_value) {
                self$as_of(ref_time_value,
                           min_time_value = ref_time_value - before) %>%
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, ..., 
                                      time_value = ref_time_value,
                                      key_vars = key_vars,
                                      new_col = new_col,
                                      .keep = TRUE) %>%
                  dplyr::ungroup()
              })
            }

            # Else interpret ... as an expression for tidy evaluation
            else {
              quos = enquos(...)
              if (length(quos) == 0) {
                Abort("If `f` is missing then a computation must be specified via `...`.")
              }
              if (length(quos) > 1) {
                Abort("If `f` is missing then only a single computation can be specified via `...`.")
              }
              
              quo = quos[[1]]
              f = function(x, quo, ...) rlang::eval_tidy(quo, x)
              new_col = sym(names(rlang::quos_auto_name(quos)))

              x = purrr::map_dfr(ref_time_values, function(ref_time_value) {
                self$as_of(ref_time_value,
                           min_time_value = ref_time_value - before) %>%
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, quo = quo,
                                      time_value = ref_time_value,
                                      key_vars = key_vars,
                                      new_col = new_col,
                                      .keep = TRUE) %>%
                  dplyr::ungroup()
              })
            }
            
            # Unnest if we need to
            if (!as_list_col) {
              x = tidyr::unnest(x, !!new_col, names_sep = names_sep)
            }
            
            # Join to get all rows, if we need to, then return
            if (all_rows) {
              cols = c(as.character(group_by), "time_value")
              y = unique(self$DT[, ..cols])
              x = dplyr::left_join(y, x, by = cols)
            }
            return(x)
          }
        )
      )
          
#' Convert to `epi_archive` format
#'
#' Converts a data frame, data table, or tibble into an `epi_archive`
#' object. See the [archive
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
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
#'   using the same version tag as the old update data), or (b) `NA`, to
#'   indicate that no versions are clobberable. There are a variety of reasons
#'   why versions could be clobberable, such as upstream hotfixes to the latest
#'   version, or delays in data synchronization that were mistaken for versions
#'   with no updates; potential causes vary between different data pipelines.
#'   The default value is `max_version_with_row_in(x)`; this default assumes
#'   that (i) if a row in `x` (even one that `compactify` would consider
#'   redundant) is present with version `ver`, then all previous versions must
#'   be finalized and non-clobberable, although `ver` (and onward) might still
#'   be modified, (ii) even if we have "observed" empty updates for some
#'   versions beyond `max(x$version)` (as indicated by `versions_end`;
#'   see below), we can't assume `max(x$version)` has been finalized, because we
#'   might see a nonfinalized version + empty subsequent versions due to
#'   upstream database replication delays in combination with the upstream
#'   replicas using last-version-carried-forward to extrapolate that there were
#'   no updates, (iii) "redundant" update rows that would be removed by
#'   `compactify` are not redundant, and actually come from an explicit version
#'   release that indicates that preceding versions are finalized. If `nrow(x)
#'   == 0`, then this argument is mandatory.
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
#'                        by = 1, length.out = 5), times = 2),
#'   version = rep(seq(as.Date("2020-01-02"), 
#'                     by = 1, length.out = 5), times = 2),
#'   value = rnorm(10, mean = 2, sd = 1)
#' )
#' 
#' toy_epi_archive <- tib %>% as_epi_archive(geo_type = "state", 
#'                                           time_type = "day")
#' toy_epi_archive 
#' 
#' # Ex. with an additional key for county
#' df <- data.frame (geo_value  = c(replicate(2, "ca"), replicate(2, "fl")),
#'                  county = c(1, 3, 2, 5),
#'                  time_value = c("2020-06-01",
#'                                 "2020-06-02",
#'                                 "2020-06-01",
#'                                 "2020-06-02"),
#'                  version = c("2020-06-02",
#'                              "2020-06-03",
#'                              "2020-06-02",
#'                              "2020-06-03"),
#'                  cases = c(1, 2, 3, 4),
#'                  cases_rate = c(0.01, 0.02, 0.01, 0.05))
#'
#' x <- df %>% as_epi_archive(geo_type = "state",
#'                            time_type = "day",
#'                            other_keys = "county")
as_epi_archive = function(x, geo_type, time_type, other_keys,
                          additional_metadata = list(),
                          compactify = NULL,
                          clobberable_versions_start = max_version_with_row_in(x),
                          versions_end = max_version_with_row_in(x)) {
  epi_archive$new(x, geo_type, time_type, other_keys, additional_metadata,
                  compactify, clobberable_versions_start, versions_end)
}

#' Test for `epi_archive` format
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from `epi_archive`.
#' 
#' @export
#' @examples
#' is_epi_archive(jhu_csse_daily_subset) # FALSE (this is an epi_df, not epi_archive)
#' is_epi_archive(archive_cases_dv_subset) # TRUE
is_epi_archive = function(x) {
  inherits(x, "epi_archive")
}
