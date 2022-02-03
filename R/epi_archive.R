# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable.aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable.aware = TRUE

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
#'   as well as any other specified in the metadata (described below). There can
#'   only be a single row per unique combination of key variables, and thus the
#'   key variables are critical for figuring out how to generate a snapshot of
#'   data from the archive, as of a given version.
#' 
#' In general, last-observation-carried-forward (LOCF) is used to data in
#'   between recorded versions. Currently, deletions must be represented as
#'   revising a row to a special state (e.g., making the entries `NA` or
#'   including a special column that flags the data as removed and performing
#'   some kind of post-processing), and the archive is unaware of what this
#'   state is.
#'
#' A word of caution: R6 objects, unlike most other objects in R, have reference
#'   semantics. A primary consequence of this is that objects are not copied
#'   when modified. You can read more about this in Hadley Wickham's [Advanced 
#'   R](https://adv-r.hadley.nz/r6.html#r6-semantics) book.
#' 
#' @section Metadata:
#' The following pieces of metadata are included as fields in an `epi_archive`
#'   object:
#'
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `max_version`: the max version in the data archive.
#' * `other_keys`: the names of the other key variables (apart from "geo_value",
#'   "time_value", and "version") in the data archive.
#' * `additional_metadata`: list of additional metadata for the data archive.
#'
#' Unlike an `epi_df` object, metadata for an `epi_archive` object `x` can be
#'   accessed (and altered) directly via `x$geo_type`.
#'
#' @section Generating Snapshots:
#' An `epi_archive` object can be used to generate a snapshot of the data in
#'   `epi_df` format, which represents the most up-to-date values of the signal
#'   variables, as of the specified version. This is accomplished by calling the 
#'   `as_of()` method for an `epi_archive` object `x`, for example:
#'   ```
#'   x$as_of(as.Date("2022-01-15"))
#'   ```
#'   to generate a snapshot as of January 15, 2022. More details on the
#'   `as_of()` method are documented below. 
#' 
#' @importFrom data.table as.data.table key setkey
#' @importFrom dplyr filter select
#' @importFrom rlang .data abort warn
#' @seealso [as_epi_archive()] for converting to `epi_archive` format
#' @export
epi_archive =
  R6::R6Class(
        classname = "epi_archive",
        #####
        public = list(
          DT = NULL,
          geo_type = NULL,
          time_type = NULL,
          max_version = NULL,
          other_keys = NULL,
          additional_metadata = NULL,
#' @description Creates a new `epi_archive` object.
#' @param x A data frame, data table, or tibble, with columns `geo_value`,
#'   `time_value`, `version`, and then any additional number of columns. 
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param max_version Maximum version in the data archive. If missing, then the
#'   function will set it equal to the maximum of the `version` column in `x`.
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `geo_type` and `time_type`
#'   fields; named entries from the passed list or will be included as well.
#' @return An `epi_archive` object.
          initialize = function(x, geo_type, time_type, max_version,
                                other_keys, additional_metadata) {
            # Check that we have a data frame
            if (!is.data.frame(x)) {
              abort("`x` must be a data frame.")
            }
                  
            # Check that we have geo_value, time_value, version columns
            if (!("geo_value" %in% names(x))) {
              abort("`x` must contain a `geo_value` column.")
            }
            if (!("time_value" %in% names(x))) {
              abort("`x` must contain a `time_value` column.")
            }
            if (!("version" %in% names(x))) {
              abort("`x` must contain a `version` column.")
            }
              
            # If geo type is missing, then try to guess it
            if (missing(geo_type)) {
              geo_type = guess_geo_type(x$geo_value)
            }

            # If time type is missing, then try to guess it
            if (missing(time_type)) {
              time_type = guess_time_type(x$time_value)
            }

            # Check several things about max version
            if (missing(max_version)) {
              max_version = max(x$version)
            }
            if (!identical(class(max_version), class(x$version))) {
              abort("`max_version` and `x$version` must have same class.")
            }
            if (length(max_version) > 1) {
              abort("`max_version` cannot be a vector.")
            }
            if (max_version < max(x$version)) {
              abort("`max_version` must be at least `max(x$version)`.")
            }
            
            # Finish off with small checks on keys variables and metadata
            if (missing(other_keys)) other_keys = NULL
            if (missing(additional_metadata)) additional_metadata = list()
            if (!all(other_keys %in% names(x))) {
              abort("`other_keys` must be contained in the column names of `x`.")
            }
            if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
              abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
            }
            if (any(names(additional_metadata) %in%
                    c("geo_type", "time_type", "max_version", "other_keys"))) {
              warn("`additional_metadata` names overlap with existing metadata fields \"geo_type\", \"time_type\", \"max_version\", and \"other_keys\".")
            }
            
            # Create the data table; if x was an un-keyed data.table itself,
            # then the call to as.data.table() will fail to set keys, so we
            # need to check this, then do it manually if needed
            key_vars = c("geo_value", "time_value", other_keys, "version")
            DT = as.data.table(x, key = key_vars)
            if (!identical(key_vars, key(DT))) setkey(DT, key_vars)

            # Instantiate all self variables
            self$DT = DT
            self$geo_type = geo_type
            self$time_type = time_type
            self$max_version = max_version
            self$other_keys = other_keys
            self$additional_metadata = additional_metadata
          },
          #####
#' @description Generates a snapshot in `epi_df` format as of a given version.
#' @param max_version Time value specifying the max version to permit in the
#'   snapshot. That is, the snapshot will comprise the unique rows that
#'   represent the most up-to-date signal values, as of the specified
#'   `max_version`, whose time values are at least `min_time_value`.
#' @param min_time_value Time value specifying the min time value to permit in
#'   the snapshot. Default is `-Inf`, which effectively means that there is no
#'   minimum considered.
#' @return An `epi_df` object.
          as_of = function(max_version, min_time_value = -Inf) {
            # Check a few things on max_version
            if (!identical(class(max_version), class(self$DT$version))) {
              abort("`max_version` and `DT$version` must have same class.")
            }
            if (length(max_version) != 1) {
              abort("`max_version` cannot be a vector.")
            }
            if (max_version > self$max_version) {
              abort("`max_version` must be at most `max_version`.")
            }
            if (max_version == self$max_version) {
              warn("Getting data as of the latest version possible. For a variety of reasons, it is possible that we only have a preliminary picture of this version (e.g., the upstream source has updated it but we have not seen it due to latency in synchronization). Thus, the snapshot that we produce here might not be reproducible at a later time (e.g., when the archive has caught up in terms of synchronization).")
            }

            return(
              self$DT %>%
              filter(data.table::between(time_value,
                                         min_time_value,
                                         max_version)) %>%
              filter(.data$version <= version) %>% 
              unique(by = c("geo_value", "time_value", self$other_keys),
                     fromLast = TRUE) %>%
              select(-.data$version) %>%
              tibble::as_tibble() %>% 
              as_epi_df(geo_type = self$geo_type,
                        time_type = self$time_type,
                        as_of = version,
                        additional_metadata = c(self$additional_metadata,
                                                other_keys = self$other_keys))
            )
          }
        )
      )

#' Convert to `epi_archive` format
#'
#' Converts a data frame, data table, or tibble into an `epi_archive`
#' object. See the [data versioning
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
#' @param max_version Maximum version in the data archive. If missing, then the
#'   function will set it equal to the maximum of the `version` column in `x`.
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `geo_type` and `time_type`
#'   fields; named entries from the passed list or will be included as well.
#' @return An `epi_archive` object.
#'
#' @export
as_epi_archive = function(x, geo_type, time_type, max_version, other_keys,
                          additional_metadata = list()){
  return(epi_archive$new(x, geo_type, time_type, max_version, other_keys,
                         additional_metadata))
}
