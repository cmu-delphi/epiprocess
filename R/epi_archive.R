# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable.aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable.aware = TRUE

#' @importFrom data.table as.data.table key setkey
#' @importFrom dplyr filter select
#' @importFrom rlang .data abort warn
#' @noRd
epi_archive =
  R6::R6Class(
        "epi_archive",
        #####
        list(
          dt = NULL,
          geo_type = NULL,
          time_type = NULL,
          max_version = NULL,
          other_keys = NULL,
          additional_metdata = NULL,
          initialize = function(x, geo_type, time_type, max_version,
                                other_keys = NULL,
                                additional_metadata = list()) {
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
            
            # Check a couple things about key variables
            if (!all(other_keys %in% names(x))) {
              abort("`other_keys` must be contained in the column names of `x`.")
            }
            if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
              abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
            }
            
            # Create the data table; if x was an un-keyed data.table itself,
            # then the call to as.data.table() will fail to set keys, so we
            # need to check this, then do it manually if needed
            key_vars = c("geo_value", "time_value", other_keys, "version")
            dt = as.data.table(x, key = key_vars)
            if (key(dt) != key_vars) setkey(dt, key_vars)

            # Instantiate all self variables
            self$dt = dt
            self$geo_type = geo_type
            self$time_type = time_type
            self$max_version = max_version
            self$other_keys = other_keys
            self$additional_metadata = additional_metadata
          },
          #####
          as_of = function(version, n = Inf, as_epi_df = TRUE) {
            # Check a few things on version
            if (!identical(class(version), class(self$dt$version))) {
              abort("`version` and `dt$version` must have same class.")
            }
            if (length(version) != 1) {
              abort("`version` cannot be a vector.")
            }
            if (version > self$max_version) {
              abort("`version` must be at most `max_version`.")
            }
            if (version == self$max_version) {
              warn("Getting data as of the latest version possible. For a variety of reasons, it is possible that we only have a preliminary picture of this version (e.g., the upstream source has updated it but we have not seen it due to latency in synchronization). Thus, the `epi_df` snapshot that we produce here might not be reproducible at a later time (e.g., when the archive has caught up in terms of synchronization).")
            }

            x = self$dt %>%
              filter(data.table::between(.data$time_value,
                                         version - n, version)) %>% 
              filter(.data$version <= version) 

            # If no epi_df, then just return x
            if (!as_epi_df) return(x)

            # Else form and return the epi_df
            else {
              return(
                x %>% 
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
          },
          #####
          versions_with_updates = function() {
            return(unique(self$dt$version))
          }
        )
      )

#' Create an `epi_archive` object
#'
#' Creates an `epi_archive` object from given `geo_value`, `time_value`, and
#' `version` variables, as well as any additional number of variables.
#'
#' @param geo_value Geographic values associated with the measurements.
#' @param time_value Time values associated with the measurements.
#' @param version Time values specifying the versions of the measurements. For
#'   example, if in a given row the `version` is January 15, 2022 and
#'   `time_value` is January 14, 2022, then this row contains the measurements
#'   of the data for January 14, 2022 that were available one day later.
#' @param ... Additional arguments of the form `value` or `name = value`, which
#'   specify any number of additional columns for the `epi_archive` object.
#' @param other_keys Character vector specifying the names of variables that
#'   should be considered key variables (in the language of `data.table`) apart
#'   from "geo_value", "time_value", and "version". There can only be a single
#'   row per unique combination of key variables, and thus the key variables are
#'   critical for figuring out how to generate snapshots of data form the
#'   archive, as of certain versions.
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `time_type` and `geo_type`
#'   fields; named entries from the passed list or will be included as well.
#' @return An `epi_archive` object.
#'
#' @details Last-observation-carried-forward (LOCF) is used to data in between 
#'   recorded versions. Currently, deletions must be represented as revising a
#'   row to a special state (e.g., making the entries `NA` or including a
#'   special column that flags the data as removed and performing some kind of 
#'   post-processing), and the archive is unaware of what this state is.
#'
#' TODO some details on the structure of the R6 object and the fact that it
#'   obeys reference semantics?? 
#'
#' @export
epi_archive = function(geo_value, time_value, version, ..., geo_type, time_type,
                       max_version, other_keys, additional_metadata = list()) {
  x = data.table::data.table(geo_value, time_value, version, ...)
  return(epi_archive$initialize(x, geo_type, time_type, max_version, other_keys,
                                additional_metadata))
}

#' Convert to `epi_archive` object
#'
#' Converts a data frame, tibble, or data table into an `epi_archive` object,
#' allowing for fast querying of data snapshots into `epi_df` format as of
#' certain versions. See the [data versioning
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html) for
#' examples.
#'
#' @export
as_epi_archive = function(x, geo_type, time_type, max_version, other_keys, 
                          additional_metadata = list()) {
  return(epi_archive$initialize(x, geo_type, time_type, max_version, other_keys,
                                additional_metadata))
}
