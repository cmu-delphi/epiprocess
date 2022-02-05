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
#'   data from the archive, as of a given version (also described below). 
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
#'   accessed (and altered) directly, as in `x$geo_type` or `x$time_type`, etc. 
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
#' @section Sliding Computations:
#' We can run a sliding computation over an `epi_archive` object, much like
#'   `epi_slide()` does for an `epi_df` object. This is accomplished by calling
#'   the `slide()` method for an `epi_archive` object, which works similarly to
#'   the way `epi_slide()` works for an `epi_df` object, but with one key
#'   difference: it is version-aware. That is, for an `epi_archive` object, the
#'   sliding computation at any given reference time point t is performed on
#'   **data that would have been available as of t**. More details on `slide()`
#'   are documented below.
#' 
#' @seealso [as_epi_archive()] for converting to `epi_archive` format
#' @importFrom data.table as.data.table key setkey
#' @importFrom dplyr filter select
#' @importFrom rlang .data abort warn
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
          print = function() {
            cat("An `epi_archive` object, with metadata:\n")
            cat(sprintf("* %-12s= %s\n", "geo_type", self$geo_type))
            cat(sprintf("* %-12s= %s\n", "time_type", self$time_type))
            cat(sprintf("* %-12s= %s\n", "max_version", self$max_version))
            if (!is.null(self$other_keys)) {
              cat(sprintf("* %-12s= %s\n", "other_keys", self$other_keys))
            }
            if (!is.null(self$additional_metadata)) { 
              sapply(self$additional_metadata, function(m) { 
                cat(sprintf("* %-12s= %s\n", names(m), m))
              })
            }
            cat("\n")
            cat(sprintf("Data archive (stored in DT field): %i x %i", 
                        nrow(self$DT), ncol(self$DT)))
            cat("\n")
            cat(sprintf("Public methods: %s",
                        paste(names(epi_archive$public_methods),
                              collapse = ", ")))
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
#' @importFrom rlang .data 
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
              filter(.data$version <= max_version) %>% 
              unique(by = c("geo_value", "time_value", self$other_keys),
                     fromLast = TRUE) %>%
              select(-.data$version) %>%
              tibble::as_tibble() %>% 
              as_epi_df(geo_type = self$geo_type,
                        time_type = self$time_type,
                        as_of = max_version,
                        additional_metadata = c(self$additional_metadata,
                                                other_keys = self$other_keys))
            )
          },
#' @description Slides a given function over variables in an `epi_archive`
#'   object. Windows are *always right-aligned*, unlike `epi_slide()`. The other
#'   arguments are as in `epi_slide()`, and its documentation gives more details
#'   on their useage. See also the [archive
#'   vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html)
#'   for examples.
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
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 5`, one time step is one day, and the alignment is "right", then to 
#'   produce a value on November 5, we apply the given function or formula to
#'   data in between November 1 and 5. Default is 14. 
#' @param complete Should the slide function be run over complete windows only?
#'   Default is `FALSE`, which allows for computation on partial windows.  
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. This function must take a positive integer and return an object
#'   of class `lubridate::period`. For example, we can use `time_step =
#'   lubridate::hours` in order to set the time step to be one hour (this would
#'   only be meaningful if `time_value` is of class `POSIXct`, that is, if
#'   `time_type` is "day-time").
#' @param by The variable(s) to group by before slide computation. If missing,
#'   then `geo_value` and the variable names in the `other_keys` field of the
#'   `epi_archive` object will be used for grouping.
#' @return A tibble with the grouping variables, `time_value`, and a new column
#'   named according to the `new_col_name` argument, with the slide values.
#' @importFrom dplyr group_by group_modify mutate select
#' @importFrom lubridate days weeks
#' @importFrom rlang !! enquo enquos 
          slide = function(f, ..., n = 14, complete = FALSE,
                           new_col_name = "slide_value", time_step, by) {
            # What is one time step?
            if (!missing(time_step)) before_fun = time_step
            else if (self$time_type == "week") before_fun = weeks
            else before_fun = days # For time_type = "day" or "day-time"
            before_num = before_fun(n-1)
            
            # What to group by? If missing, set according to internal keys
            if (missing(by)) by = c("geo_value", other_keys)
            by = enquo(by)

            # Computation for just one group
            comp_one_grp = function(.data_group,
                                    ...,
                                    f,
                                    complete,
                                    min_time_value,
                                    new_col_name) {
              # Check if we don't have a complete window, and if we needed
              # to have one, then return NA
              if (complete && min(.data_group$time_value > min_time_value)) {
                comp_value = NA
              }
              else comp_value = f(.data_group, ...)
              return(mutate(.data_group, !!new_col_name := comp_value))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              return(
                purrr::map_dfr(self$DT$time_value, function(t) {
                  self$as_of(t, min_time_value = t - before_num) %>%
                    tibble::as_tsibble() %>% 
                    group_by(!!by) %>%
                    group_modify(comp_one_grp,
                                 f = f,
                                 ...,
                                 complete = complete,
                                 min_time_value = t - before_num,
                                 new_col_name = new_col_name) %>%
                    select(!!by, time_value, !!new_col_name)
                })
              )
            }

            # Else interpret ... as an expression for tidy evaluation
            else {
              quos = enquos(...)
              if (length(quos) == 0) {
                abort("If `f` is missing then a computation must be specified via `...`.")
              }
              if (length(quos) > 1) {
                abort("If `f` is missing then only a single computation can be specified via `...`.")
              }
              
              quo = quos[[1]]
              f = function(x, quo, ...) rlang::eval_tidy(quo, x)
              new_col_name = names(rlang::quos_auto_name(quos))

              return(
                purrr::map(self$DT$time_value, function(t) {
                  self$as_of(t, min_time_value = t - before_num) %>%
                    tibble::as_tsibble() %>% 
                    group_by(!!by) %>%
                    group_modify(comp_one_grp,
                                 f = f,
                                 quo = quo,
                                 complete = complete,
                                 min_time_value = t - before_num,
                                 new_col_name = new_col_name) %>%
                    select(!!by, time_value, !!new_col_name)
                })
              )
            }
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
#' @seealso [`epi_archive`][epi_archive] for more details on the `epi_archive`
#'   format 
#' @export
as_epi_archive = function(x, geo_type, time_type, max_version, other_keys,
                          additional_metadata = list()) {
  epi_archive$new(x, geo_type, time_type, max_version, other_keys,
                  additional_metadata) 
}
