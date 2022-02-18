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
#'   as well as any others (these can be specified when instantiating the
#'   `epi_archive` object via the `other_keys` argument, and/or set by operating
#'   on `DT` directly). There can only be a single row per unique combination of
#'   key variables, and thus the key variables are critical for figuring out how
#'   to generate a snapshot of data from the archive, as of a given version
#'   (also described below).
#' 
#' In general, last observation carried forward (LOCF) is used to data in
#'   between recorded versions. Currently, deletions must be represented as
#'   revising a row to a special state (e.g., making the entries `NA` or
#'   including a special column that flags the data as removed and performing
#'   some kind of post-processing), and the archive is unaware of what this
#'   state is.
#'
#' **A word of caution:** R6 objects, unlike most other objects in R, have
#'   reference semantics. A primary consequence of this is that objects are not
#'   copied when modified. You can read more about this in Hadley Wickham's
#'   [Advanced R](https://adv-r.hadley.nz/r6.html#r6-semantics) book.
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
#' @export
epi_archive =
  R6::R6Class(
        classname = "epi_archive",
        #####
        public = list(
          DT = NULL,
          geo_type = NULL,
          time_type = NULL,
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
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version".
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_archive` object. The metadata will have `geo_type` and `time_type`
#'   fields; named entries from the passed list or will be included as well.
#' @return An `epi_archive` object.
#' @importFrom data.table as.data.table key setkeyv
#' @importFrom rlang abort warn
          initialize = function(x, geo_type, time_type, other_keys,
                                additional_metadata) {  
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
                    c("geo_type", "time_type"))) {
              warn("`additional_metadata` names overlap with existing metadata fields \"geo_type\", \"time_type\".")
            }
            
            # Create the data table; if x was an un-keyed data.table itself,
            # then the call to as.data.table() will fail to set keys, so we
            # need to check this, then do it manually if needed
            key_vars = c("geo_value", "time_value", other_keys, "version")
            DT = as.data.table(x, key = key_vars)
            if (!identical(key_vars, key(DT))) setkeyv(DT, cols = key_vars)

            # Instantiate all self variables
            self$DT = DT
            self$geo_type = geo_type
            self$time_type = time_type
            self$additional_metadata = additional_metadata
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
            cat(sprintf("* %-14s = %s\n", "min time value",
                        min(self$DT$time_value)))
            cat(sprintf("* %-14s = %s\n", "max time value",
                        max(self$DT$time_value)))
            cat(sprintf("* %-14s = %s\n", "min version",
                        min(self$DT$version)))
            cat(sprintf("* %-14s = %s\n", "max version",
                        max(self$DT$version)))
            cat("----------\n")
            cat(sprintf("Data archive (stored in DT field): %i x %i\n", 
                        nrow(self$DT), ncol(self$DT)))
            cat("----------\n")
            cat(sprintf("Public methods: %s",
                        paste(names(epi_archive$public_methods),
                              collapse = ", ")))
          },
          #####
#' @description Generates a snapshot in `epi_df` format as of a given version.
#' @param max_version Time value specifying the max version to permit in the
#'   snapshot. That is, the snapshot will comprise the unique rows of the
#'   current archive data that represent the most up-to-date signal values, as
#'   of the specified `max_version` (and whose time values are at least
#'   `min_time_value`.)
#' @param min_time_value Time value specifying the min time value to permit in
#'   the snapshot. Default is `-Inf`, which effectively means that there is no
#'   minimum considered.
#' @return An `epi_df` object.
#' @importFrom data.table between key
#' @importFrom rlang .data abort warn
          as_of = function(max_version, min_time_value = -Inf) {
            # Self max version and other keys
            self_max = max(self$DT$version)
            other_keys = setdiff(key(self$DT),
                                 c("geo_value", "time_value", "version"))
            if (length(other_keys) == 0) other_keys = NULL
            
            # Check a few things on max_version
            if (!identical(class(max_version), class(self$DT$version))) {
              abort("`max_version` and `DT$version` must have same class.")
            }
            if (length(max_version) != 1) {
              abort("`max_version` cannot be a vector.")
            }
            if (max_version > self_max) {
              abort("`max_version` must be at most `max(DT$max_version)`.")
            }
            if (max_version == self_max) {
              warn("Getting data as of the latest version possible. For a variety of reasons, it is possible that we only have a preliminary picture of this version (e.g., the upstream source has updated it but we have not seen it due to latency in synchronization). Thus, the snapshot that we produce here might not be reproducible at a later time (e.g., when the archive has caught up in terms of synchronization).")
            }
            
            # Filter by version and return
            return(
              # Make sure to use data.table ways of filtering and selecting 
              self$DT[between(time_value,
                              min_time_value,
                              max_version) &
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
#' @description Merges another `data.table` with the current one, and allows for
#'   a post-filling of `NA` values by last observation carried forward (LOCF).
#' @param y A `data.table` to join to the current one. This can instead be an
#'   `epi_archive` object, in which case its underlying data table is joined.
#' @param ... Named arguments to pass to `data.table::merge.data.table()`, which
#'   is used for the join (with all default settings as in this function). For
#'   example, passing `all = TRUE` will perform a full join.
#' @param locf Should LOCF be used after joining on all non-key columns? This
#'   will take the latest version of each signal value and propogate it forward
#'   to fill in gaps that appear after merging. Default is `TRUE`.
#' @param nan Should `NaN` values be treated as `NA` values in the post-filling
#' step?  Default is `NA`, which means that they are treated as `NA` values; if
#    `NaN`, then they are treated as distinct.
#' @return Nothing (the underlying data table overwritten with the merged one).
#' @importFrom data.table key merge.data.table nafill
#' @importFrom rlang abort 
          merge = function(y, ..., locf = TRUE, nan = NA) {
            # Check we have a `data.table` object
            if (!(inherits(y, "data.table") || inherits(y, "epi_archive"))) {
              abort("`y` must be of class `data.table` or `epi_archive`.") 
            }

            # Use the data.table merge function, carrying through ... args
            if (inherits(y, "data.table")) self$DT = merge(self$DT, y, ...)
            else self$DT = merge(self$DT, y$DT, ...)

            # Now use the data.table locf function, if we're asked to
            if (locf) {
              key_vars = key(self$DT)
              cols = setdiff(names(self$DT), key_vars)
              by = setdiff(key_vars, "version")

              # Important: use nafill and not setnafill because the latter
              # returns the entire data frame by reference, and the former can
              # be set to act on particular columns by reference using := 
              self$DT[,
              (cols) := nafill(.SD, type = "locf", nan = nan),        
              .SDcols = cols, 
              by = by]
            }
          },   
          #####
#' @description Slides a given function over variables in an `epi_archive`
#'   object. Windows are **always right-aligned**, unlike `epi_slide()`. The
#'   other arguments are just as in `epi_slide()`. The exception is the `by`
#'   argument, which used to specify the grouping upfront (whereas in an
#'   `epi_df`, this would be accomplished by a call to `dplyr::group_by()` that
#'   precedes a call to `epi_df()`). See the archive vignette for examples.
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
#'   `n = 7`, and one time step is one day, then to produce a value on January 7
#'   we apply the given function or formula to data in between January 1 and
#'   7. Default is 7.
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
#' @param complete Should the slide function be run over complete windows only?
#'   (A complete window has distance `n-1` between its left and right
#'   endpoints.) Default is `FALSE`, which allows for partial computations. 
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
#' @param all_rows If `all_rows = TRUE`, then there will be one row per
#'   combination of grouping variables and unique time values in output. the rows corresponding to
#'   unique time values  the underlying
#'   data table will be kept
#'   in the output; otherwise, there will be one row in the output for each time
#'   value in `x` that acts as a reference time value. Default is `FALSE`.
#' @return A tibble with whose columns are the grouping variables; `time_value`,
#'   containing the reference time values for the slide computation; and a new
#'   column named according to the `new_col_name` argument, containing the slide
#'   values. 
#' @importFrom data.table key
#' @importFrom rlang !! !!! abort enquo enquos sym syms
          slide = function(f, ..., n = 7, group_by, ref_time_values, 
                           time_step, complete = FALSE,
                           new_col_name = "slide_value",
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
              
            # If a custom time step is specified, then redefine units 
            before_num = n-1
            if (!missing(time_step)) before_num = time_step(n-1)
            
            # What to group by? If missing, set according to internal keys
            if (missing(group_by)) {
              group_by = setdiff(key(self$DT), c("time_value", "version"))
            }
            
            # Symbolize column name, defuse grouping variables
            new_col = sym(new_col_name)
            group_by = syms(names(eval_select(enquo(group_by), self$DT)))
            
            # Computation for one group, one time value
            comp_one_grp = function(.data_group,
                                    f, ..., n,
                                    time_value,
                                    complete, 
                                    new_col) {
              # Check if we need a complete window (max - min = n-1)
              if (complete && diff(range(.data_group$time_value)) < n-1) {
                comp_values = NA
              }
              # Else carry out the specified computation and return 
              else comp_values = f(.data_group, ...)
              return(tibble::tibble(time_value = time_value,
                                    !!new_col := comp_values))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              x = purrr::map_dfr(ref_time_values, function(t) {
                self$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>% 
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, ..., n = n,
                                      time_value = t,
                                      complete = complete,
                                      new_col = new_col) %>%
                  dplyr::ungroup()
              })
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
              new_col = sym(names(rlang::quos_auto_name(quos)))

              x = purrr::map_dfr(ref_time_values, function(t) {
                self$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>% 
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, quo = quo, n = n,
                                      time_value = t,
                                      complete = complete,
                                      new_col = new_col) %>%
                  dplyr::ungroup()
              })
            }

            # If we're asked for all rows, then do a join
            if (all_rows) {
            }
            
            # Unnest if we need to, and return
            if (!as_list_col && is.list(dplyr::pull(x, !!new_col))) {
              x = tidyr::unnest(x, !!new_col, names_sep = names_sep)
            }
            return(x)
          }
        )
      )
          
#' Convert to `epi_archive` format
#'
#' Converts a data frame, data table, or tibble into an `epi_archive`
#' object. See the archive vignette for examples.
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
#' @return An `epi_archive` object.
#'
#' @details This is a simple wrapper around the `new()` method of the
#'   `epi_archive` class, so, for example:
#'   ```
#'   x <- as_epi_archive(df, geo_type = "state", time_type = "day")
#'   ```
#'   would be equivalent to:
#'   ```
#'   x <- epi_archive$new(df, geo_type = "state", time_type = "day")
#'   ```
#'
#' @export
as_epi_archive = function(x, geo_type, time_type, other_keys,
                          additional_metadata = list()) { 
  epi_archive$new(x, geo_type, time_type, other_keys, additional_metadata) 
}
