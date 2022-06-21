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
#'   to generate a snapshot of data from the archive, as of a given version.
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
#'   `as_of()` method for an `epi_archive` object `x`. More details on this
#'   method are documented in the wrapper function `epix_as_of()`.
#'
#' @section Sliding Computations:
#' We can run a sliding computation over an `epi_archive` object, much like
#'   `epi_slide()` does for an `epi_df` object. This is accomplished by calling
#'   the `slide()` method for an `epi_archive` object, which works similarly to
#'   the way `epi_slide()` works for an `epi_df` object, but with one key
#'   difference: it is version-aware. That is, for an `epi_archive` object, the
#'   sliding computation at any given reference time point t is performed on
#'   **data that would have been available as of t**. More details on `slide()`
#'   are documented in the wrapper function `epix_slide()`.
#' 
#' @importFrom R6 R6Class
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
#' @param compactify Optional, Boolean: should we remove rows that are
#'   considered redundant for the purposes of `epi_archive`'s built-in methods
#'   such as `$as_of`? As these methods use the last (version of an)
#'   observation carried forward (LOCF) to interpolate between the version data
#'   provided, rows that won't change these LOCF results can potentially be
#'   omitted to save space. Generally, this can be set to `TRUE`, but if you
#'   directly inspect or edit the fields of the `epi_archive` such as the `$DT`,
#'   you will have to determine whether `compactify=TRUE` will still produce
#'   equivalent results.
#' @return An `epi_archive` object.
#' @importFrom data.table as.data.table key setkeyv
          initialize = function(x, geo_type, time_type, other_keys,
                                additional_metadata, compactify) {
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
            
            # Finish off with compactify
            if (missing(compactify)) {
              compactify = NULL
            } else if (!rlang::is_bool(compactify) &&
                        !rlang::is_null(compactify)) {
              Abort("compactify must be boolean or null.")
            } 
            # Create the data table; if x was an un-keyed data.table itself,
            # then the call to as.data.table() will fail to set keys, so we
            # need to check this, then do it manually if needed
            key_vars = c("geo_value", "time_value", other_keys, "version")
            DT = as.data.table(x, key = key_vars)
            if (!identical(key_vars, key(DT))) setkeyv(DT, cols = key_vars)
            
            # Checks to see if a value in a vector is LOCF
            is_locf <- function(vec) {
              dplyr::if_else(!is.na(vec) & !is.na(dplyr::lag(vec)),
                     vec == lag(vec),
                     is.na(vec) & is.na(dplyr::lag(vec)))
            }
            
            # LOCF is defined by a row where all values except for the version
            # differ from their respective lag values
            
            # Checks for LOCF's in a data frame
            rm_locf <- function(df) {
             filter(df,if_any(c(everything(),-version),~ !is_locf(.))) 
            }
            
            # Keeps LOCF values, such as to be printed
            keep_locf <- function(df) {
              filter(df,if_all(c(everything(),-version),~ is_locf(.))) 
            }
            
            # Runs compactify on data frame
            if (is.null(compactify) || compactify == TRUE) {
              elim = keep_locf(DT)
              DT = rm_locf(DT)
            } else {
              # Create empty data frame for nrow(elim) to be 0
              elim = tibble()
            }
            
            # Warns about redundant rows
            if (is.null(compactify) && nrow(elim) > 0) {
              warning_intro <- paste("LOCF rows found;",
                                     "these have been removed: \n")
              
              # elim size capped at 6
              len <- nrow(elim)
              elim <- elim[1:min(6,len),]
              
              warning_data <- paste(collapse="\n",capture.output(print(elim)))
              
              warning_message <- paste(warning_intro,warning_data)
              if (len > 6) {
                warning_message <- paste0(warning_message,"\n",
                                          "Only the first 6 LOCF rows are ",
                                          "printed. There are more than 6 LOCF",
                                          " rows.")
              }
              
              warning_message <- paste0(warning_message,"\n",
                                        "To disable warning but still remove ",
                                        "LOCF rows, set compactify=FALSE.")
              
              rlang::warn(warning_message)
            }
            
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
#'   See the documentation for the wrapper function `epix_as_of()` for details.
#' @importFrom data.table between key
          as_of = function(max_version, min_time_value = -Inf) {
            # Self max version and other keys
            self_max = max(self$DT$version)
            other_keys = setdiff(key(self$DT),
                                 c("geo_value", "time_value", "version"))
            if (length(other_keys) == 0) other_keys = NULL
            
            # Check a few things on max_version
            if (!identical(class(max_version), class(self$DT$version))) {
              Abort("`max_version` and `DT$version` must have same class.")
            }
            if (length(max_version) != 1) {
              Abort("`max_version` cannot be a vector.")
            }
            if (max_version > self_max) {
              Abort("`max_version` must be at most `max(DT$max_version)`.")
            }
            if (max_version == self_max) {
              Warn("Getting data as of the latest version possible. For a variety of reasons, it is possible that we only have a preliminary picture of this version (e.g., the upstream source has updated it but we have not seen it due to latency in synchronization). Thus, the snapshot that we produce here might not be reproducible at a later time (e.g., when the archive has caught up in terms of synchronization).")
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
#'   See the documentation for the wrapper function `epix_merge()` for details.
#' @importFrom data.table key merge.data.table nafill
          merge = function(y, ..., locf = TRUE, nan = NA) {
            # Check we have a `data.table` object
            if (!(inherits(y, "data.table") || inherits(y, "epi_archive"))) {
              Abort("`y` must be of class `data.table` or `epi_archive`.") 
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
#'   object. See the documentation for the wrapper function `epix_as_of()` for
#'   details. 
#' @importFrom data.table key
#' @importFrom rlang !! !!! enquo enquos is_quosure sym syms
          slide = function(f, ..., n = 7, group_by, ref_time_values, 
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
              
            # If a custom time step is specified, then redefine units 
            before_num = n-1
            if (!missing(time_step)) before_num = time_step(n-1)
            
            # What to group by? If missing, set according to internal keys
            if (missing(group_by)) {
              group_by = setdiff(key(self$DT), c("time_value", "version"))
            }
            
            # Symbolize column name, defuse grouping variables. We have to do
            # the middle step here which is a bit complicated (unfortunately)
            # since the function epix_slide() could have called the current one,
            # and in doing so, it may have already needed to defuse the grouping
            # variables
            new_col = sym(new_col_name)
            if (!is_quosure(group_by)) group_by = enquo(group_by)
            group_by = syms(names(eval_select(group_by, self$DT)))

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
              
              x = purrr::map_dfr(ref_time_values, function(t) {
                self$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>% 
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, ..., 
                                      time_value = t,
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

              x = purrr::map_dfr(ref_time_values, function(t) {
                self$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>% 
                  dplyr::group_by(!!!group_by) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, quo = quo,
                                      time_value = t,
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
#' @param compactify By default, removes LOCF rows and warns the user about
#'   them. Optionally, one can input a Boolean: TRUE eliminates LOCF rows,
#'   while FALSE keeps them.
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
#' df <- data.frame (geo_value  = c(replicate(2, "ca"), replicate(2, "fl")),
#'                  county = c(1, 3, 2, 5),
#'                  time_value = c("2020-06-01",
#'                  "2020-06-02",
#'                  "2020-06-01",
#'                  "2020-06-02"),
#'                  version = c("2020-06-02",
#'                  "2020-06-03",
#'                  "2020-06-02",
#'                  "2020-06-03"),
#'                  cases = c(1, 2, 3, 4),
#'                  cases_rate = c(0.01, 0.02, 0.01, 0.05))
#'
#' x <- df %>% as_epi_archive(geo_type = "state",
#'                           time_type = "day",
#'                           other_keys = "county")
as_epi_archive = function(x, geo_type, time_type, other_keys,
                          additional_metadata = list(),compactify = NULL) {
  epi_archive$new(x, geo_type, time_type, other_keys, additional_metadata,
                  compactify)
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
