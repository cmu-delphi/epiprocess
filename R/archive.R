# We use special features of data.table's `[`. The data.table package has a
# compatibility feature that disables some/all of these features if it thinks we
# might expect `data.frame`-compatible behavior instead. We can signal that we
# want the special behavior via `.datatable_aware = TRUE` or by importing any
# `data.table` package member. Do both to prevent surprises if we decide to use
# `data.table::` everywhere and not importing things.
.datatable_aware <- TRUE


#' Validate a version bound arg
#'
#' Expected to be used on `clobberable_versions_start`, `versions_end`, and
#' similar arguments. Some additional context-specific checks may be needed.
#' Side effects: raises an error if version bound appears invalid.
#'
#' @param version_bound the version bound to validate
#' @param x a data frame containing a version column with which to check
#'   compatibility
#' @param na_ok Boolean; is `NA` an acceptable "bound"? (If so, `NA` will
#'   have a special context-dependent meaning.)
#' @param version_bound_arg optional string; what to call the version bound in
#'   error messages
#'
#' @keywords internal
validate_version_bound <- function(version_bound, x, na_ok = FALSE,
                                   version_bound_arg = rlang::caller_arg(version_bound),
                                   x_arg = rlang::caller_arg(x)) {
  if (is.null(version_bound)) {
    cli_abort(
      "{version_bound_arg} cannot be NULL",
      class = "epiprocess__version_bound_null"
    )
  }
  if (length(version_bound) != 1L) {
    cli_abort(
      "{version_bound_arg} must have length of 1",
      class = "epiprocess__version_bound_wrong_length"
    )
  }
  if (is.na(version_bound)) {
    if (!na_ok) {
      cli_abort(
        "{version_bound_arg} cannot be NA",
        class = "epiprocess__version_bound_na_with_na_not_okay"
      )
    }
  } else {
    if (!identical(class(version_bound), class(x[["version"]]))) {
      cli_abort(
        "{version_bound_arg} must have the same `class` vector as x$version,
        which has a `class` of {format_chr_deparse(class(x$version))}",
        class = "epiprocess__version_bound_mismatched_class"
      )
    }
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
#' @keywords internal
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
    check_names(names(x), must.include = "version")
    version_col <- x[["version"]]
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
#' @keywords internal
next_after <- function(x) UseMethod("next_after")


#' @keywords internal
next_after.integer <- function(x) x + 1L


#' @keywords internal
next_after.Date <- function(x) x + 1L


#' `epi_archive` object
#'
#' @description The second main data structure for storing time series in
#' `epiprocess`. It is similar to `epi_df` in that it fundamentally a table with
#' a few required columns that stores epidemiological time series data. An
#' `epi_archive` requires a `geo_value`, `time_value`, and `version` column (and
#' possibly other key columns) along with measurement values. In brief, an
#' `epi_archive` is a history of the time series data, where the `version`
#' column tracks the time at which the data was available. This allows for
#' version-aware forecasting.
#'
#' `new_epi_archive` is the low-level constructor for `epi_archive` objects that
#' only performs some fast, basic checks on the inputs. `validate_epi_archive`
#' can perform more costly validation checks on its output. But most users
#' should use `as_epi_archive`, which performs all necessary checks and has some
#' additional features.
#'
#' @details An `epi_archive` contains a `data.table` object `DT` (from the
#' `{data.table}` package), with (at least) the following columns:
#'
#' * `geo_value`: the geographic value associated with each row of measurements,
#' * `time_value`: the time value associated with each row of measurements,
#' * `version`: the time value specifying the version for each row of
#'   measurements. For example, if in a given row the `version` is January 15,
#'   2022 and `time_value` is January 14, 2022, then this row contains the
#'   measurements of the data for January 14, 2022 that were available one day
#'   later.
#'
#' The variables `geo_value`, `time_value`, `version` serve as key variables for
#'   the data table (in addition to any other keys specified in the metadata).
#'   There can only be a single row per unique combination of key variables. The
#'   keys for an `epi_archive` can be viewed with `key(epi_archive$DT)`.
#'
#' ## Compactification
#'
#' By default, an `epi_archive` will compactify the data table to remove
#' redundant rows. This is done by not storing rows that have the same value,
#' except for the `version` column (this is essentially a last observation
#' carried forward, but along the version index). This is done to save space and
#' improve performance. If you do not want to compactify the data, you can set
#' `compactify = FALSE` in `as_epi_archive()`.
#'
#' Note that in some data scenarios, LOCF may not be appropriate. For instance,
#' if you expected data to be updated on a given day, but your data source did
#' not update, then it could be reasonable to code the data as `NA` for that
#' day, instead of assuming LOCF.
#'
#' `NA`s *can* be introduced by `epi_archive` methods for other
#' reasons, e.g., in [`epix_fill_through_version`] and [`epix_merge`], if
#' requested, to represent potential update data that we do not yet have access
#' to; or in [`epix_merge`] to represent the "value" of an observation before
#' the version in which it was first released, or if no version of that
#' observation appears in the archive data at all.
#'
#' ## Metadata
#'
#' The following pieces of metadata are included as fields in an `epi_archive`
#'   object:
#'
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `other_keys`: any additional keys as a character vector.
#'    Typical examples are "age" or sub-geographies.
#'
#' While this metadata is not protected, it is generally recommended to treat it
#'  as read-only, and to use the `epi_archive` methods to interact with the data
#'  archive. Unexpected behavior may result from modifying the metadata
#'  directly.
#'
#' @param x A data.frame, data.table, or tibble, with columns `geo_value`,
#'   `time_value`, `version`, and then any additional number of columns.
#' @param geo_type DEPRECATED Has no effect. Geo value type is inferred from the
#' location column and set to "custom" if not recognized.
#' @param time_type DEPRECATED Has no effect. Time value type inferred from the time
#' column and set to "custom" if not recognized. Unpredictable behavior may result
#' if the time type is not recognized.
#' @param other_keys Character vector specifying the names of variables in `x`
#'   that should be considered key variables (in the language of `data.table`)
#'   apart from "geo_value", "time_value", and "version". Typical examples
#'   are "age" or more granular geographies.
#' @param compactify Optional; `TRUE`, `FALSE`, or `"message"`. `TRUE` will
#'   remove some redundant rows, `FALSE` will not. `"message"` is like `TRUE`
#'   but will emit a message if anything was changed. Default is `TRUE`. See
#'   more information below under "Compactification:".
#' @param compactify_abs_tol Optional; double. A tolerance level used to detect
#'   approximate equality for compactification. The default is 0, which
#'   corresponds to exact equality. Consider using this if your value columns
#'   undergo tiny nonmeaningful revisions and the archive object with the
#'   default setting is too large.
#' @param clobberable_versions_start Optional; `length`-1; either a value of the
#'   same `class` as `x$version`, or an `NA` of any `class`: specifically,
#'   either (a) the earliest version that could be subject to "clobbering"
#'   (being overwritten with different update data, but using the *same* version
#'   tag as the old update data), or (b) `NA`, to indicate that no versions are
#'   clobberable. There are a variety of reasons why versions could be
#'   clobberable under routine circumstances, such as (a) today's version of
#'   one/all of the columns being published after initially being filled with
#'   `NA` or LOCF, (b) a buggy version of today's data being published but then
#'   fixed and republished later in the day, or (c) data pipeline delays (e.g.,
#'   publisher uploading, periodic scraping, database syncing, periodic
#'   fetching, etc.) that make events (a) or (b) reflected later in the day (or
#'   even on a different day) than expected; potential causes vary between
#'   different data pipelines. The default value is `NA`, which doesn't consider
#'   any versions to be clobberable. Another setting that may be appropriate for
#'   some pipelines is `max_version_with_row_in(x)`.
#' @param versions_end Optional; length-1, same `class` as `x$version`: what is
#'   the last version we have observed? The default is
#'   `max_version_with_row_in(x)`, but values greater than this could also be
#'   valid, and would indicate that we observed additional versions of the data
#'   beyond `max(x$version)`, but they all contained empty updates. (The default
#'   value of `clobberable_versions_start` does not fully trust these empty
#'   updates, and assumes that any version `>= max(x$version)` could be
#'   clobbered.) If `nrow(x) == 0`, then this argument is mandatory.
#' @return * Of `new_epi_archive`: an (unvalidated) `epi_archive`
#'
#' @seealso [`epix_as_of`] [`epix_merge`] [`epix_slide`]
#' @importFrom dplyr if_any if_all everything
#' @importFrom utils capture.output
#'
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
#' toy_epi_archive <- tib %>% as_epi_archive()
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
#' x <- df %>% as_epi_archive(other_keys = "county")
#'
#' @name epi_archive
#' @order 3
#' @export
new_epi_archive <- function(
    x,
    geo_type,
    time_type,
    other_keys,
    clobberable_versions_start,
    versions_end) {
  assert_data_frame(x)
  assert_string(geo_type)
  assert_string(time_type)
  assert_character(other_keys, any.missing = FALSE)
  if (any(c("geo_value", "time_value", "version") %in% other_keys)) {
    cli_abort("`other_keys` cannot contain \"geo_value\", \"time_value\", or \"version\".")
  }
  validate_version_bound(clobberable_versions_start, x, na_ok = TRUE)
  validate_version_bound(versions_end, x, na_ok = FALSE)

  key_vars <- c("geo_value", "time_value", other_keys, "version")
  if (!all(key_vars %in% names(x))) {
    # Give a more tailored error message than as.data.table would:
    cli_abort(c(
      "`x` is missing the following expected columns:
       {format_varnames(setdiff(key_vars, names(x)))}.",
      ">" = "You might need to `dplyr::rename()` beforehand
                       or use `as_epi_archive()`'s renaming feature.",
      ">" = if (!all(other_keys %in% names(x))) {
        "Check also for typos in `other_keys`."
      }
    ))
  }

  # Create the data table; if x was an un-keyed data.table itself,
  # then the call to as.data.table() will fail to set keys, so we
  # need to check this, then do it manually if needed
  data_table <- as.data.table(x, key = key_vars)
  if (!identical(key_vars, key(data_table))) setkeyv(data_table, cols = key_vars)

  structure(
    list(
      DT = data_table,
      geo_type = geo_type,
      time_type = time_type,
      other_keys = other_keys,
      clobberable_versions_start = clobberable_versions_start,
      versions_end = versions_end
    ),
    class = "epi_archive"
  )
}

#' Perform second (costly) round of validation that `x` is a proper `epi_archive`
#'
#' Does not validate `geo_type` or `time_type` against `geo_value` and
#' `time_value` columns. These are assumed to have been set to compatibly.
#'
#' @return * Of `validate_epi_archive`: an `epi_archive`,
#'   [invisibly][base::invisible] (or raises an error if `x` was invalid)
#'
#' @rdname epi_archive
#' @order 4
#' @export
validate_epi_archive <- function(x) {
  assert_class(x, "epi_archive")

  ukey_vars1 <- c("geo_value", "time_value", x$other_keys, "version")
  ukey_vars2 <- key(x$DT)
  if (!identical(ukey_vars1, ukey_vars2)) {
    cli_abort(c("`data.table::key(x$DT)` not as expected",
      "*" = "Based on `x$other_keys` the key should be {format_chr_deparse(ukey_vars1)}",
      "*" = "But `key(x$DT)` is {format_chr_deparse(ukey_vars2)}",
      ">" = "Consider reconstructing the archive from `x$DT` specifying
                       the appropriate `other_keys`."
    ))
  }
  # Rely on data.table to ensure that these key columns exist.

  if (anyDuplicated(x$DT, by = key(x$DT)) != 0L) {
    cli_abort("`x$DT` must have one row per unique combination of the key variables. If you
            have additional key variables other than `geo_value`, `time_value`, and
            `version`, such as an age group column, please specify them in `other_keys`.
            Otherwise, check for duplicate rows and/or conflicting values for the same
            measurement.",
      class = "epiprocess__epi_archive_requires_unique_key"
    )
  }

  if (!identical(class(x$DT$time_value), class(x$DT$version))) {
    cli_abort(
      "`x$DT$time_value` and `x$DT$version` must have the same class.",
      class = "epiprocess__time_value_version_mismatch"
    )
  }

  if (anyMissing(x$DT$version)) {
    cli_abort("Column `version` must not contain missing values.")
  }

  if (nrow(x$DT) > 0L && x$versions_end < max(x$DT$version)) {
    cli_abort(
      "`x$versions_end` was {x$versions_end}, but `x$DT` contained
        updates for a later version or versions, up through {max(x$DT$version)}",
      class = "epiprocess__versions_end_earlier_than_updates"
    )
  }
  if (!is.na(x$clobberable_versions_start) && x$clobberable_versions_start > x$versions_end) {
    cli_abort(
      "`x$versions_end` was {x$versions_end}; however, `x$clobberable_versions_start`
        was {x$clobberable_versions_start}, indicating that there were later observed versions",
      class = "epiprocess__versions_end_earlier_than_clobberable_versions_start"
    )
  }

  # Return x visibly due to popular `validate_...(new_...())` pattern:
  x
}


#' Given a tibble as would be found in an epi_archive, remove duplicate entries.
#'
#' Works by shifting all rows except the version, then comparing values to see
#'   if they've changed. We need to arrange in descending order, but note that
#'   we don't need to group, since at least one column other than version has
#'   changed, and so is kept.
#'
#' @param updates_df DT of an `epi_archive` or something analogous (though
#'   potentially unsorted) of another class
#' @param ukey_names chr; the column names forming a unique key for the
#'   `updates_df`; "version" must come last. For an `epi_archive`'s `DT`, this
#'   would be `key(DT)`.
#' @param abs_tol numeric, >=0; absolute tolerance to use on numeric measurement
#'   columns when determining whether something can be compactified away; see
#'   [`is_locf`]
#'
#' @importFrom data.table is.data.table key
#' @importFrom dplyr arrange filter
#' @importFrom vctrs vec_duplicate_any
#'
#' @keywords internal
apply_compactify <- function(updates_df, ukey_names, abs_tol = 0) {
  assert_data_frame(updates_df)
  assert_character(ukey_names)
  assert_subset(ukey_names, names(updates_df))
  if (vec_duplicate_any(ukey_names)) {
    cli_abort("`ukey_names` must not contain duplicates")
  }
  if (length(ukey_names) == 0 || ukey_names[[length(ukey_names)]] != "version") {
    cli_abort('"version" must appear in `ukey_names` and must be last.')
  }
  assert_numeric(abs_tol, len = 1, lower = 0)

  if (!is.data.table(updates_df) || !identical(key(updates_df), ukey_names)) {
    updates_df <- updates_df %>% arrange(pick(all_of(ukey_names)))
  }
  updates_df[!update_is_locf(updates_df, ukey_names, abs_tol), ]
}

#' get the entries that `compactify` would remove
#' @keywords internal
#' @importFrom dplyr filter if_all everything
removed_by_compactify <- function(updates_df, ukey_names, abs_tol) {
  if (!is.data.table(updates_df) || !identical(key(updates_df), ukey_names)) {
    updates_df <- updates_df %>% arrange(pick(all_of(ukey_names)))
  }
  updates_df[update_is_locf(updates_df, ukey_names, abs_tol), ]
}

#' Internal helper; lgl; which updates are LOCF
#'
#' (Not validated:) Must be called inside certain dplyr data masking verbs (e.g.,
#' `filter` or `mutate`) being run on an `epi_archive`'s `DT` or a data frame
#' formatted like one.
#'
#' @param arranged_updates_df an arranged update data frame like an `epi_archive` `DT`
#' @param ukey_names (not validated:) chr; the archive/equivalent
#'   [`key_colnames`]; must include `"version"`.
#' @param abs_tol (not validated:) as in [`apply_compactify`]
#'
#' @return lgl
#'
#' @keywords internal
update_is_locf <- function(arranged_updates_df, ukey_names, abs_tol) {
  # Use as.list to get a shallow "copy" in case of data.table, so that column
  # selection does not copy the column contents. Don't leak these column aliases
  # or it will break data.table ownership model.
  updates_col_refs <- as.list(arranged_updates_df)

  all_names <- names(arranged_updates_df)
  ekt_names <- ukey_names[ukey_names != "version"]
  val_names <- all_names[!all_names %in% ukey_names]

  Reduce(`&`, lapply(updates_col_refs[ekt_names], is_locf, abs_tol, TRUE)) &
    Reduce(`&`, lapply(updates_col_refs[val_names], is_locf, abs_tol, FALSE))
}

#' Checks to see if a value in a vector is LOCF
#' @description LOCF meaning last observation carried forward (to later
#'   versions). Lags the vector by 1, then compares with itself. If `is_key` is
#'   `TRUE`, only values that are exactly the same between the lagged and
#'   original are considered LOCF. If `is_key` is `FALSE` and `vec` is a vector
#'   of numbers ([`base::is.numeric`]), then approximate equality will be used,
#'   checking whether the absolute difference between each pair of entries is
#'   `<= abs_tol`; if `vec` is something else, then exact equality is used
#'   instead.
#'
#' @details
#'
#' We include epikey-time columns in LOCF comparisons as part of an optimization
#' to avoid slower grouped operations while still ensuring that the first
#' observation for each time series will not be marked as LOCF. We test these
#' key columns for exact equality to prevent chopping off consecutive
#' time_values during flat periods when `abs_tol` is high.
#'
#' We use exact equality for non-`is.numeric` double/integer columns such as
#' dates, datetimes, difftimes, `tsibble::yearmonth`s, etc., as these may be
#' used as part of re-indexing or grouping procedures, and we don't want to
#' change the number of groups for those operations when we remove LOCF data
#' during compactification.
#'
#' @importFrom dplyr lag if_else
#' @importFrom rlang is_bare_numeric
#' @importFrom vctrs vec_equal
#' @keywords internal
is_locf <- function(vec, abs_tol, is_key) { # nolint: object_usage_linter
  lag_vec <- lag(vec)
  if (is.vector(vec, mode = "numeric") && !is_key) {
    # (integer or double vector, no class (& no dims); maybe names, which we'll
    # ignore like `vec_equal`); not a key column
    unname(if_else(
      !is.na(vec) & !is.na(lag_vec),
      abs(vec - lag_vec) <= abs_tol,
      is.na(vec) & is.na(lag_vec)
    ))
  } else {
    vec_equal(vec, lag_vec, na_equal = TRUE)
  }
}

#' `as_epi_archive` converts a data frame, data table, or tibble into an
#' `epi_archive` object.
#'
#' @param ... used for specifying column names, as in [`dplyr::rename`]. For
#'   example `version = release_date`
#' @param .versions_end location based versions_end, used to avoid prefix
#'   `version = issue` from being assigned to `versions_end` instead of being
#'   used to rename columns.
#' @return * Of `as_epi_archive`: an `epi_archive` object
#'
#' @rdname epi_archive
#' @order 1
#'
#' @export
as_epi_archive <- function(
    x,
    geo_type = deprecated(),
    time_type = deprecated(),
    other_keys = character(),
    compactify = TRUE,
    compactify_abs_tol = 0,
    clobberable_versions_start = NA,
    .versions_end = max_version_with_row_in(x), ...,
    versions_end = .versions_end) {
  assert_data_frame(x)
  x <- rename(x, ...)
  x <- guess_column_name(x, "time_value", time_column_names())
  x <- guess_column_name(x, "geo_value", geo_column_names())
  x <- guess_column_name(x, "version", version_column_names())

  if (lifecycle::is_present(geo_type)) {
    cli_warn("epi_archive constructor argument `geo_type` is now ignored. Consider removing.")
  }
  if (lifecycle::is_present(time_type)) {
    cli_warn("epi_archive constructor argument `time_type` is now ignored. Consider removing.")
  }

  geo_type <- guess_geo_type(x$geo_value)
  time_type <- guess_time_type(x$time_value)

  result <- validate_epi_archive(new_epi_archive(
    x, geo_type, time_type, other_keys,
    clobberable_versions_start, versions_end
  ))

  # Compactification:
  if (!list(compactify) %in% list(TRUE, FALSE, "message")) {
    cli_abort('`compactify` must be `TRUE`, `FALSE`, or `"message"`')
  }

  data_table <- result$DT
  key_vars <- key(data_table)

  nrow_before_compactify <- nrow(data_table)
  # Runs compactify on data frame
  if (identical(compactify, TRUE) || identical(compactify, "message")) {
    compactified <- apply_compactify(data_table, key_vars, compactify_abs_tol)
  } else {
    compactified <- data_table
  }
  # Messages about redundant rows if the number of rows decreased, and we didn't
  # explicitly say to compactify
  if (identical(compactify, "message") && nrow(compactified) < nrow_before_compactify) {
    elim <- removed_by_compactify(data_table, key_vars, compactify_abs_tol)
    message_intro <- cli::format_inline(
      "Found rows that appear redundant based on
       last (version of each) observation carried forward;
       these rows have been removed to 'compactify' and save space:",
      keep_whitespace = FALSE
    )
    message_data <- paste(collapse = "\n", capture.output(print(elim, topn = 3L, nrows = 7L)))
    message_outro <- cli::format_inline(
      "Built-in `epi_archive` functionality should be unaffected,
       but results may change if you work directly with its fields (such as `DT`).
       See `?as_epi_archive` for details.
       To silence this message but keep compactification,
       you can pass `compactify=TRUE` when constructing the archive.",
      keep_whitespace = FALSE
    )
    message_string <- paste(sep = "\n", message_intro, message_data, message_outro)
    rlang::inform(message_string, class = "epiprocess__compactify_default_removed_rows")
  }

  result$DT <- compactified
  result
}


#' Print information about an `epi_archive` object
#'
#' @param x An `epi_archive` object.
#' @param ... Should be empty, there to satisfy the S3 generic.
#' @param class Boolean; whether to print the class label header
#' @param methods Boolean; whether to print all available methods of
#'   the archive
#'
#' @importFrom cli cat_line format_message
#' @importFrom rlang check_dots_empty
#' @export
print.epi_archive <- function(x, ..., class = TRUE, methods = TRUE) {
  if (rlang::dots_n(...) > 0) {
    cli_abort(c(
      "Error in print.epi_archive()",
      "i" = "Too many arguments passed to `print.epi_archive()`."
    ))
  }

  cat_line(format_message(
    c(
      ">" = if (class) "An `epi_archive` object, with metadata:",
      "i" = if (length(setdiff(key(x$DT), c("geo_value", "time_value", "version"))) > 0) {
        "Other DT keys: {setdiff(key(x$DT), c('geo_value', 'time_value', 'version'))}"
      },
      "i" = if (nrow(x$DT) != 0L) {
        "Min/max time values: {min(x$DT$time_value)} / {max(x$DT$time_value)}"
      },
      "i" = if (nrow(x$DT) != 0L) {
        "First/last version with update: {min(x$DT$version)} / {max(x$DT$version)}"
      },
      "i" = if (!is.na(x$clobberable_versions_start)) {
        "Clobberable versions start: {x$clobberable_versions_start}"
      },
      "i" = "Versions end: {x$versions_end}",
      "i" = "A preview of the table ({nrow(x$DT)} rows x {ncol(x$DT)} columns):"
    )
  ))
  print(x$DT[])

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
#' @param x For `groups`, `group_vars`, or `ungroup`: a `grouped_epi_archive`;
#'   for `is_grouped_epi_archive`: any object
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
#'     .f = ~ mean(.x$case_rate_7d_av),
#'     .before = 2,
#'     .versions = as.Date("2020-06-11") + 0:2,
#'     .new_col_name = "case_rate_3d_av"
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
#' # To get the grouping variable names as a character vector:
#' toy_archive %>%
#'   group_by(geo_value) %>%
#'   group_vars()
#'
#' # To get the grouping variable names as a `list` of `name`s (a.k.a. symbols):
#' toy_archive %>%
#'   group_by(geo_value) %>%
#'   groups()
#'
#' toy_archive %>%
#'   group_by(geo_value, age_group, .drop = FALSE) %>%
#'   epix_slide(.f = ~ sum(.x$value), .before = 20) %>%
#'   ungroup()
#'
#' @importFrom dplyr group_by
#' @export
#'
#' @aliases grouped_epi_archive
group_by.epi_archive <- function(.data, ..., .add = FALSE,
                                 .drop = dplyr::group_by_drop_default(.data)) {
  # `add` makes no difference; this is an ungrouped `epi_archive`.
  detailed_mutate <- epix_detailed_restricted_mutate(.data, ...)
  assert_logical(.drop)
  if (!.drop) {
    grouping_cols <- as.list(detailed_mutate[["archive"]][["DT"]])[detailed_mutate[["request_names"]]]
    grouping_col_is_factor <- purrr::map_lgl(grouping_cols, is.factor)
    # ^ Use `as.list` to try to avoid any possibility of a deep copy.
    if (length(grouping_cols) != 0L && !any(grouping_col_is_factor)) {
      cli_warn(
        "`.drop=FALSE` but none of the grouping columns are factors;
        did you mean to convert one of the columns to a factor beforehand?",
        class = "epiprocess__group_by_epi_archive__drop_FALSE_no_factors"
      )
    } else if (any(diff(grouping_col_is_factor) == -1L)) {
      cli_warn(
        "`.drop=FALSE` but there are one or more non-factor grouping columns listed
        after a factor grouping column; this may produce groups with `NA`s for these
        non-factor columns; see https://github.com/tidyverse/dplyr/issues/5369#issuecomment-683762553;
        depending on how you want completion to work, you might instead want to convert all
        grouping columns to factors beforehand, specify the non-factor grouping columns first,
        or use `.drop=TRUE` and add a call to `tidyr::complete()`.",
        class = "epiprocess__group_by_epi_archive__drop_FALSE_nonfactor_after_factor"
      )
    }
  }
  new_grouped_epi_archive(detailed_mutate[["archive"]],
    detailed_mutate[["request_names"]],
    drop = .drop
  )
}


#' Clone an `epi_archive` object.
#'
#' @param x An `epi_archive` object.
#'
#' @importFrom data.table copy
#' @export
clone <- function(x) {
  UseMethod("clone")
}


#' @rdname clone
#' @export
clone.epi_archive <- function(x) {
  x$DT <- data.table::copy(x$DT)
  x
}

#' Test for `epi_archive` format
#'
#' @param x An object.
#' @return * Of `is_epi_archive`: `TRUE` if the object inherits from `epi_archive`,
#'           otherwise `FALSE`.
#'
#' @rdname epi_archive
#' @order 2
#' @export
is_epi_archive <- function(x) {
  inherits(x, "epi_archive")
}
