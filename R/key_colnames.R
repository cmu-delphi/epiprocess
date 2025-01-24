#' Get names of columns that form a (unique) key associated with an object
#'
#' This is entirely based on metadata and arguments passed; there are no
#' explicit checks that the key actually is unique in any associated data
#' structures.
#'
#' @param x an object, often a data frame or something similar. `{epiprocess}`
#'   includes implementations for [`epi_df`]s, [`epi_archive`]s,
#'   [`tsibble::tsibble`]s, and other data frames (including
#'   [`tibble::tibble`]s); other packages, like `{epipredict}`, can add more.
#' @param ... additional arguments passed on to methods
#' @param geo_keys,other_keys,time_keys character vectors, sometimes optional;
#'   which variables (if any) should be considered as part of a unique
#'   key/identifier for data in `x`, dealing respectively with the associated
#'   geographical region, demographic/strain/other information needed in
#'   addition to the geographical region to identify individual time series in
#'   `x`, and time interval during which associated events occurred.
#'
#'   Mandatory if `x` is a regular `data.frame` or `tibble`. Optional if `x` is an
#'   `epi_df`; the defaults are `"geo_value"`, the `epi_df`'s `other_keys`
#'   metadata, and `"time_value"`, respectively; if you provide these manually,
#'   they must match the defaults. (This behavior is to enable consistent and
#'   sane results when you can't guarantee whether `x` is an `epi_df` or just a
#'   `tibble`/`data.frame`. You don't need to use it if you know that `x` is
#'   definitely an `epi_df`.)
#' @param exclude an optional character vector of key column names to exclude
#'   from the result
#' @return character vector
#' @keywords internal
#' @export
key_colnames <- function(x, ..., exclude = character()) {
  provided_args <- rlang::call_args_names(rlang::call_match())
  if ("extra_keys" %in% provided_args) {
    lifecycle::deprecate_soft("0.9.6", "key_colnames(extra_keys=)", "key_colnames(other_keys=)")
    redispatch <- function(..., extra_keys) {
      key_colnames(..., other_keys = extra_keys)
    }
    redispatch(x, ..., exclude = exclude)
  } else {
    UseMethod("key_colnames")
  }
}

#' @rdname key_colnames
#' @importFrom rlang check_dots_empty0
#' @method key_colnames data.frame
#' @export
key_colnames.data.frame <- function(x, ...,
                                    geo_keys,
                                    other_keys,
                                    time_keys,
                                    exclude = character()) {
  check_dots_empty0(...)
  assert_character(geo_keys)
  assert_character(time_keys)
  assert_character(other_keys)
  assert_character(exclude)
  keys <- c(geo_keys, other_keys, time_keys)
  if (!all(keys %in% names(x))) {
    cli_abort(c(
      "Some of the specified key columns aren't present in `x`",
      "i" = "Specified keys: {format_varnames(keys)}",
      "i" = "Columns of x: {format_varnames(names(x))}",
      "x" = "Missing keys: {format_varnames(setdiff(keys, names(x)))}"
    ), class = "epiprocess__key_colnames__keys_not_in_colnames")
  }
  setdiff(keys, exclude)
}

#' @rdname key_colnames
#' @method key_colnames epi_df
#' @export
key_colnames.epi_df <- function(x, ...,
                                geo_keys = "geo_value",
                                other_keys = attr(x, "metadata")$other_keys,
                                time_keys = "time_value",
                                exclude = character()) {
  check_dots_empty0(...)
  if (!identical(geo_keys, "geo_value")) {
    cli_abort('If `x` is an `epi_df`, then `geo_keys` must be `"geo_value"`',
      class = "epiprocess__key_colnames__mismatched_geo_keys"
    )
  }
  if (!identical(time_keys, "time_value")) {
    cli_abort('If `x` is an `epi_df`, then `time_keys` must be `"time_value"`',
      class = "epiprocess__key_colnames__mismatched_time_keys"
    )
  }
  expected_other_keys <- attr(x, "metadata")$other_keys
  if (!identical(other_keys, expected_other_keys)) {
    cli_abort(c(
      "The provided `other_keys` argument didn't match the `other_keys` of `x`",
      "*" = "`other_keys` was {format_chr_with_quotes(other_keys)}",
      "*" = "`expected_other_keys` was {format_chr_with_quotes(expected_other_keys)}",
      "i" = "If you know that `x` will always be an `epi_df` and
             resolve this discrepancy by adjusting the metadata of `x`, you
             shouldn't have to pass `other_keys =` here anymore,
             unless you want to continue to perform this check."
    ), class = "epiprocess__key_colnames__mismatched_other_keys")
  }
  assert_character(exclude)
  setdiff(c("geo_value", other_keys, "time_value"), exclude)
}

#' @rdname key_colnames
#' @method key_colnames tbl_ts
#' @export
key_colnames.tbl_ts <- function(x, ..., exclude = character()) {
  check_dots_empty0(...)
  assert_character(exclude)
  idx <- tsibble::index_var(x)
  idx2 <- tsibble::index2_var(x)
  if (!identical(idx, idx2)) {
    cli_abort(c(
      "`x` is in the middle of a re-indexing operation with `index_by()`; it's unclear
       whether we should output the old unique key or the new unique key-to-be",
      "i" = "Old index: {format_varname(idx)}",
      "i" = "Pending new index: {format_varname(idx2)}",
      "Please complete (e.g., with `summarise()`) or remove the re-indexing operation."
    ), class = "epiprocess__key_colnames__incomplete_reindexing_operation")
  }
  setdiff(c(tsibble::key_vars(x), idx), exclude)
}

#' @rdname key_colnames
#' @method key_colnames epi_archive
#' @export
key_colnames.epi_archive <- function(x, ..., exclude = character()) {
  check_dots_empty0(...)
  assert_character(exclude)
  setdiff(c("geo_value", x$other_keys, "time_value", "version"), exclude)
}
