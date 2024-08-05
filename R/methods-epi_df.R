#' Convert to tibble
#'
#' Converts an `epi_df` object into a tibble, dropping metadata and any
#' grouping.
#'
#' Advanced: if you are working with a third-party package that uses
#' `as_tibble()` on `epi_df`s but you actually want them to remain `epi_df`s,
#' use `attr(your_epi_df, "decay_to_tibble") <- FALSE` beforehand.
#'
#' @template x
#'
#' @inheritParams tibble::as_tibble
#'
#' @importFrom tibble as_tibble
#' @export
as_tibble.epi_df <- function(x, ...) {
  # Decaying drops the class and metadata. `as_tibble.grouped_df` drops the
  # grouping and should be called by `NextMethod()` in the current design.
  # See #223 for discussion of alternatives.
  if (attr(x, "decay_to_tibble") %||% TRUE) {
    return(decay_epi_df(NextMethod()))
  }
  metadata <- attr(x, "metadata")
  reclass(NextMethod(), metadata)
}

#' Convert to tsibble format
#'
#' Converts an `epi_df` object into a tsibble, where the index is taken to be
#' `time_value`, and the key variables taken to be `geo_value` along with any
#' others in the `other_keys` field of the metadata, or else explicitly set.
#'
#' @method as_tsibble epi_df
#' @template x
#' @param key Optional. Any additional keys (other than `geo_value`) to add to
#'   the `tsibble`.
#' @param ... additional arguments passed on to `tsibble::as_tsibble()`
#' @export
as_tsibble.epi_df <- function(x, key, ...) {
  if (missing(key)) key <- c("geo_value", attributes(x)$metadata$other_keys)
  return(as_tsibble(tibble::as_tibble(x),
    key = tidyselect::all_of(key), index = "time_value",
    ...
  ))
}

#' Base S3 methods for an `epi_df` object
#'
#' Print and summary functions for an `epi_df` object.
#'
#' @template x
#'
#' @method print epi_df
#' @param ... additional arguments to forward to `NextMethod()`, or unused
#' @export
print.epi_df <- function(x, ...) {
  cat(
    "An `epi_df` object,", prettyNum(nrow(x), ","), "x",
    prettyNum(ncol(x), ","), "with metadata:\n"
  )
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(x)$metadata$as_of))
  # Conditional output (silent if attribute is NULL):
  cat(sprintf("* %-9s = %s\n", "decay_to_tibble", attr(x, "decay_to_tibble")))
  cat("\n")
  NextMethod()
}

#' Summarize `epi_df` object
#'
#' Prints a variety of summary statistics about the `epi_df` object, such as
#' the time range included and geographic coverage.
#'
#' @param object an `epi_df`
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#'
#' @method summary epi_df
#' @importFrom rlang .data
#' @importFrom stats median
#' @export
summary.epi_df <- function(object, ...) {
  cat("An `epi_df` x, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(object)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(object)$metadata$as_of))
  cat("----------\n")
  cat(sprintf("* %-27s = %s\n", "min time value", min(object$time_value)))
  cat(sprintf("* %-27s = %s\n", "max time value", max(object$time_value)))
  cat(sprintf(
    "* %-27s = %i\n", "average rows per time value",
    as.integer(
      object %>%
        dplyr::group_by(.data$time_value) %>%
        dplyr::summarize(num = dplyr::n()) %>%
        dplyr::summarize(mean(.data$num))
    )
  ))
}

#' Drop any `epi_df` metadata and class on a data frame
#'
#' Useful in implementing `?dplyr_extending` when manipulations cause invariants
#' of `epi_df`s to be violated and we need to return some other class. Note that
#' this will maintain any grouping (keeping the `grouped_df` class and
#' associated attributes, if present).
#'
#' @param x an `epi_df` or other data frame
#' @return `x` with any metadata dropped and the `"epi_df"` class, if previously
#'   present, dropped
#'
#' @noRd
decay_epi_df <- function(x) {
  attributes(x)$metadata <- NULL
  class(x) <- class(x)[class(x) != "epi_df"]
  x
}

# Implementing `dplyr_extending`: we have a few metadata attributes to consider:
# `as_of` is an attribute doesn't depend on the rows or columns, `geo_type` is a
# scalar attribute dependent on columns, and `other_keys` acts like an attribute
# vectorized over columns; `dplyr_extending` advice at time of writing says to
# implement `dplyr_reconstruct`, 1d `[`, `dplyr_col_modify`, and `names<-`, but
# not `dplyr_row_slice`; however, we'll also implement `dplyr_row_slice` anyway
# to prevent a `arrange` on grouped `epi_df`s from dropping the `epi_df` class.
# We'll implement `[` to allow either 1d or 2d. We'll also implement some other
# methods where we want to (try to) maintain an `epi_df`.

#' @param data tibble or `epi_df` (`dplyr` feeds in former, but we may
#'   directly feed in latter from our other methods)
#' @param template `epi_df` template to use to restore
#' @return `epi_df` or degrade into `tbl_df`
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom cli cli_vec
#' @export
#' @noRd
dplyr_reconstruct.epi_df <- function(data, template) {
  # Start from a reconstruction for the backing S3 classes; this ensures that we
  # keep any grouping that has been applied:
  res <- NextMethod()

  cn <- names(res)

  # Duplicate columns, cli_abort
  dup_col_names <- cn[duplicated(cn)]
  if (length(dup_col_names) != 0) {
    cli_abort(c(
      "Duplicate column names are not allowed",
      "i" = "Duplicated column name{?s}:
        {cli_vec(unique(dup_col_names),
                 style = list('vec-sep2' = ', ', 'vec-last' = ', '))}"
    ))
  }

  not_epi_df <- !("time_value" %in% cn) || !("geo_value" %in% cn)

  if (not_epi_df) {
    # If we're calling on an `epi_df` from one of our own functions, we need to
    # decay to a non-`epi_df` result. If `dplyr` is calling, `x` is a tibble,
    # `res` is not an `epi_df` yet (but might, e.g., be a `grouped_df`), and we
    # simply need to skip adding the metadata & class. Current `decay_epi_df`
    # should work in both cases.
    return(decay_epi_df(res))
  }

  res <- reclass(res, attr(template, "metadata"))

  # XXX we may want verify the `geo_type` and `time_type` here. If it's
  # significant overhead, we may also want to keep this less strict version
  # around and implement some extra S3 methods that use it, when appropriate.

  # Amend additional metadata if some other_keys cols are dropped in the subset
  old_other_keys <- attr(template, "metadata")$other_keys
  attr(res, "metadata")$other_keys <- old_other_keys[old_other_keys %in% cn]

  res
}

#' @export
`[.epi_df` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()

  if (!is.data.frame(res)) {
    return(res)
  }

  dplyr::dplyr_reconstruct(res, x)
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epi_df <- function(data, cols) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epi_df <- function(data, i, ...) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

#' @export
`names<-.epi_df` <- function(x, value) {
  old_names <- names(x)
  old_metadata <- attr(x, "metadata")
  old_other_keys <- old_metadata[["other_keys"]]
  new_other_keys <- value[match(old_other_keys, old_names)]
  new_metadata <- old_metadata
  new_metadata[["other_keys"]] <- new_other_keys
  result <- reclass(NextMethod(), new_metadata)
  # decay to non-`epi_df` if needed:
  dplyr::dplyr_reconstruct(result, result)
}

#' @method group_by epi_df
#' @param .data an `epi_df`
#' @rdname print.epi_df
#' @export
group_by.epi_df <- function(.data, ...) {
  metadata <- attributes(.data)$metadata
  .data <- NextMethod()
  reclass(.data, metadata)
}

#' @method ungroup epi_df
#' @rdname print.epi_df
#' @export
ungroup.epi_df <- function(x, ...) {
  metadata <- attributes(x)$metadata
  x <- NextMethod()
  reclass(x, metadata)
}

#' @method group_modify epi_df
#' @rdname print.epi_df
#' @param .data an `epi_df`
#' @param .f function or formula; see [`dplyr::group_modify`]
#' @param .keep Boolean; see [`dplyr::group_modify`]
#' @export
group_modify.epi_df <- function(.data, .f, ..., .keep = FALSE) {
  dplyr::dplyr_reconstruct(NextMethod(), .data)
}

#' Complete epi_df
#'
#' A [tidyr::complete()] analogue for `epi_df` objects. This function fills in
#' missing combinations of `geo_value` and `time_value` with `NA` values. See
#' the examples for usage details.
#'
#' @param data an `epi_df`
#' @param ... see [`tidyr::complete`]
#' @param fill see [`tidyr::complete`]
#' @param explicit see [`tidyr::complete`]
#'
#' @method complete epi_df
#' @importFrom tidyr complete
#'
#' @examples
#' start_date <- as.Date("2020-01-01")
#' daily_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 3, 3,
#'   2, start_date + 2, 2,
#'   2, start_date + 3, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' # Complete without grouping puts all the geo_values on the same min and max
#' # time_value index
#' daily_edf %>%
#'   complete(geo_value, time_value = full_seq(time_value, period = 1))
#' # Complete with grouping puts all the geo_values on individual min and max
#' # time_value indices
#' daily_edf %>%
#'   group_by(geo_value) %>%
#'   complete(time_value = full_seq(time_value, period = 1))
#' # Complete has explicit=TRUE by default, but if it's FALSE, then complete only fills the implicit gaps
#' # not those that are explicitly NA
#' daily_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 2, NA,
#'   1, start_date + 3, 3,
#'   2, start_date + 2, 2,
#'   2, start_date + 3, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' daily_edf %>%
#'   complete(geo_value, time_value = full_seq(time_value, period = 1), fill = list(value = 0), explicit = FALSE)
#' # Complete works for weekly data and can take a fill value
#' # No grouping
#' weekly_edf <- tibble::tribble(
#'   ~geo_value, ~time_value, ~value,
#'   1, start_date + 1, 1,
#'   1, start_date + 15, 3,
#'   2, start_date + 8, 2,
#'   2, start_date + 15, 3,
#' ) %>%
#'   as_epi_df(as_of = start_date + 3)
#' weekly_edf %>%
#'   complete(geo_value, time_value = full_seq(time_value, period = 7), fill = list(value = 0))
#' # With grouping
#' weekly_edf %>%
#'   group_by(geo_value) %>%
#'   complete(time_value = full_seq(time_value, period = 7), fill = list(value = 0))
#' @export
complete.epi_df <- function(data, ..., fill = list(), explicit = TRUE) {
  result <- dplyr::dplyr_reconstruct(NextMethod(), data)
  if ("time_value" %in% names(rlang::call_match(dots_expand = FALSE)[["..."]])) {
    attr(result, "metadata")$time_type <- guess_time_type(result$time_value)
  }
  result
}

#' @method unnest epi_df
#' @rdname print.epi_df
#' @param data an `epi_df`
#' @export
unnest.epi_df <- function(data, ...) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

# Simple reclass function
reclass <- function(x, metadata) {
  class(x) <- unique(c("epi_df", class(x)))
  attributes(x)$metadata <- metadata
  return(x)
}
