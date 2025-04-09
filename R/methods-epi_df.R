#' Convert to tibble
#'
#' Converts an `epi_df` object into a tibble, dropping metadata, any
#' grouping, and any unrelated classes and attributes.
#'
#' Advanced: if you are working with a third-party package that uses
#' `as_tibble()` on `epi_df`s but you actually want them to remain `epi_df`s,
#' use `attr(your_epi_df, "decay_to_tibble") <- FALSE` beforehand.
#'
#' @param x an `epi_df`
#' @param ... if present, forwarded to [`tibble::as_tibble`]
#' @importFrom tibble as_tibble new_tibble
#' @importFrom rlang dots_n
#' @importFrom vctrs vec_data vec_size
#' @export
as_tibble.epi_df <- function(x, ...) {
  # Note that some versions of `tsibble` overwrite `as_tibble.grouped_df`, which
  # also impacts grouped `epi_df`s, so don't rely on `NextMethod()`. Destructure
  # and redispatch instead.
  destructured <- vec_data(x) # -> data.frame, dropping extra attrs
  tbl <- if (dots_n(...) == 0 &&
    is.null(pkgconfig::get_config("tibble::rownames"))) { # nolint: indentation_linter
    # perf: new_tibble instead of as_tibble.data.frame which performs
    # extra checks whose defaults should be redundant here:
    new_tibble(destructured)
    # (^ We don't need to provide nrow= as we have >0 columns.)
  } else {
    as_tibble(destructured, ...)
  }
  if (attr(x, "decay_to_tibble") %||% TRUE) {
    tbl
  } else {
    # We specially requested via attr not to decay epi_df-ness but to drop any
    # grouping. (Miscellaneous attrs are also dropped.)
    reclass(tbl, attr(x, "metadata"))
  }
}

#' Convert to tsibble format
#'
#' Converts an `epi_df` object into a tsibble, where the index is taken to be
#' `time_value`, and the key variables taken to be `geo_value` along with any
#' others in the `other_keys` field of the metadata, or else explicitly set.
#'
#' @method as_tsibble epi_df
#' @param x an `epi_df`
#' @param key Optional. Any additional keys (other than `geo_value`) to add to
#'   the `tsibble`.
#' @param ... additional arguments passed on to `tsibble::as_tsibble()`
#' @export
as_tsibble.epi_df <- function(x, key, ...) {
  if (missing(key)) key <- c("geo_value", attributes(x)$metadata$other_keys)
  as_tsibble(
    tibble::as_tibble(x),
    key = tidyselect::all_of(key), index = "time_value",
    ...
  )
}

#' Base S3 methods for an `epi_df` object
#'
#' Print and summary functions for an `epi_df` object.
#'
#' @param x an `epi_df`
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
  ok <- attributes(x)$metadata$other_keys
  if (length(ok) > 0) {
    cat(sprintf("* %-9s = %s\n", "other_keys", paste(ok, collapse = ", ")))
  }
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
#' @rdname print.epi_df
#' @export
summary.epi_df <- function(object, ...) {
  cat("An `epi_df` x, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(object)$metadata$geo_type))
  ok <- attributes(object)$metadata$other_keys
  if (length(ok) > 0) {
    cat(sprintf("* %-9s = %s\n", "other_keys", paste(ok, collapse = ", ")))
  }
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
#' @keywords internal
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

#' dplyr_reconstruct
#'
#' @param data tibble or `epi_df` (`dplyr` feeds in former, but we may
#'   directly feed in latter from our other methods)
#' @param template `epi_df` template to use to restore
#' @return `epi_df` or degrade into `tbl_df`
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom cli cli_vec
#' @export
#' @keywords internal
dplyr_reconstruct.epi_df <- function(data, template) {
  # Start from a reconstruction for the backing S3 classes; this ensures that we
  # keep any grouping that has been applied:
  res <- NextMethod()

  reconstruct_light_edf(res, template)
}

#' Like `dplyr_reconstruct.epi_df` but not recomputing any grouping
#'
#' In the move to our current not-quite-proper/effective "implementation" of
#' [`dplyr::dplyr_extending`] for `epi_df`s, we moved a lot of checks in
#' `dplyr_reconstruct` and used it instead of `reclass()` in various
#' operations to prevent operations from outputting invalid metadata/classes,
#' instead of more careful tailored and relevant checks. However, this actually
#' introduced extra overhead due to `dplyr_reconstruct.epi_df()` passing off to
#' `dplyr_reconstruct.grouped_df()` when grouped, which assumes that it will
#' need to / should for safety recompute the groups, even when it'd be safe for
#' it not to do so. In many operations, we're using `NextMethod()` to dispatch
#' to `grouped_df` behavior if needed, and it should output something with valid
#' groupings.
#'
#' This function serves the original purpose of performing `epi_df`-centric
#' checks rather than just throwing on potentially-incorrect metadata like
#' `reclass()`, but without unnecessary `dplyr_reconstruct()` delegation.
#'
#' @keywords internal
reconstruct_light_edf <- function(data, template) {
  col_names <- names(data)

  # Duplicate columns, cli_abort
  dup_col_names <- col_names[duplicated(col_names)]
  if (length(dup_col_names) != 0) {
    cli_abort(c(
      "Duplicate column names are not allowed",
      "i" = "Duplicated column name{?s}:
        {cli_vec(unique(dup_col_names),
                 style = list('vec-sep2' = ', ', 'vec-last' = ', '))}"
    ))
  }

  not_epi_df <- !("time_value" %in% col_names) || !("geo_value" %in% col_names)

  if (not_epi_df) {
    # If we're calling on an `epi_df` from one of our own functions, we need to
    # decay to a non-`epi_df` result. If `dplyr` is calling, `x` is a tibble,
    # `data` is not an `epi_df` yet (but might, e.g., be a `grouped_df`), and we
    # simply need to skip adding the metadata & class. Current `decay_epi_df`
    # should work in both cases.
    return(decay_epi_df(data))
  }

  data <- reclass(data, attr(template, "metadata"))

  # XXX we may want verify the `geo_type` and `time_type` here. If it's
  # significant overhead, we may also want to keep this less strict version
  # around and implement some extra S3 methods that use it, when appropriate.

  # Amend additional metadata if some other_keys cols are dropped in the subset
  old_other_keys <- attr(template, "metadata")$other_keys
  attr(data, "metadata")$other_keys <- old_other_keys[old_other_keys %in% col_names]

  data
}

#' @export
`[.epi_df` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()

  if (!is.data.frame(res)) {
    return(res)
  }

  reconstruct_light_edf(res, x)
}

#' @export
`[<-.epi_df` <- function(x, i, j, ..., value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @export
`[[<-.epi_df` <- function(x, i, j, ..., value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @export
`$<-.epi_df` <- function(x, name, value) {
  res <- NextMethod()

  reconstruct_light_edf(res, x)
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epi_df <- function(data, cols) {
  reconstruct_light_edf(NextMethod(), data)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epi_df <- function(data, i, ...) {
  reconstruct_light_edf(NextMethod(), data)
}

#' @export
`names<-.epi_df` <- function(x, value) {
  old_names <- names(x)
  old_metadata <- attr(x, "metadata")
  new_metadata <- old_metadata
  old_other_keys <- old_metadata[["other_keys"]]
  if (!is.null(old_other_keys)) {
    new_other_keys <- value[match(old_other_keys, old_names)]
    new_metadata[["other_keys"]] <- new_other_keys
  }
  result <- reclass(NextMethod(), new_metadata)
  reconstruct_light_edf(result, result)
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
  reconstruct_light_edf(NextMethod(), .data)
}

#' "Complete" an `epi_df`, adding missing rows and/or replacing `NA`s
#'
#' A `tidyr::complete()` analogue for `epi_df` objects. This function
#' can be used, for example, to add rows for missing combinations
#' of `geo_value` and `time_value`, filling other columns with `NA`s.
#' See the examples for usage details.
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
#' # Complete has explicit=TRUE by default, but if it's FALSE, then complete
#' # only fills the implicit gaps, not those that are explicitly NA
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
#'   complete(
#'     geo_value,
#'     time_value = full_seq(time_value, period = 1),
#'     fill = list(value = 0),
#'     explicit = FALSE
#'   )
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
#'   complete(
#'     geo_value,
#'     time_value = full_seq(time_value, period = 7),
#'     fill = list(value = 0)
#'   )
#' # With grouping
#' weekly_edf %>%
#'   group_by(geo_value) %>%
#'   complete(
#'     time_value = full_seq(time_value, period = 7),
#'     fill = list(value = 0)
#'   )
#' @export
complete.epi_df <- function(data, ..., fill = list(), explicit = TRUE) {
  result <- reconstruct_light_edf(NextMethod(), data)
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
  reconstruct_light_edf(NextMethod(), data)
}

# Simple reclass function
reclass <- function(x, metadata) {
  class(x) <- unique(c("epi_df", class(x)))
  attr(x, "metadata") <- metadata
  x
}

#' Arrange an epi_df into a standard order
#'
#' Moves [key_colnames()] to the left, then arranges rows based on that
#' ordering. This function is mainly for use in tests and so that
#' other function output will be in predictable order, where necessary.
#'
#' @param x an `epi_df`. Other objects will produce a warning and return as is.
#' @param ... not used
#'
#' @keywords internal
#' @export
arrange_canonical <- function(x, ...) {
  UseMethod("arrange_canonical")
}

#' @export
arrange_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  x %>%
    arrange_row_canonical() %>%
    arrange_col_canonical()
}

arrange_row_canonical <- function(x, ...) {
  UseMethod("arrange_row_canonical")
}

#' @export
arrange_row_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_row_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_row_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  cols <- key_colnames(x)
  x[vctrs::vec_order(x[cols]), ]
}

arrange_col_canonical <- function(x, ...) {
  UseMethod("arrange_col_canonical")
}

#' @export
arrange_col_canonical.default <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_abort(c(
    "`arrange_col_canonical()` is only meaningful for an {.cls epi_df}."
  ))
  x
}

#' @export
arrange_col_canonical.epi_df <- function(x, ...) {
  rlang::check_dots_empty()
  all_names <- names(x)
  key_names <- key_colnames(x)
  val_names <- all_names[!all_names %in% key_names]
  x[c(key_names, val_names)]
}

#' Group an `epi_df` object by default keys
#' @param x an `epi_df`
#' @param exclude character vector of column names to exclude from grouping
#' @return a grouped `epi_df`
#' @export
group_epi_df <- function(x, exclude = character()) {
  cols <- key_colnames(x, exclude = exclude)
  reclass(grouped_df(x, cols), attr(x, "metadata"))
}

#' Aggregate an `epi_df` object
#'
#' Aggregates an `epi_df` object by the specified group columns, summing the
#' `value` column, and returning an `epi_df`. If aggregating over `geo_value`,
#' the resulting `epi_df` will have `geo_value` set to `"total"`.
#'
#' @param .x an `epi_df`
#' @param sum_cols `r tidyselect_arg_roxygen`
#' @param group_cols character vector of column names to group by. "time_value" is
#'   included by default.
#' @return an `epi_df` object
#'
#' @examples
#' # This data has other_keys age_group and edu_qual:
#' grad_employ_subset
#'
#' # Aggregate num_graduates within each geo_value (and time_value):
#' grad_employ_subset %>%
#'   sum_groups_epi_df(num_graduates, group_cols = "geo_value")
#'
#' @export
sum_groups_epi_df <- function(.x, sum_cols, group_cols = "time_value") {
  assert_class(.x, "epi_df")
  assert_character(group_cols)
  checkmate::assert_subset(group_cols, key_colnames(.x))
  if (!"time_value" %in% group_cols) {
    group_cols <- c("time_value", group_cols)
  }
  # Attempt tidyselection ourselves to get "Error in `sum_groups_epi_df()`"
  # rather than "in `dplyr::summarize()`", before forwarding:
  sum_cols <- rlang::enquo(sum_cols)
  tidyselect::eval_select(sum_cols, .x)
  out <- group_by(.x, across(all_of(group_cols))) %>%
    dplyr::summarize(across(!!sum_cols, sum), .groups = "drop")

  # To preserve epi_df-ness, we need to ensure that the `geo_value` column is
  # present.
  if (!"geo_value" %in% group_cols) {
    out <- out %>%
      mutate(geo_value = "total") %>%
      relocate(.data$geo_value, .before = 1)
  }

  # The `geo_type` will be correctly inherited here by the following logic:
  # - if `geo_value` is in `group_cols`, then the constructor will see the
  #   geo_value here and will correctly read the existing values
  # - if `geo_value` is not in `group_cols`, then the constructor will see
  #   the unrecognizeable "total" value and will correctly infer the "custom"
  #   geo_type.
  as_epi_df(
    out,
    as_of = attr(.x, "metadata")$as_of,
    other_keys = intersect(attr(.x, "metadata")$other_keys, group_cols)
  ) %>%
    arrange_canonical()
}
