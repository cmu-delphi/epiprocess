#' Convert to tsibble format
#' 
#' Converts an `epi_df` object into a tsibble, where the index is taken to be 
#' `time_value`, and the key variables taken to be `geo_value` along with any
#' others in the `other_keys` field of the metadata, or else explicitly set. 
#'
#' @method as_tsibble epi_df
#' @param x The `epi_df` object.
#' @param key Optional. Any additional keys (other than `geo_value`) to add to 
#'   the `tsibble`.
#' @param ... additional arguments passed on to `tsibble::as_tsibble()`
#' @export
as_tsibble.epi_df = function(x, key, ...) {
  if (missing(key)) key = c("geo_value", attributes(x)$metadata$other_keys)
  return(as_tsibble(tibble::as_tibble(x),
                    key = tidyselect::all_of(key), index = "time_value",
                    ...))
}

#' Basic S3 methods for an `epi_df` object
#' @name basic_s3_for_epi_df
NULL

#' @importFrom pillar tbl_sum
#' @rdname basic_s3_for_epi_df
#' @export
tbl_sum.epi_df = function(x, ...) {
  c("An epi_df" = pillar::dim_desc(x),
    purrr::map_chr(attr(x, "metadata"), toString))
}

#' Summarize `epi_df` object
#'
#' Prints a variety of summary statistics about the `epi_df` object, such as
#' the time range included and geographic coverage.
#'
#' @param object The `epi_df` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#'
#' @method summary epi_df
#' @rdname basic_s3_for_epi_df
#' @importFrom rlang .data
#' @importFrom stats median
#' @export
summary.epi_df = function(object, ...) {
  cat("An `epi_df` x, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(object)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "time_type", attributes(object)$metadata$time_type))
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(object)$metadata$as_of))
  cat("----------\n")
  cat(sprintf("* %-27s = %s\n", "min time value", min(object$time_value)))
  cat(sprintf("* %-27s = %s\n", "max time value", max(object$time_value)))
  cat(sprintf("* %-27s = %i\n", "average rows per time value",
              as.integer(object %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(mean(.data$num)))))
}

#' Drop any `epi_df` metadata and `epi_df`/`grouped_edf` class on a data frame
#'
#' Useful in implementing `?dplyr_extending` when manipulations cause invariants
#' of `epi_df`s to be violated and we need to return some other class. Note that
#' this will maintain any grouping (keeping the `grouped_df` class and
#' associated attributes, if present).
#'
#' @param x an `epi_df` or other data frame
#' @return `x` with any metadata dropped and the `"epi_df"` and `"grouped_edf"`
#'   classes, if previously present, dropped
#'
#' @noRd
decay_epi_df = function(x) {
  attributes(x)$metadata <- NULL
  class(x) <- class(x)[! class(x) %in% c("grouped_edf", "epi_df")]
  x
}

# Implementing `dplyr_extending`: we have a few metadata attributes to consider:
# `as_of` is an attribute doesn't depend on the rows or columns, `geo_type` and
# `time_type` are scalar attributes dependent on columns, and `other_keys` acts
# like an attribute vectorized over columns; `dplyr_extending` advice at time of
# writing says to implement `dplyr_reconstruct`, 1d `[`, `dplyr_col_modify`, and
# `names<-`, but not `dplyr_row_slice`; however, we'll also implement
# `dplyr_row_slice` anyway to prevent a `arrange` on grouped `epi_df`s from
# dropping the `epi_df` class. We'll implement `[` to allow either 1d or 2d.
# We'll also implement some other methods where we want to (try to) maintain an
# `epi_df`.

#' @param data tibble or `epi_df` (`dplyr` feeds in former, but we may
#'   directly feed in latter from our other methods)
#' @param template `epi_df` template to use to restore
#' @return `epi_df` or degrade into `tbl_df`
#' @importFrom dplyr dplyr_reconstruct
#' @export
#' @noRd
dplyr_reconstruct.epi_df = function(data, template) {
  # Start from a reconstruction for the backing S3 classes; this ensures that we
  # keep any grouping that has been applied:
  res <- NextMethod()
  
  cn <- names(res)

  # Duplicate columns, Abort
  dup_col_names = cn[duplicated(cn)]
  if (length(dup_col_names) != 0) {
    Abort(paste0("Column name(s) ", 
                 paste(unique(dup_col_names), 
                       collapse = ", "), " must not be duplicated."))
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
  
  res <- reclass_epi_df(res, attr(template, "metadata"))

  # XXX we may want verify the `geo_type` and `time_type` here. If it's
  # significant overhead, we may also want to keep this less strict version
  # around and implement some extra S3 methods that use it, when appropriate.
  
  # Amend additional metadata if some other_keys cols are dropped in the subset
  old_other_keys = attr(template, "metadata")$other_keys
  attr(res, "metadata")$other_keys <- old_other_keys[old_other_keys %in% cn]
  
  res
}

#' @export
`[.epi_df` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()
  
  if (!is.data.frame(res)) return(res)
  
  dplyr::dplyr_reconstruct(res, x)
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epi_df = function(data, cols) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epi_df = function(data, i, ...) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

#' @export
`names<-.epi_df` = function(x, value) {
  old_names = names(x)
  old_other_keys = attributes(x)$metadata$other_keys
  result = NextMethod()
  attributes(x)$metadata$other_keys <- value[match(old_other_keys, old_names)]
  dplyr::dplyr_reconstruct(result, x)
}

#' @method unnest epi_df
#' @rdname basic_s3_for_epi_df
#' @param data The `epi_df` object.
#' @export
unnest.epi_df = function(data, ...) {
  dplyr::dplyr_reconstruct(NextMethod(), data)
}

#' Reinterpretation of `x` as inheriting `subclass` if it doesn't already
#'
#' @param x an S3 object
#' @param subclass string; S3 class name we want to ensure that the result will
#'   [`base::inherit`]. We call this a "subclass" because we generally expect it
#'   to be in the beginning or middle of a `class` vector, followed by some
#'   other S3 class(es). If `x` already inherits this subclass, we return `x`
#'   unchanged.
#' @param potential_subsubclasses Optional; character vector: if `class(x)`
#'   doesn't already include `subclass`, it should be added in the first
#'   position after all of the `potential_subsubclasses` that appear in
#'   `class(x)`. This lets us control the order of `subclass` relative to
#'   "base"-like classes such as `"tbl_df"` and other wrapper classes like
#'   `"grouped_df"`. Will be ignored if `subclass` is already in `class(x)`.
#' @return object that inherits `subclass`
as_inheriting = function(x, subclass, potential_subsubclasses=character(0L)) {
  if(inherits(x, subclass)) {
    return(x)
  } else {
    insertion_index =
      max(0L, match(potential_subsubclasses, class(x)), na.rm = TRUE) + 1L
    result = x
    class(result) <- c(
      class(x)[seq_len(insertion_index - 1L)],
      subclass,
      class(x)[insertion_index - 1L +
                 seq_len(length(class(x)) - insertion_index + 1L)]
    )
    return(result)
  }
}

#' Simple reclass function for `epi_df`
#'
#' @param x Object to reclass to `epi_df`
#' @param metadata List; `epi_df` metadata to assign
#' @param potential_subsubclasses As in [`as_inheriting`]
#' @return An `epi_df`
#'
#' @noRd
reclass_epi_df = function(x, metadata, potential_subsubclasses=character(0L)) {
  result = as_inheriting(x, "epi_df", potential_subsubclasses)
  attributes(result)$metadata = metadata
  return(result)
}

