#' Convert to tsibble format
#' 
#' Converts an `epi_df` object into a tsibble, where the index is taken to be 
#' `time_value`, and the key variables taken to be `geo_value` along with any
#' others in the `other_keys` field of the metadata, or else explicitly set. 
#'
#' @importFrom tsibble as_tsibble
#' @method as_tsibble epi_df
#' @export
as_tsibble.epi_df = function(x, key, ...) {
  if (missing(key)) key = c("geo_value", attributes(x)$metadata$other_keys)
  return(as_tsibble(tibble::as_tibble(x), key, index = "time_value", ...))
}

#' S3 methods for an `epi_df` object
#'
#' Print, summary, and `dplyr` verbs (that preserve class and attributes) for an
#' `epi_df` object.
#'
#' @param x The `epi_df` object.
#' @param ... Additional arguments passed to methods.
#'
#' @method print epi_df
#' @export
print.epi_df = function(x, ...) {
  cat("An `epi_df` object, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(x)$metadata$as_of))
  cat("\n")
  NextMethod()
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
#' @rdname print.epi_df
#' @importFrom rlang .data
#' @importFrom stats median
#' @export
summary.epi_df = function(x, ...) {
  cat("An `epi_df` x, with metadata:\n")
  cat(sprintf("* %-9s = %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-9s = %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-9s = %s\n", "as_of", attributes(x)$metadata$as_of))
  cat("----------\n")
  cat(sprintf("* %-27s = %s\n", "min time value", min(x$time_value)))
  cat(sprintf("* %-27s = %s\n", "max time value", max(x$time_value)))
  cat(sprintf("* %-27s = %i\n", "average rows per time value",
              as.integer(x %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(mean(.data$num)))))
}

#' @method head epi_df
#' @export
#' @noRd
head.epi_df = function(x, ...) {
  head(tibble::as_tibble(x), ...)
}

#' @method tail epi_df
#' @export
#' @noRd
tail.epi_df = function(x, ...) {
  tail(tibble::as_tibble(x), ...)
}

#' `dplyr` verbs
#'
#' `dplyr` verbs for `epi_df` objexts, preserving class and attributes. 
#'
#' @method arrange epi_df
#' @rdname print.epi_df
#' @importFrom dplyr arrange
#' @export
arrange.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method filter epi_df
#' @rdname print.epi_df
#' @importFrom dplyr filter
#' @export
filter.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method group_by epi_df
#' @rdname print.epi_df
#' @importFrom dplyr group_by
#' @export
group_by.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method group_modify epi_df
#' @rdname print.epi_df
#' @importFrom dplyr group_modify
#' @export
group_modify.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method mutate epi_df
#' @rdname print.epi_df
#' @importFrom dplyr mutate
#' @export
mutate.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method relocate epi_df
#' @rdname print.epi_df
#' @importFrom dplyr relocate
#' @export
relocate.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method rename epi_df
#' @rdname print.epi_df
#' @importFrom dplyr rename
#' @export
rename.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method slice epi_df
#' @rdname print.epi_df
#' @importFrom dplyr slice
#' @export
slice.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method ungroup epi_df
#' @rdname print.epi_df
#' @importFrom dplyr ungroup
#' @export
ungroup.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method unnest epi_df
#' @rdname print.epi_df
#' @importFrom tidyr unnest
#' @export
unnest.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

# Simple reclass function
reclass = function(x, metadata) {
  class(x) = unique(c("epi_df", class(x)))
  attributes(x)$metadata = metadata
  return(x)
}
