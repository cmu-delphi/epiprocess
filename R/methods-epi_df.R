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
  return(as_tsibble(tibble::as_tibble(x), key, index = "time_value", ...))
}

#' Base S3 methods for an `epi_df` object
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

#' @export
`[.epi_df` <- function(x, i, j, drop = FALSE) { 
  res <- NextMethod() 
  
  if (!is.data.frame(res)) return(res)
  
  i_arg <- substitute(i) 
  j_arg <- substitute(j) 
  
  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }
  
  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }
  
  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop) 
  
  # Case when the number of args (excluding drop) is not 3 or more
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
  }
  
  cn <- names(res)
  nr <- vctrs::vec_size(x) 
  not_epi_df <- !("time_value" %in% cn) || !("geo_value" %in% cn) || 
    vctrs::vec_size(res) > nr || any(i > nr)
  if (not_epi_df) return(tibble::as_tibble(res))
  
  # Case when i is numeric and there are duplicate values in it
  if (is.numeric(i) && vctrs::vec_duplicate_any(i) > 0) 
    return(tibble::as_tibble(res))
  
  # Column subsetting only, then return res as tibble
  if (rlang::is_null(i) && !rlang::is_null(j)) 
    return(tibble::as_tibble(res))
  
  att_x = attr(x, "metadata")
  new_epi_df(tibble::as_tibble(res), 
             geo_type = att_x$geo_type, 
             time_type = att_x$time_type, 
             as_of = att_x$as_of,
             additional_metadata = list(att_x$additional_metadata))
}

#' `dplyr` verbs
#'
#' `dplyr` verbs for `epi_df` objects, preserving class and attributes. 
#'
#' @method arrange epi_df
#' @param .data The `epi_df` object.
#' @rdname print.epi_df
#' @export
arrange.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method filter epi_df
#' @rdname print.epi_df
#' @export
filter.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method group_by epi_df
#' @rdname print.epi_df
#' @export
group_by.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method group_modify epi_df
#' @rdname print.epi_df
#' @export
group_modify.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method mutate epi_df
#' @rdname print.epi_df
#' @export
mutate.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method relocate epi_df
#' @rdname print.epi_df
#' @export
relocate.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method rename epi_df
#' @rdname print.epi_df
#' @export
rename.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method slice epi_df
#' @rdname print.epi_df
#' @export
slice.epi_df = function(.data, ...) {
  metadata = attributes(.data)$metadata
  .data = NextMethod()
  reclass(.data, metadata)
}

#' @method ungroup epi_df
#' @rdname print.epi_df
#' @export
ungroup.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  reclass(x, metadata)
}

#' @method unnest epi_df
#' @rdname print.epi_df
#' @param data The `epi_df` object.
#' @export
unnest.epi_df = function(data, ...) {
  metadata = attributes(data)$metadata
  data = NextMethod()
  reclass(data, metadata)
}

# Simple reclass function
reclass = function(x, metadata) {
  class(x) = unique(c("epi_df", class(x)))
  attributes(x)$metadata = metadata
  return(x)
}
