#' Wrappers to functions that remove `NA` values by default
#' @export
Min = function(x) min(x, na.rm = TRUE)

#' @rdname Min
#' @export
Max = function(x) max(x, na.rm = TRUE)

#' @rdname Min
#' @export
Sum = function(x) sum(x, na.rm = TRUE)

#' @rdname Min
#' @export
Mean = function(x) mean(x, na.rm = TRUE)

#' @rdname Min
#' @export
Median = function(x) median(x, na.rm = TRUE)

##########

#' @export
#' @noRd
Start = function(x) head(x, 1)

#' @export
#' @noRd
End = function(x) tail(x, 1)

#' @export
#' @noRd
quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

##########

# TODO: fix. this function is no longer in sync with the epi_signal format.
# Currently not being exported

#' Fetch the latest or for each observation
#'
#' The data returned from `covidcast_signal()` or `covidcast_signals()` can, if
#' called with the `issues` argument, contain multiple issues for a single
#' observation in a single location. These functions filter the data frame to
#' contain only the latest issue.
#'
#' @param df A `covidcast_signal` or `covidcast_signal_long` data frame, such as
#'   returned from `covidcast_signal()` or the "long" format of
#'   `aggregate_signals()`.
#' @return A data frame in the same form, but with only the latest issue of
#'   every observation. Note that these functions sort the data frame as part of
#'   their filtering, so the output data frame rows may be in a different
#'   order. 
#' 
latest_issue = function(x) {
  if (!inherits(x, "epi_signal")) {
    abort("`x` of class `epi_signal`.")
  }

  # Save the metadata, since dplyr drops it
  metadata = attributes(x)$metadata

  x = x %>%
    dplyr::arrange(x, dplyr::desc(.data$issue)) %>%
    dplyr::distinct(.data$geo_value, .data$time_value, .keep_all = TRUE)

  attributes(x)$metadata = metadata
  return(x)
}
