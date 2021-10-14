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

#' Fetch the latest or earliest issue for each observation
#'
#' The data returned from `covidcast_signal()` or `covidcast_signals()` can, if
#' called with the `issues` argument, contain multiple issues for a single
#' observation in a single location. These functions filter the data frame to
#' contain only the earliest issue or only the latest issue.
#'
#' @param df A `covidcast_signal` or `covidcast_signal_long` data frame, such as
#'   returned from `covidcast_signal()` or the "long" format of
#'   `aggregate_signals()`.
#' @return A data frame in the same form, but with only the earliest or latest
#'   issue of every observation. Note that these functions sort the data frame
#'   as part of their filtering, so the output data frame rows may be in a
#'   different order.
#' 
#' @export
latest_issue = function(x) {
  if (!inherits(x, "epi_signal")) {
    abort("`x` of class `epi_signal`.")
  }

  # Save the attributes, such as metadata, since dplyr drops them
  attrs = attributes(x)
  attrs = attrs[!(names(attrs) %in% c("row.names", "names"))]

  x = x %>%
    dplyr::arrange(x, dplyr::desc(.data$issue)) %>%
    dplyr::distinct(.data$geo_value, .data$time_value, .keep_all = TRUE)

  attributes(x) = c(attributes(x), attrs)
  return(x)
}
