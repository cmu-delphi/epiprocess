#' Convert data frame to `epi_signal` format
#'
#' Converts a data frame or tibble into a format consistent with the
#' `epi_signal` class (ensuring that it has a certain minimal set of columns, 
#' and that it has certain metadata stored in its attributes).
#'
#' @details An `epi_signal` object is simply a tibble, with (at least) the
#'   following columns (with data types written in tibble notation):    
#' * `value` <dbl>: the value of the signal
#' * `geo_value` <int> or <str>: the associated geographic value  
#' * `time_value` <date>: the associated time value 
#' * `issue` <date>: the time value at which the given signal value was issued 
#'
#' An `epi_signal` object also has a tibble `metadata` stored in its attributes,  
#' with (at least) the following columns:
#' * `name` <str>: the name of the signal
#' * `geo_type` <str>: the geographic resolution
#' * `time_type` <str>: the temporal resolution
#' * `signal_type` <str>: the type of the signal value (optional)
#' * `signal_unit` <str>: the units associated with the signal value (optional) 
#'
#' More information on geo types, time types, and signal types, is given
#' below. 
#' 
#' @section Geo types:
#' The allowable geo types, and their coding (allowable range for geo values), 
#'   are as follows.
#' * `county`: U.S. counties; coded by 5-digit FIPS codes.  
#' * `hrr`: U.S. hospital referral regions (designed to represent regional
#'   health care markets); there are 306 HRRs in the U.S; coded by number
#'   (nonconsecutive, between 1 and 457). 
#' * `state`: U.S. states; coded by 2-digit postal abbreviation (lowercase);
#'   note that Puerto Rico is "pr" and Washington D.C. is "dc".  
#' * `hhs`: U.S. HHS regions; coded by number (between 1 and 10).
#' * `nation`: country; coded by ISO 3166-1 alpha-2 country codes (lowercase). 
#'
#' @section Time types:
#' The allowable time types are as follows. In each case, their coding
#'   (allowable range for time values) is an object of class `Date`, or string
#'   in the format "YYYY-MM-DD".
#' * "day": each observation covers one day.
#' * "week": each observation covers one week, and the time value is assumed to 
#'   be the start date of the epiweek (MMWR week) that the data represents.
#'
#' @section Signal types:
#' todo
#'
#' @param x Object to be converted. See Methods section below for details on
#'   formatting of each input type.
#' @param name The name to use for this signal.
#' @param geo_type The geographic resolution. If missing, then it will be
#'   guessed from the geo values present.  
#' @param time_type The temporal resolution. If missing, then it will be guessed
#'   from the time values present. 
#' @param signal_type The type of the signal value. 
#' @param signal_unit The units of the signal value. 
#' @param issue Issue date to use for this data, if not present in `x`. If no
#'   issue date is present in `x` and `issue` is missing, then today's date will
#'   be used. 
#' @param metadata List or tibble of additional metadata to attach to the
#'   `epi_signal` object. All objects will have `geo_type`, `time_type`,
#'   `signal_type` (optional), and `signal_unit` (optional) entries included in
#'   their metadata, derived from the above arguments; any entries in the passed
#'   argument will be preserved in the metadata as well.
#' @param ... Additional arguments passed to methods.
#' @return An `epi_signal` object.
#' @export
as.epi_signal = function(x, ...) {
  UseMethod("as.epi_signal")
}

#' @method as.epi_signal epi_signal
#' @describeIn as.epi_signal Simply returns the `epi_signal` object unchanged.
#' @export
as.epi_signal.epi_signal = function(x, ...) {
  return(x)
}

#' @method as.epi_signal tibble
#' @describeIn as.epi_signal The input tibble `x` must contain the columns
#'   `value`, `geo_value`, and `time_value`. If an `issue` column is present in
#'   `x`, it will be used as the issue date for each observation; if not, the
#'   `issue` argument will be used. Other columns will be preserved as-is.
#' @importFrom rlang .data abort
#' @export
as.epi_signal.tibble = function(x, name, geo_type, time_type, signal_type,
                                signal_unit, issue, metadata = list(), ...) { 
  if (!("value" %in% names(x))) {
    abort(paste(
      "`x` must contain a `value` column",
      "containing the signal value of each observation."
    ), class = "epi_coerce_value")
  }

  if (!("geo_value" %in% names(x))) {
    abort(paste(
      "`x` must contain a `geo_value` column",
      "containing the geo location of each observation." 
    ), class = "epi_coerce_geo_value")
  }

  if (!("time_value" %in% names(x))) {
    abort(paste(
      "`x` must contain a `time_value` column",
      "containing the time value of each observation." 
    ), class = "epi_coerce_time_value")
  }

  if (missing(name)) {
    abort(
      "`name` must be specified.",
      class = "epi_coerce_name")
  }
  
  if (missing(geo_type)) {
    if (is.character(x$geo_value)) {
      # Convert geo values to lowercase
      x$geo_value = tolower(x$geo_value)
      
      # If all geo values are state abbreviations, then use "state" 
      state_values = c(tolower(state.abb), "pr", "dc")
      if (all(x$geo_value %in% state_values)) geo_type = "state"

      # Else if all geo values are 2 letters, then use "nation"
      else if (all(grepl("[a-z]{2}", x$geo_value))) geo_type = "nation"

      # Else if all geo values are 5 numbers, then use "county"
      else if (all(grepl("[0-9]{5}", x$geo_value))) geo_type = "county"
    }

    else if (is.numeric(x$geo_value)) {
      # Convert geo values to integers
      x$geo_value = as.integer(x$geo_value)

      # If the max geo value is at most 10, then use "hhs"
      if (max(x$geo_value) <= 10) geo_type = "hhs"
      
      # Else if the max geo value is at most 457, then use "hrr"
      if (max(x$geo_value) <= 457) geo_type = "hrr"
    }

    # If we got here then we failed
    else geo_type = "unknown" # TODO should we use NA? Or some other flag?
  }

  if (missing(time_type)) {
    # Convert time values to Date format
    x$time_value = as.Date(x$time_value)

    # If all time values are 7 days apart, then use "week"
    if (all(diff(sort(x$time_value, decreasing=TRUE)) == 7)) time_type = "week"

    # Otherwise just use "day"
    else time_type = "day"
  }

  # Define metadata fields
  metadata$name = name
  metadata$geo_type = geo_type
  metadata$time_type = time_type
  if (!missing(signal_type)) metadata$signal_type = signal_type
  if (!missing(signal_unit)) metadata$signal_unit = signal_unit
  metadata = tibble::as_tibble(metadata)
 
  # Convert to a tibble, apply epi_signal class, attach metadata
  x = tibble::as_tibble(x)
  class(x) = c("epi_signal", class(x))
  attributes(x)$metadata = metadata
  
  # Reorder columns: value, geo_value, time_value,
  x = dplyr::relocate(x,
                      .data$value,
                      .data$geo_value,
                      .data$time_value)

  # If no rows, then quit
  if (nrow(x) == 0) return(x)

  # Add issue column if we need to
  if (!("issue" %in% names(x))) {
    if (missing(issue)) x$issue = Sys.Date()
    x$issue = issue
  }

  # Reorder columns: issue after time_value
  x = dplyr::relocate(x, 
                      .data$issue,
                      .after = .data$time_value)

  return(x)
}

#' @method as.epi_signal data.frame
#' @describeIn as.epi_signal The input data frame `x` must contain the columns
#'   `value`, `geo_value`, and `time_value`. If an `issue` column is present in
#'   `x`, it will be used as the issue date for each observation; if not, the
#'   `issue` argument will be used. Other columns will be preserved as-is.
#' @export
as.epi_signal.data.frame = as.epi_signal.tibble

#' Print `epi_signal` object
#'
#' Prints a brief summary of the signal, then prints the underlying data frame
#' (tibble), for an `epi_signal` object.  
#'
#' @param x The `epi_signal` object.
#' @param ... Additional arguments passed to `print.tibble()` to print the
#'   data.
#' @return The `epi_signal` object, unchanged.
#'
#' @method print epi_signal
#' @export
print.epi_signal = function(x, ...) {
  cat(sprintf("An `epi_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-10s: %s\n", "name", attributes(x)$metadata$name))
  cat(sprintf("%-10s: %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("%-10s: %s\n", "time_type", attributes(x)$metadata$time_type))
  if (suppressWarnings(!is.null(attributes(x)$metadata$signal_type))) {
    cat(sprintf("%-10s: %s\n", "signal_type",
                attributes(x)$metadata$signal_type))
  }
  if (suppressWarnings(!is.null(attributes(x)$metadata$signal_unit))) {
    cat(sprintf("%-10s: %s\n", "signal_unit",
                attributes(x)$metadata$signal_unit))
  }
  cat("\n")
  NextMethod("print")
}

#' @method head epi_signal
#' @importFrom utils head
#' @export
head.epi_signal = function(x, ...) {
  head(tibble::as_tibble(x), ...)
}

#' Summarize `epi_signal` object
#'
#' Prints a variety of summary statistics about the underlying data, such as the
#' date range included and geographic coverage, for an `epi_signal` object.
#'
#' @param object The `epi_signal` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#' @return No return value; called only to print summary statistics.
#'
#' @method summary epi_signal
#' @importFrom stats median
#' @export
summary.epi_signal = function(object, ...) {
  cat(sprintf("An `epi_signal` data frame with %i rows and %i columns.\n\n",
              nrow(x), ncol(x)))
  cat(sprintf("%-10s: %s\n", "name", attributes(x)$metadata$name))
  cat(sprintf("%-10s: %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("%-10s: %s\n", "time_type", attributes(x)$metadata$time_type)) 
  if (suppressWarnings(!is.null(attributes(x)$metadata$signal_type))) {
    cat(sprintf("%-10s: %s\n", "signal_type",
                attributes(x)$metadata$signal_type))
  }
  if (suppressWarnings(!is.null(attributes(x)$metadata$signal_unit))) {
    cat(sprintf("%-10s: %s\n", "signal_unit",
                attributes(x)$metadata$signal_unit))
  }
  cat("\n")
  cat(sprintf("%-43s: %s\n", "first time value", min(object$time_value)))
  cat(sprintf("%-43s: %s\n", "last time value", max(object$time_value)))
  cat(sprintf("%-43s: %i\n", "median number of geo values per time value",
              as.integer(object %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(median(.data$num)))))
}

