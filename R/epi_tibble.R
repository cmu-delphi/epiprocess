#' Convert data to `epi_tibble` format
#'
#' Converts a data frame or tibble into a format that is consistent with the 
#' `epi_tibble` class, ensuring that it has a certain minimal set of columns,
#' and that it has certain minimal metadata.
#'
#' @param x The object to be converted. See the methods section below for
#'   details on formatting of each input type.
#' @param geo_type The type for the geo values. If missing, then the function
#'   will attempt to infer it from the geo values present; if this fails, then
#'   it will be set to "custom".  
#' @param time_type The type for the time values. If missing, then the function
#'   will attempt to infer it from the time values present; if this fails, then
#'   it will be set to "custom". 
#' @param issue Issue to use for this data. If missing, then the function will
#'   attempt to infer it from the passed object `x`; if this fails, then the
#'   current day-time will be used. 
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_tibble` object. All objects will have `time_type`, `geo_type`, and
#'   `issue` fields; named entries from the passed list or will be included as
#'   well.
#' @param ... Additional arguments passed to methods.
#' @return An `epi_tibble` object.
#'
#' @details An `epi_tibble` is a tibble with (at least) the following columns:  
#' 
#' * `geo_value`: the geographic value associated with each measurement.
#' * `time_value`: the time value associated with each measurement.
#'
#' Other columns can be considered as measured variables, which we also broadly
#'   refer to as signal variables. An `epi_tibble` object also has metadata with
#'   (at least) the following fields: 
#' 
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `issue`: the time value at which the given data set was issued.
#'
#' The first two fields above, `geo_type` and `time_type`, can usually be
#'   inferred from the `geo_value` and `time_value` columns, respectively. The
#'   last field above, `issue`, is the most unique to the `epi_tibble` format.
#'   In a typical case, this represents the maximum of the issues of individual
#'   signal values measured in the data set; hence we would also say that the
#'   data set is comprised of all signal values observed "as of" the given issue
#'   in the metadata.
#'
#' Metadata for an `epi_tibble` object `x` can be accessed (and altered) via
#'   `attributes(x)$metadata`. More information on geo types, time types, and
#'   issues is given below.   
#'
#' @section Geo types:
#' The following geo types are supported in an `epi_tibble`. Their geo coding 
#'   (specification of geo values for each geo type) is also described below.      
#' 
#' * `"county"`: each observation corresponds to a U.S. county; coded by 5-digit
#'   FIPS code. 
#' * `"hrr"`: each observation corresponds to a U.S. hospital referral region
#'   (designed to represent regional healthcare markets); there are 306 HRRs in
#'   the U.S; coded by number (nonconsecutive, between 1 and 457).
#' * `"state"`: each observation corresponds to a U.S. state; coded by 2-digit
#'   postal abbreviation (lowercase); 
#'   note that Puerto Rico is "pr" and Washington D.C. is "dc".  
#' * `"hhs"`: each observation corresponds to a U.S. HHS region; coded by number
#'   (consecutive, between 1 and 10).
#' * `"nation"`: each observation corresponds to a country; coded by ISO 31661-
#'   alpha-2 country codes (lowercase).
#'
#' The above geo types come with aggregation utilities in the package, *todo:
#'   refer to relevant functionality, vignette, and so on*. An unrecognizable 
#'   geo type is labeled as "custom".  
#' 
#' @section Time types:
#' The following time types are supported in an `epi_tibble`. Their time coding 
#'   (specification of time values for each time type) is also described below.
#' 
#' * `"day-time"`: each observation corresponds to a time on a given day (measured
#'   to the second); coded as a `POSIXct` object, as in `as.POSIXct("2020-06-09
#'   18:45:40")`.  
#' * `"day"`: each observation corresponds to a day; coded as a `Date` object,
#'   as in `as.Date("2020-06-09")`.
#' * `"week"`: each observation corresponds to a week; the alignment can be
#'   arbitrary (as to whether a week starts on a Monday, Tuesday, etc.; the
#'   U.S. CDC definition of an epidemiological week starts on a Sunday); coded
#'   as a `Date` object, representing the start date of week. 
#'
#' An unrecognisable time type is labeled as "custom".
#' 
#' @section Issues: 
#' todo
#' 
#' @export
as.epi_tibble = function(x, ...) {
  UseMethod("as.epi_tibble")
}

#' @method as.epi_tibble epi_tibble
#' @describeIn as.epi_tibble Simply returns the `epi_tibble` object unchanged.
#' @export
as.epi_tibble.epi_tibble = function(x, ...) {
  return(x)
}

#' @method as.epi_tibble tibble
#' @describeIn as.epi_tibble The input tibble `x` must contain the columns
#'   `geo_value` and `time_value`. All other columns will be preserved as is,
#'   and treated as measured variables. If `issue` is missing, then the function
#'   will look for `issue` as a column of `x`, or as a field in its metadata
#'   (stored in its attributes), to infer the issue; if this fails, then the
#'   current day-time will be used. 
#' @importFrom rlang .data abort
#' @export
as.epi_tibble.tibble = function(x, geo_type, time_type, issue,
                                additional_metadata = list(), ...) {
  # Check that we have geo_value and time_value columns
  if (!("geo_value" %in% names(x))) {
    abort("`x` must contain a `geo_value` column.")
  }
  if (!("time_value" %in% names(x))) {
    abort("`x` must contain a `time_value` column.")
  }

  # If geo type is missing, then try to guess it
  if (missing(geo_type)) {
    if (is.character(x$geo_value)) {
      # Convert geo values to lowercase
      x$geo_value = tolower(x$geo_value)
      
      # If all geo values are state abbreviations, then use "state" 
      state_values = c(tolower(state.abb), "as", "dc", "gu", "mp", "pr", "vi")
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
    else geo_type = "custom" 
  }

  # If time type is missing, then try to guess it
  if (missing(time_type)) {
    # Convert character time values to Date or POSIXct
    if (is.character(x$time_value)) {
      if (nchar(x$time_value[1]) <= "10") {
        new_time_value = tryCatch({ as.Date(x$time_value) },
                                  error = function(e) NULL)
      }
      else {
        new_time_value = tryCatch({ as.POSIXct(x$time_value) },
                                  error = function(e) NULL)
      }
      if (!is.null(new_time_value)) x$time_value = new_time_value
    }
    
    # Now, if a POSIXct class, then use "day-time"
    if (inherits(x$time_value, "POSIXct")) time_type = "day-time"

    # Else, if a Date class, then use "week" or "day" depending on gaps 
    else if (inherits(x$time_value, "Date")) {
      time_type = ifelse(all(diff(sort(x$time_value)) == -7), "week", "day")
    }

    # If we got here then we failed
    else time_type = "custom" 
  }

  # If issue is missing, then try to guess it
  if (missing(issue)) {
    # First check for a column, and take the maximum of issues
    if ("issue" %in% names(x)) issue = max(x$issue)

    # Next, check the metadata
    else if ("issue" %in% names(attributes(x$metadata))) {
      issue = attributes(x)$metadata$issue
    }

    # If we got here then we failed 
    else issue = Sys.time() # Use the current day-time
  }

  # Define metadata fields
  metadata = list()
  metadata$geo_type = geo_type
  metadata$time_type = time_type
  metadata$issue = issue
  metadata = c(metadata, additional_metadata)
 
  # Convert to a tibble, apply epi_tibble class, attach metadata
  if (!inherits(x, "tibble")) x = tibble::as_tibble(x)
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  
  # Reorder columns (geo_value, time_value, ...) and return
  x = dplyr::relocate(x, .data$geo_value, .data$time_value)
  return(x)
}

#' @method as.epi_tibble data.frame
#' @describeIn as.epi_tibble The input data frame `x` must contain the columns 
#'   `geo_value` and `time_value`. All other columns will be preserved as is,
#'   and treated as measured variables. If `issue` is missing, then the function
#'   will look for `issue` as a column of `x`, or as a field in its metadata
#'   (stored in its attributes), to infer the issue; if this fails, then the
#'   current day-time will be used. 
#' @export
as.epi_tibble.data.frame = as.epi_tibble.tibble

#' Print `epi_tibble` object
#'
#' Prints a brief summary of the `epi_tibble` object, then prints the underlying
#' tibble. 
#'
#' @param x The `epi_tibble` object.
#' @param ... Additional arguments passed to `print.tibble()` to print the
#'   data.
#' @return The `epi_tibble` object, unchanged.
#'
#' @method print epi_tibble
#' @export
print.epi_tibble = function(x, ...) {
  cat("An `epi_tibble` object, with metadata:\n")
  cat(sprintf("* %-10s= %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-10s= %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-10s= %s\n", "issue", attributes(x)$metadata$issue))
  cat("\n")
  NextMethod()
}

#' @method head epi_tibble
#' @importFrom utils head
#' @export
head.epi_tibble = function(x, ...) {
  head(tibble::as_tibble(x), ...)
}

#' Summarize `epi_tibble` object
#'
#' Prints a variety of summary statistics about the `epi_tibble` object, such as
#' the time range included and geographic coverage.
#'
#' @param object The `epi_tibble` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#' @return No return value; called only to print summary statistics.
#'
#' @method summary epi_tibble
#' @importFrom stats median
#' @export
summary.epi_tibble = function(object, ...) {
  cat("An `epi_tibble` object, with metadata:\n")
  cat(sprintf("* %-10s= %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-10s= %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-10s= %s\n", "issue", attributes(x)$metadata$issue))
  cat("\nSummary of space-time coverge:\n")
  cat(sprintf("* %-33s= %s\n", "earliest time value", min(object$time_value)))
  cat(sprintf("* %-33s= %s\n", "latest time value", max(object$time_value)))
  cat(sprintf("* %-33s= %i\n", "median geo values per time value",
              as.integer(object %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(median(.data$num)))))
}

#' Group or ungroup `epi_tibble` object
#'
#' Groups or ungroups an `epi_tibble`, preserving class and attributes.  
#'
#' @method group_by epi_tibble
#' @importFrom dplyr group_by
#' @export
group_by.epi_tibble = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method ungroup epi_tibble
#' @rdname group_by.epi_tibble
#' @importFrom dplyr ungroup
#' @export
ungroup.epi_tibble = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}
