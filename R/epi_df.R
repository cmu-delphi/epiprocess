#' Create `epi_df` object
#'
#' Creates an `epi_df` object from given `geo_value` and `time_value` variables,
#' and any additional number of variables.
#'
#' @param geo_value Geographic values associated with the measurements.
#' @param time_value Time values associated with the measurements.
#' @param ... Additional arguments of the form `value` or `name = value`, which
#'   specify any number of additional columns for the `epi_df` object.
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param as_of Time value representing the time at which the given data were
#'   available. For example, if `as_of` were January 31, 2022, then the `epi_df`
#'   object that is created would represent the most up-to-date version of the
#'   data available as of January 31, 2022. If the `as_of` argument is missing,
#'   then the current day-time will be used.
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_df` object. The metadata will have `time_type`, `geo_type`, and
#'   `as_of` fields; named entries from the passed list or will be included as
#'   well.
#'
#' @details An `epi_df` is a tibble with (at least) the following columns:  
#' 
#' * `geo_value`: the geographic value associated with each measurement.
#' * `time_value`: the time value associated with each measurement.
#'
#' Other columns can be considered as measured variables, which we also broadly
#'   refer to as signal variables. An `epi_df` object also has metadata with (at
#'   least) the following fields:
#' 
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `as_of`: the time value at which the given data were available.
#'
#' Metadata for an `epi_df` object `x` can be accessed (and altered) via
#'   `attributes(x)$metadata`. The first two fields in the above list,
#'   `geo_type` and `time_type`, can usually be inferred from the `geo_value`
#'   and `time_value` columns, respectively. More information on their coding is
#'   given below. 
#'
#' The last field in the above list, `as_of`, is one of the most unique aspects
#'   of an `epi_df` object. In brief, we can think of an `epi_df` object as a
#'   single snapshot of a data set that contains the most up-to-date values of
#'   some signals of interest, as of the time specified in the `as_of` field.  A
#'   companion object is the `epi_archive` object, which contains the full
#'   version history of a given data set. Revisions are common in many types of
#'   epidemiological data streams, and paying attention to data revisions can be
#'   important for all sorts of downstream data analysis and modeling tasks. See
#'   the `epi_archive()` help file for more details on how data versioning works
#'   in the `epiprocess` package (including how to create `epi_df` objects, as
#'   data snapshots, from an `epi_archive` object).
#'
#' @section Geo types:
#' The following geo types are supported in an `epi_df`. Their geo coding
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
#' The following time types are supported in an `epi_df`. Their time coding
#'   (specification of time values for each time type) is also described below.
#' 
#' * `"day-time"`: each observation corresponds to a time on a given day
#'   (measured to the second); coded as a `POSIXct` object, as in
#'   `as.POSIXct("2022-01-31 18:45:40")`.
#' * `"day"`: each observation corresponds to a day; coded as a `Date` object,
#'   as in `as.Date("2022-01-31")`.
#' * `"week"`: each observation corresponds to a week; the alignment can be
#'   arbitrary (as to whether a week starts on a Monday, Tuesday, etc.; the
#'   U.S. CDC definition of an epidemiological week starts on a Sunday); coded
#'   as a `Date` object, representing the start date of week.
#'
#' An unrecognizable time type is labeled as "custom". *todo: refer to vignette
#'   for time aggregation examples*
#'
#' @export
epi_df = function(geo_value, time_value, ..., geo_type, time_type, as_of,
                  additional_metadata = list()) {
  x = tibble::tibble(geo_value = geo_value, time_value = time_value, ...)
  return(as_epi_df(x, geo_type, time_type, as_of, additional_metadata))
}

#' Convert data to `epi_df` format
#'
#' Converts a data frame or tibble into a format that is consistent with the 
#' `epi_df` class, ensuring that it has a certain minimal set of columns, and
#' that it has certain minimal metadata.  
#'
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param as_of Time value representing the time at which the given data were
#'   available. For example, if `as_of` were January 31, 2022, then the `epi_df`
#'   object that is created would represent the most up-to-date version of the
#'   data available as of January 31, 2022. If the `as_of` argument is missing,
#'   then the function will attempt to infer it from the passed object `x`; if
#'   this fails, then the current day-time will be used.
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_df` object. The metadata will have `time_type`, `geo_type`, and
#'   `as_of` fields; named entries from the passed list or will be included as
#'   well.
#' @param ... Additional arguments passed to methods.
#' @return An `epi_df` object.
#' 
#' @export
as_epi_df = function(x, ...) {
  UseMethod("as_epi_df")
}

#' @method as_epi_df epi_df
#' @describeIn as_epi_df Simply returns the `epi_df` object unchanged.
#' @export
as_epi_df.epi_df = function(x, ...) {
  return(x)
}

#' @method as_epi_df tibble
#' @describeIn as_epi_df The input tibble `x` must contain the columns
#'   `geo_value` and `time_value`. All other columns will be preserved as is,
#'   and treated as measured variables. If `as_of` is missing, then the function
#'   will try to guess it from an `as_of`, `issue`, or `version` column of `x`
#'   (if any of these are present), or from as an `as_of` field in its metadata
#'   (stored in its attributes); if this fails, then the current day-time will
#'   be used. 
#' @importFrom rlang .data abort
#' @export
as_epi_df.tibble = function(x, geo_type, time_type, as_of,
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

  # If as_of is missing, then try to guess it
  if (missing(as_of)) {
    # First check the metadata for an as_of field
    if ("as_of" %in% names(attributes(x$metadata))) {
      as_of = attributes(x)$metadata$as_of
    }
    
    # Next check for as_of, issue, or version columns
    else if ("as_of" %in% names(x)) as_of = max(x$as_of)
    else if ("issue" %in% names(x)) as_of = max(x$issue)
    else if ("version" %in% names(x)) as_of = max(x$version)

    # If we got here then we failed 
    else as_of = Sys.time() # Use the current day-time
  }

  # Define metadata fields
  metadata = list()
  metadata$geo_type = geo_type
  metadata$time_type = time_type
  metadata$as_of = as_of
  metadata = c(metadata, additional_metadata)
 
  # Convert to a tibble, apply epi_df class, attach metadata
  if (!inherits(x, "tibble")) x = tibble::as_tibble(x)
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  
  # Reorder columns (geo_value, time_value, ...) and return
  x = dplyr::relocate(x, .data$geo_value, .data$time_value)
  return(x)
}

#' @method as_epi_df data.frame
#' @describeIn as_epi_df The input data frame `x` must contain the columns
#'   `geo_value` and `time_value`. All other columns will be preserved as is,
#'   and treated as measured variables. If `as_of` is missing, then the function
#'   will try to guess it from an `as_of`, `issue`, or `version` column of `x`
#'   (if any of these are present), or from as an `as_of` field in its metadata
#'   (stored in its attributes); if this fails, then the current day-time will
#'   be used. 
#' @export
as_epi_df.data.frame = as_epi_df.tibble

#' Print `epi_df` object
#'
#' Prints a brief summary of the `epi_df` object, then prints the underlying
#' tibble.
#'
#' @param x The `epi_df` object.
#' @param ... Additional arguments passed to `print.tibble()` to print the
#'   data.
#' @return The `epi_df` object, unchanged.
#'
#' @method print epi_df
#' @export
print.epi_df = function(x, ...) {
  cat("An `epi_df` object, with metadata:\n")
  cat(sprintf("* %-10s= %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-10s= %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-10s= %s\n", "as_of", attributes(x)$metadata$as_of))
  cat("\n")
  NextMethod()
}

#' @method head epi_df
#' @importFrom utils head
#' @export
head.epi_df = function(x, ...) {
  head(tibble::as_tibble(x), ...)
}

#' Summarize `epi_df` object
#'
#' Prints a variety of summary statistics about the `epi_df` object, such as
#' the time range included and geographic coverage.
#'
#' @param object The `epi_df` object.
#' @param ... Additional arguments, for compatibility with `summary()`.
#'   Currently unused.
#' @return No return value; called only to print summary statistics.
#'
#' @method summary epi_df
#' @importFrom rlang .data
#' @importFrom stats median
#' @export
summary.epi_df = function(object, ...) {
  cat("An `epi_df` object, with metadata:\n")
  cat(sprintf("* %-10s= %s\n", "geo_type", attributes(x)$metadata$geo_type))
  cat(sprintf("* %-10s= %s\n", "time_type", attributes(x)$metadata$time_type))
  cat(sprintf("* %-10s= %s\n", "as_of", attributes(x)$metadata$as_of))
  cat("\nSummary of space-time coverge:\n")
  cat(sprintf("* %-33s= %s\n", "earliest time value", min(object$time_value)))
  cat(sprintf("* %-33s= %s\n", "latest time value", max(object$time_value)))
  cat(sprintf("* %-33s= %i\n", "median geo values per time value",
              as.integer(object %>% dplyr::group_by(.data$time_value) %>%
                         dplyr::summarize(num = dplyr::n()) %>%
                         dplyr::summarize(median(.data$num)))))
}

#' Group or ungroup `epi_df` object
#'
#' Groups or ungroups an `epi_df`, preserving class and attributes.  
#'
#' @method group_by epi_df
#' @importFrom dplyr group_by
#' @export
group_by.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' @method ungroup epi_df
#' @rdname group_by.epi_df
#' @importFrom dplyr ungroup
#' @export
ungroup.epi_df = function(x, ...) {
  metadata = attributes(x)$metadata
  x = NextMethod()
  class(x) = c("epi_df", class(x))
  attributes(x)$metadata = metadata
  return(x)
}
