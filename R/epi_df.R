#' @title `epi_df` object
#'
#' @description An `epi_df` is a tibble with certain minimal column structure
#'   and metadata. It can be seen as a snapshot of a data set that contains the
#'   most up-to-date values of some signal variables of interest, as of a given
#'   time.
#'
#' @details An `epi_df` is a tibble with (at least) the following columns:
#'
#' * `geo_value`: the geographic value associated with each row of measurements.
#' * `time_value`: the time value associated with each row of measurements.
#'
#' Other columns can be considered as measured variables, which we also refer to
#'   as signal variables. An `epi_df` object also has metadata with (at least)
#'   the following fields:
#'
#' * `geo_type`: the type for the geo values.
#' * `time_type`: the type for the time values.
#' * `as_of`: the time value at which the given data were available.
#'
#' Metadata for an `epi_df` object `x` can be accessed (and altered) via
#'   `attributes(x)$metadata`. The first two fields in the above list,
#'   `geo_type` and `time_type`, can usually be inferred from the `geo_value`
#'   and `time_value` columns, respectively. They are not currently used by any
#'   downstream functions in the `epiprocess` package, and serve only as useful
#'   bits of information to convey about the data set at hand. More information
#'   on their coding is given below.
#'
#' The last field in the above list, `as_of`, is one of the most unique aspects
#'   of an `epi_df` object. In brief, we can think of an `epi_df` object as a
#'   single snapshot of a data set that contains the most up-to-date values of
#'   the signals variables, as of the time specified in the `as_of` field.
#'
#' A companion object is the `epi_archive` object, which contains the full
#'   version history of a given data set. Revisions are common in many types of
#'   epidemiological data streams, and paying attention to data revisions can be
#'   important for all sorts of downstream data analysis and modeling tasks. See
#'   the documentation for [`epi_archive`][epi_archive] for more details on how
#'   data versioning works in the `epiprocess` package (including how to
#'   generate `epi_df` objects, as data snapshots, from an `epi_archive`
#'   object).
#'
#' @section Geo Types:
#' The following geo types are recognized in an `epi_df`.
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
#' An unrecognizable geo type is labeled "custom".
#'
#' @section Time Types:
#' The following time types are recognized in an `epi_df`.
#'
#' * `"day-time"`: each observation corresponds to a time on a given day
#'   (measured to the second); coded as a `POSIXct` object, as in
#'   `as.POSIXct("2022-01-31 18:45:40")`.
#' * `"day"`: each observation corresponds to a day; coded as a `Date` object,
#'   as in `as.Date("2022-01-31")`.
#' * `"week"`: each observation corresponds to a week; the alignment can be
#'   arbitrary (as to whether a week starts on a Monday, Tuesday); coded as a
#'   `Date` object, representing the start date of week.
#' * `"yearweek"`: each observation corresponds to a week; the alignment can be
#'   arbitrary; coded as a `tsibble::yearweek` object, where the alignment is
#'   stored in the `week_start` field of its attributes.
#' * `"yearmonth"`: each observation corresponds to a month; coded as a
#'   `tsibble::yearmonth` object.
#' * `"yearquarter"`: each observation corresponds to a quarter; coded as a
#'   `tsibble::yearquarter` object.
#' * `"year"`: each observation corresponds to a year; coded as an integer
#'   greater than or equal to 1582.
#'
#' An unrecognizable time type is labeled "custom".
#'
#' @name epi_df
NULL


#' Creates an `epi_df` object
#'
#' Creates a new `epi_df` object. By default, builds an empty tibble with the
#' correct metadata for an `epi_df` object (ie. `geo_type`, `time_type`, and `as_of`).
#' Refer to the below info. about the arguments for more details.
#'
#' @template epi_df-params
#'
#' @export
new_epi_df <- function(x = tibble::tibble(), geo_type, time_type, as_of,
                       additional_metadata = list(), ...) {
  assert_data_frame(x)
  assert_list(additional_metadata)

  additional_metadata[["other_keys"]] <- additional_metadata[["other_keys"]] %||% character(0L)

  # If geo type is missing, then try to guess it
  if (missing(geo_type)) {
    geo_type <- guess_geo_type(x$geo_value)
  }

  # If time type is missing, then try to guess it
  if (missing(time_type)) {
    time_type <- guess_time_type(x$time_value)
  }

  # If as_of is missing, then try to guess it
  if (missing(as_of)) {
    # First check the metadata for an as_of field
    if (
      "metadata" %in% names(attributes(x)) &&
        "as_of" %in% names(attributes(x)$metadata)
    ) {
      as_of <- attributes(x)$metadata$as_of
    } else if ("as_of" %in% names(x)) {
      # Next check for as_of, issue, or version columns
      as_of <- max(x$as_of)
    } else if ("issue" %in% names(x)) {
      as_of <- max(x$issue)
    } else if ("version" %in% names(x)) {
      as_of <- max(x$version)
    } else {
      # If we got here then we failed
      as_of <- Sys.time()
    } # Use the current day-time
  }

  # Define metadata fields
  metadata <- list()
  metadata$geo_type <- geo_type
  metadata$time_type <- time_type
  metadata$as_of <- as_of
  metadata <- c(metadata, additional_metadata)

  # Reorder columns (geo_value, time_value, ...)
  if (sum(dim(x)) != 0) {
    cols_to_put_first <- c("geo_value", "time_value")
    x <- x[, c(
      cols_to_put_first,
      # All other columns
      names(x)[!(names(x) %in% cols_to_put_first)]
    )]
  }

  # Apply epi_df class, attach metadata, and return
  class(x) <- c("epi_df", class(x))
  attributes(x)$metadata <- metadata
  return(x)
}

#' Convert to `epi_df` format
#'
#' Converts a data frame or tibble into an `epi_df` object. See the [getting
#' started
#' guide](https://cmu-delphi.github.io/epiprocess/articles/epiprocess.html) for
#' examples.
#'
#' @template epi_df-params
#'
#' @export
#' @examples
#' # Convert a `tsibble` that has county code as an extra key
#' # Notice that county code should be a character string to preserve any leading zeroes
#'
#' ex1_input <- tibble::tibble(
#'   geo_value = rep(c("ca", "fl", "pa"), each = 3),
#'   county_code = c(
#'     "06059", "06061", "06067",
#'     "12111", "12113", "12117",
#'     "42101", "42103", "42105"
#'   ),
#'   time_value = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
#'     by = "day"
#'   ), length.out = length(geo_value)),
#'   value = 1:length(geo_value) + 0.01 * rnorm(length(geo_value))
#' ) %>%
#'   tsibble::as_tsibble(index = time_value, key = c(geo_value, county_code))
#'
#' # The `other_keys` metadata (`"county_code"` in this case) is automatically
#' # inferred from the `tsibble`'s `key`:
#' ex1 <- as_epi_df(x = ex1_input, geo_type = "state", time_type = "day", as_of = "2020-06-03")
#' attr(ex1, "metadata")[["other_keys"]]
#'
#'
#'
#' # Dealing with misspecified column names:
#' # Geographical and temporal information must be provided in columns named
#' # `geo_value` and `time_value`; if we start from a data frame with a
#' # different format, it must be converted to use `geo_value` and `time_value`
#' # before calling `as_epi_df`.
#'
#' ex2_input <- tibble::tibble(
#'   state = rep(c("ca", "fl", "pa"), each = 3), # misnamed
#'   pol = rep(c("blue", "swing", "swing"), each = 3), # extra key
#'   reported_date = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
#'     by = "day"
#'   ), length.out = length(state)), # misnamed
#'   value = 1:length(state) + 0.01 * rnorm(length(state))
#' )
#'
#' print(ex2_input)
#'
#' ex2 <- ex2_input %>%
#'   dplyr::rename(geo_value = state, time_value = reported_date) %>%
#'   as_epi_df(
#'     geo_type = "state", as_of = "2020-06-03",
#'     additional_metadata = list(other_keys = "pol")
#'   )
#'
#' attr(ex2, "metadata")
#'
#'
#'
#' # Adding additional keys to an `epi_df` object
#'
#' ex3_input <- jhu_csse_county_level_subset %>%
#'   dplyr::filter(time_value > "2021-12-01", state_name == "Massachusetts") %>%
#'   dplyr::slice_tail(n = 6)
#'
#' ex3 <- ex3_input %>%
#'   tsibble::as_tsibble() %>% # needed to add the additional metadata
#'   # add 2 extra keys
#'   dplyr::mutate(
#'     state = rep("MA", 6),
#'     pol = rep(c("blue", "swing", "swing"), each = 2)
#'   ) %>%
#'   # the 2 extra keys we added have to be specified in the other_keys
#'   # component of additional_metadata.
#'   as_epi_df(additional_metadata = list(other_keys = c("state", "pol")))
#'
#' attr(ex3, "metadata")
as_epi_df <- function(x, ...) {
  UseMethod("as_epi_df")
}

#' @method as_epi_df epi_df
#' @describeIn as_epi_df Simply returns the `epi_df` object unchanged.
#' @export
as_epi_df.epi_df <- function(x, ...) {
  return(x)
}

#' @method as_epi_df tbl_df
#' @describeIn as_epi_df The input tibble `x` must contain the columns
#'   `geo_value` and `time_value`, or column names that uniquely map onto these
#'   (e.g. `date` or `province`). Alternatively, you can specify the conversion
#'   explicitly (`time_value = someWeirdColumnName`). All other columns not
#'   specified as `other_keys` will be preserved as is, and treated as measured
#'   variables.
#'
#'  If `as_of` is missing, then the function will try to guess it from an
#'   `as_of`, `issue`, or `version` column of `x` (if any of these are present),
#'   or from as an `as_of` field in its metadata (stored in its attributes); if
#'   this fails, then the current day-time will be used.
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
#' @importFrom cli cli_inform
#' @export
as_epi_df.tbl_df <- function(x, geo_type, time_type, as_of,
                             additional_metadata = list(),
                             ...) {
  # possible standard substitutions for time_value
  x <- rename(x, ...)
  x <- guess_time_column_name(x)
  x <- guess_geo_column_name(x)
  if (!test_subset(c("geo_value", "time_value"), names(x))) {
    cli_abort(
      "Either columns `geo_value` and `time_value` must be present in `x`, or related columns (see the internal functions `guess_time_column_name()` and/or `guess_geo_column_name()` for a complete list)."
    )
  }

  new_epi_df(
    x, geo_type, time_type, as_of,
    additional_metadata, ...
  )
}

#' @method as_epi_df data.frame
#' @describeIn as_epi_df Works analogously to `as_epi_df.tbl_df()`.
#' @export
as_epi_df.data.frame <- function(x, geo_type, time_type, as_of,
                                 additional_metadata = list(), ...) {
  as_epi_df.tbl_df(
    tibble::as_tibble(x), geo_type, time_type, as_of,
    additional_metadata, ...
  )
}

#' @method as_epi_df tbl_ts
#' @describeIn as_epi_df Works analogously to `as_epi_df.tbl_df()`, except that
#'   the `tbl_ts` class is dropped, and any key variables (other than
#'   "geo_value") are added to the metadata of the returned object, under the
#'   `other_keys` field.
#' @export
as_epi_df.tbl_ts <- function(x, geo_type, time_type, as_of,
                             additional_metadata = list(), ...) {
  tsibble_other_keys <- setdiff(tsibble::key_vars(x), "geo_value")
  if (length(tsibble_other_keys) != 0) {
    additional_metadata$other_keys <- unique(
      c(additional_metadata$other_keys, tsibble_other_keys)
    )
  }
  as_epi_df.tbl_df(
    tibble::as_tibble(x), geo_type, time_type, as_of,
    additional_metadata, ...
  )
}

#' Test for `epi_df` format
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from `epi_df`.
#'
#' @export
is_epi_df <- function(x) {
  inherits(x, "epi_df")
}
