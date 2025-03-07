#' `epi_df` object
#'
#' One of the two main data structures for storing time series in `epiprocess`.
#' It is simply tibble with at least two columns, `geo_value` and `time_value`,
#' that provide the keys for the time series. It can have any other columns,
#' which can be seen as measured variables at each key. In brief, an `epi_df`
#' represents a snapshot of an epidemiological data set at a point in time.
#'
#' @details An `epi_df` is a tibble with (at least) the following columns:
#'
#' - `geo_value`: A character vector representing the geographical unit of
#'    observation. This could be a country code, a state name, a county code,
#'    etc.
#' - `time_value`: A date or integer vector representing the time of observation.
#'
#' Other columns can be considered as measured variables, which we also refer to
#'   as signal variables. An `epi_df` object also has metadata with (at least)
#'   the following fields:
#'
#' * `geo_type`: the type for the geo values.
#' * `as_of`: the time value at which the given data were available.
#'
#' Most users should use `as_epi_df`. The input tibble `x` to the constructor
#'   must contain the columns `geo_value` and `time_value`. All other columns
#'   will be preserved as is, and treated as measured variables. If `as_of` is
#'   missing, then the function will try to guess it from an `as_of`, `issue`,
#'   or `version` column of `x` (if any of these are present), or from as an
#'   `as_of` field in its metadata (stored in its attributes); if this fails,
#'   then the current day-time will be used. The `new_epi_df` constructor
#'   assumes its arguments have already been validated, so it should mainly be
#'   used by advanced users.
#'
#' Metadata for an `epi_df` object `x` can be accessed (and altered) via
#'   `attributes(x)$metadata`. The first field in the above list, `geo_type`,
#'   can usually be inferred from the `geo_value` columns. They are not
#'   currently used by any downstream functions in the `epiprocess` package,
#'   and serve only as useful bits of information to convey about the data set
#'   at hand. More information on their coding is given below.
#'
#' The last field in the above list, `as_of`, is one of the most unique aspects
#'   of an `epi_df` object. In brief, we can think of an `epi_df` object as a
#'   single snapshot of a data set that contains the most up-to-date values of
#'   the signals variables, as of the time specified in the `as_of` field.
#'
#' If an `epi_df` ever loses its `geo_value` or `time_value` columns, it will
#'   decay into a regular tibble.
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
#' ## Geo Types
#'
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
#' ## Time Types
#'
#' The following time types are recognized in an `epi_df`.
#'
#' * `"day"`: each observation corresponds to a day; coded as a `Date` object,
#'   as in `as.Date("2022-01-31")`.
#' * `"week"`: each observation corresponds to a week; the alignment can be
#'   arbitrary (as to whether a week starts on a Monday, Tuesday); coded as a
#'   `Date` object, representing the start date of week.
#' * `"yearmonth"`: each observation corresponds to a month; coded as a
#'   `tsibble::yearmonth` object.
#' * `"integer"`: a generic integer index (e.g. years or something else).
#'
#' An unrecognizable time type is labeled "custom".
#'
#' @name epi_df
#' @examples
#' # Convert a `tsibble` that has county code as an extra key
#' # Notice that county code should be a character string to preserve any leading zeroes
#' ex1_input <- tibble::tibble(
#'   geo_value = c(
#'     "06059", "06061", "06067",
#'     "12111", "12113", "12117",
#'     "42101", "42103", "42105"
#'   ),
#'   state_name = rep(c("ca", "fl", "pa"), each = 3),
#'   time_value = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
#'     by = "day"
#'   ), length.out = length(geo_value)),
#'   value = 1:length(geo_value) + 0.01 * rnorm(length(geo_value))
#' ) %>%
#'   tsibble::as_tsibble(index = time_value, key = c(geo_value, state_name))
#'
#' # The `other_keys` metadata (`"state_name"` in this case) is automatically
#' # inferred from the `tsibble`'s `key`:
#' ex1 <- as_epi_df(x = ex1_input, as_of = "2020-06-03")
#' attr(ex1, "metadata")[["other_keys"]]
#'
#' # Dealing with misspecified column names:
#' # Geographical and temporal information must be provided in columns named
#' # `geo_value` and `time_value`; if we start from a data frame with a
#' # different format, it must be converted to use `geo_value` and `time_value`
#' # before calling `as_epi_df`.
#' ex2_input <- tibble::tibble(
#'   state = rep(c("ca", "fl", "pa"), each = 3), # misnamed
#'   pol = rep(c("blue", "swing", "swing"), each = 3), # extra key
#'   reported_date = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
#'     by = "day"
#'   ), length.out = length(state)), # misnamed
#'   value = 1:length(state) + 0.01 * rnorm(length(state))
#' )
#' print(ex2_input)
#'
#' ex2 <- ex2_input %>%
#'   dplyr::rename(geo_value = state, time_value = reported_date) %>%
#'   as_epi_df(
#'     as_of = "2020-06-03",
#'     other_keys = "pol"
#'   )
#' attr(ex2, "metadata")
#'
#' # Adding additional keys to an `epi_df` object
#' ex3_input <- covid_incidence_county_subset %>%
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
#'   as_epi_df(other_keys = c("state", "pol"))
#'
#' attr(ex3, "metadata")
#'
#' # Decays to a tibble
#' covid_incidence_county_subset %>%
#'   dplyr::select(-geo_value)
NULL

#' @describeIn epi_df Lower-level constructor for `epi_df` object
#' @order 2
#' @param geo_type `r lifecycle::badge("deprecated")` in `as_epi_df()`, has no
#'   effect; the geo value type is inferred from the location column and set to
#'   "custom" if not recognized. In `new_epi_df()`, should be set to the same
#'   value that would be inferred.
#' @param time_type `r lifecycle::badge("deprecated")` in `as_epi_df()`, has no
#'   effect: the time value type inferred from the time column and set to
#'   "custom" if not recognized. Unpredictable behavior may result if the time
#'   type is not recognized. In `new_epi_df()`, should be set to the same value
#'   that would be inferred.
#' @param as_of Time value representing the time at which the given data were
#'   available. For example, if `as_of` is January 31, 2022, then the `epi_df`
#'   object that is created would represent the most up-to-date version of the
#'   data available as of January 31, 2022. If the `as_of` argument is missing,
#'   then the current day-time will be used.
#' @param other_keys If your tibble has additional keys, be sure to specify them
#'   as a character vector here (typical examples are "age" or sub-geographies).
#' @param ... Additional arguments passed to methods.
#' @return * Of `new_epi_df()`: an `epi_df`
#'
#' @export
new_epi_df <- function(x = tibble::tibble(geo_value = character(), time_value = as.Date(integer())),
                       geo_type, time_type, as_of,
                       other_keys = character(), ...) {
  # Define metadata fields
  metadata <- list()
  metadata$geo_type <- geo_type
  metadata$time_type <- time_type
  metadata$as_of <- as_of
  metadata$other_keys <- other_keys

  # Reorder columns (geo_value, time_value, ...)
  if (nrow(x) > 0) {
    all_names <- names(x)
    ukey_names <- c("geo_value", other_keys, "time_value")
    value_names <- all_names[!all_names %in% ukey_names]
    x <- x[c(ukey_names, value_names)]
  }

  # Apply epi_df class, attach metadata, and return
  class(x) <- c("epi_df", class(x))
  attributes(x)$metadata <- metadata

  return(x)
}

#' @describeIn epi_df The preferred way of constructing `epi_df`s
#' @order 1
#' @param x An `epi_df`, `data.frame`, [tibble::tibble], or [tsibble::tsibble]
#'   to be converted
#' @param ... used for specifying column names, as in [`dplyr::rename`]. For
#'   example, `geo_value = STATEFP, time_value = end_date`.
#' @return * Of `as_epi_df()`: an (ungrouped) `epi_df`
#'
#' @export
as_epi_df <- function(x, ...) {
  UseMethod("as_epi_df")
}

#' @rdname epi_df
#' @order 1
#' @method as_epi_df epi_df
#' @export
as_epi_df.epi_df <- function(x, ...) {
  x <- ungroup(x)
  x
}

#' @rdname epi_df
#' @order 1
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
#' @importFrom cli cli_inform
#' @method as_epi_df tbl_df
#' @export
as_epi_df.tbl_df <- function(
    x,
    geo_type = deprecated(),
    time_type = deprecated(),
    as_of,
    other_keys = character(),
    ...) {
  x <- rename(x, ...)
  x <- guess_column_name(x, "time_value", time_column_names())
  x <- guess_column_name(x, "geo_value", geo_column_names())
  if (!test_subset(c("geo_value", "time_value"), names(x))) {
    cli_abort(
      "Either columns `geo_value` and `time_value` or related columns
       (see the internal functions `guess_time_column_name()` and/or
       `guess_geo_column_name()` for a complete list)
       must be present in `x`."
    )
  }
  if (lifecycle::is_present(geo_type)) {
    cli_warn("epi_df constructor argument `geo_type` is now ignored. Consider removing.")
  }
  if (lifecycle::is_present(time_type)) {
    cli_warn("epi_df constructor argument `time_type` is now ignored. Consider removing.")
  }

  # If geo type is missing, then try to guess it
  geo_type <- guess_geo_type(x$geo_value)
  time_type <- guess_time_type(x$time_value)

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

  assert_character(other_keys)
  assert_subset(other_keys, names(x))
  # Fix up if given more than just other keys, at least until epipredict#428
  # merged:
  other_keys <- other_keys[!other_keys %in% c("geo_value", "time_value")]

  if (".time_value_counts" %in% other_keys) {
    cli_abort("as_epi_df: `other_keys` can't include \".time_value_counts\"")
  }

  assert(check_ukey_unique(x, c("geo_value", other_keys, "time_value"), c(
    ">" = "If this is line list data, convert it to counts/rates first.",
    ">" = "If this contains a demographic breakdown, check that you have
           specified appropriate `other_keys`" # . from checkmate
  )))

  new_epi_df(x, geo_type, time_type, as_of, other_keys)
}

#' @rdname epi_df
#' @order 1
#' @method as_epi_df grouped_df
#' @export
as_epi_df.grouped_df <- function(x, ...) {
  as_epi_df(ungroup(x), ...)
}

#' @rdname epi_df
#' @order 1
#' @method as_epi_df data.frame
#' @export
as_epi_df.data.frame <- function(x, as_of, other_keys = character(), ...) {
  as_epi_df(x = tibble::as_tibble(x), as_of = as_of, other_keys = other_keys, ...)
}

#' @rdname epi_df
#' @order 1
#' @method as_epi_df tbl_ts
#' @export
as_epi_df.tbl_ts <- function(x, as_of, other_keys = character(), ...) {
  tsibble_other_keys <- setdiff(tsibble::key_vars(x), "geo_value")
  if (length(tsibble_other_keys) > 0) {
    other_keys <- unique(c(other_keys, tsibble_other_keys))
  }
  as_epi_df(x = tibble::as_tibble(x), as_of = as_of, other_keys = other_keys, ...)
}

#' Test for `epi_df` format
#'
#' @param x An object.
#' @return * Of `is_epi_df`: `TRUE` if the object inherits from `epi_df`,
#'           otherwise `FALSE`.
#'
#' @rdname epi_df
#' @order 1
#' @export
is_epi_df <- function(x) {
  inherits(x, "epi_df")
}
