#' @param x A data.frame, [tibble::tibble], or [tsibble::tsibble] to be converted
#' @param geo_type Type for the geo values. If missing, then the function will
#'   attempt to infer it from the geo values present; if this fails, then it
#'   will be set to "custom".
#' @param time_type Type for the time values. If missing, then the function will
#'   attempt to infer it from the time values present; if this fails, then it
#'   will be set to "custom".
#' @param as_of Time value representing the time at which the given data were
#'   available. For example, if `as_of` is January 31, 2022, then the `epi_df`
#'   object that is created would represent the most up-to-date version of the
#'   data available as of January 31, 2022. If the `as_of` argument is missing,
#'   then the current day-time will be used.
#' @param additional_metadata List of additional metadata to attach to the
#'   `epi_df` object. The metadata will have `geo_type`, `time_type`, and
#'   `as_of` fields; named entries from the passed list will be included as
#'   well. If your tibble has additional keys, be sure to specify them as a
#'   character vector in the `other_keys` component of `additional_metadata`.
#' @return An `epi_df` object.
