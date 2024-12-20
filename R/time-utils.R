#' Use max valid period as guess for `period` of `time_values`
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param time_values Vector containing time-interval-like or time-point-like
#'   data, with at least two distinct values.
#' @param time_values_arg Optional, string; name to give `time_values` in error
#'   messages. Defaults to quoting the expression the caller fed into the
#'   `time_values` argument.
#' @param ... Should be empty, there to satisfy the S3 generic.
#' @return length-1 vector; `r lifecycle::badge("experimental")` class will
#'   either be the same class as [`base::diff()`] on such time values, an
#'   integer, or a double, such that all `time_values` can be exactly obtained
#'   by adding `k * result` for an integer k, and such that there is no smaller
#'   `result` that can achieve this.
#'
#' @keywords internal
#' @export
guess_period <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  UseMethod("guess_period")
}

#' @export
guess_period.default <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  rlang::check_dots_empty()
  sorted_distinct_time_values <- sort(unique(time_values))
  if (length(sorted_distinct_time_values) < 2L) {
    cli_abort("Not enough distinct values in {.code {time_values_arg}} to guess the period.",
      class = "epiprocess__guess_period__not_enough_times",
      time_values = time_values
    )
  }
  skips <- diff(sorted_distinct_time_values)
  # Certain diff results have special classes or attributes; use vctrs to try to
  # appropriately destructure for gcd_num, then restore to their original class
  # & attributes.
  skips_data <- vctrs::vec_data(skips)
  period_data <- gcd_num(skips_data, rrtol = 0)
  vctrs::vec_restore(period_data, skips)
}

# `full_seq()` doesn't like difftimes, so convert to the natural units of some time types:

#' @export
guess_period.Date <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  as.numeric(NextMethod(), units = "days")
}

#' @export
guess_period.POSIXt <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  as.numeric(NextMethod(), units = "secs")
}

validate_slide_window_arg <- function(arg, time_type, lower = 1, allow_inf = TRUE, arg_name = rlang::caller_arg(arg)) {
  if (time_type == "custom") {
    cli_abort(
      "Unsure how to interpret slide units with a custom time type. Consider converting your time
      column to a Date, yearmonth, or integer type.",
      class = "epiprocess__validate_slide_window_arg"
    )
  }

  msg <- ""
  inf_if_okay <- if (allow_inf) {
    "Inf"
  } else {
    character(0L)
  }

  # nolint start: indentation_linter.
  if (time_type == "day") {
    if (!(test_sensible_int(arg, lower = lower) ||
      inherits(arg, "difftime") && length(arg) == 1L && units(arg) == "days" ||
      allow_inf && identical(arg, Inf)
    )) {
      msg <- glue::glue_collapse(c("length-1 difftime with units in days", "non-negative integer", inf_if_okay), " or ")
    }
  } else if (time_type == "week") {
    if (!(inherits(arg, "difftime") && length(arg) == 1L && units(arg) == "weeks" ||
      allow_inf && identical(arg, Inf)
    )) {
      msg <- glue::glue_collapse(c("length-1 difftime with units in weeks", inf_if_okay), " or ")
    }
  } else if (time_type == "yearmonth") {
    if (!(test_sensible_int(arg, lower = lower) ||
      allow_inf && identical(arg, Inf)
    )) {
      msg <- glue::glue_collapse(c("non-negative integer", inf_if_okay), " or ")
    }
  } else if (time_type == "integer") {
    if (!(test_sensible_int(arg, lower = lower) ||
      allow_inf && identical(arg, Inf)
    )) {
      msg <- glue::glue_collapse(c("non-negative integer", inf_if_okay), " or ")
    }
  } else {
    cli_abort('`epiprocess` internal error: unrecognized time_type: "{time_type}"',
      class = "epiprocess__unrecognized_time_type"
    )
  }
  # nolint end

  if (msg != "") {
    cli_abort(
      "Slide function expected `{arg_name}` to be a {msg}.",
      class = "epiprocess__validate_slide_window_arg"
    )
  }
}


#' Convert a time delta to a integerish number of "unit" steps between time values
#'
#' @param time_delta a vector that can be added to time values of time type
#'   `time_type` to arrive at other time values of that time type, or
#'   `r lifecycle::badge("experimental")` such a vector with Inf/-Inf entries mixed
#'   in, if supported by the class of `time_delta`, even if `time_type` doesn't
#'   necessarily support Inf/-Inf entries. Basically a slide window arg but
#'   without sign and length restrictions.
#' @param time_type as in `validate_slide_window_arg`
#' @return [bare integerish][rlang::is_integerish] vector (with possible
#'   infinite values) that produces the same result as `time_delta` when
#'   multiplied by the natural [`unit_time_delta`] for
#'   that time type and added to time values of time type `time_type`. If the
#'   given time type does not support infinite values, then it should produce
#'   +Inf or -Inf for analogous entries of `time_delta`, and match the addition
#'   result match the addition result for non-infinite entries.
#'
#' @keywords internal
time_delta_to_n_steps <- function(time_delta, time_type) {
  # could be S3 if we're willing to export
  if (inherits(time_delta, "difftime")) {
    output_units <- switch(time_type,
      day = "days",
      week = "weeks",
      cli_abort("difftime objects not supported for time_type {format_chr_with_quotes(time_type)}")
    )
    units(time_delta) <- output_units # converts number to represent same duration; not just attr<-
    n_steps <- vec_data(time_delta)
    if (!is_bare_integerish(n_steps)) {
      cli_abort("`time_delta` did not appear to contain only integerish numbers
                 of steps between time values of time type {format_chr_with_quotes(time_type)}")
    }
    n_steps
  } else if (is_bare_integerish(time_delta)) { # (allows infinite values)
    switch(time_type,
      day = ,
      week = ,
      yearmonth = ,
      integer = time_delta,
      cli_abort("Invalid or unsupported time_type {format_chr_with_quotes(time_type)}")
    )
  } else {
    cli_abort("Invalid or unsupported kind of `time_delta`")
  }
}

#' Object that, added to time_values of time_type, advances by one time step/interval
#'
#' @param time_type string; `epi_df`'s or `epi_archive`'s `time_type`
#' @return an object `u` such that `time_values + u` represents advancing by one
#'   time step / moving to the subsequent time interval for any `time_values`
#'   object of time type `time_type`, and such that `time_values + k * u` for
#'   integerish vector `k` advances by `k` steps (with vectorization,
#'   recycling). At time of writing, these objects also all support
#'   multiplication by nonintegerish numeric vectors, `mean`, and `median`,
#'   which are useful for summarizing vector time_deltas, but these fractional
#'   time_deltas are not allowed in time_delta-specific operations.
#'
#' @keywords internal
unit_time_delta <- function(time_type) {
  switch(time_type,
    day = as.difftime(1, units = "days"),
    week = as.difftime(1, units = "weeks"),
    yearmonth = 1,
    integer = 1L,
    cli_abort("Unsupported time_type: {time_type}")
  )
}

# Using these unit abbreviations happens to make our automatic slide output
# naming look like taking ISO-8601 duration designations, removing the P, and
# lowercasing any characters. Fortnightly or sub-daily time types would need an
# adjustment to remain consistent.
time_type_unit_abbrs <- c(
  day = "d",
  week = "w",
  yearmonth = "m"
)

#' Get an abbreviation for the "units" of `unit_time_delta(time_type)`
#'
#' For use in formatting or automatically naming things based on
#' `time_delta_to_n_steps(time_delta)` for a `time_delta` between times of time
#' type `time_type`.
#'
#' @param time_type str
#' @return str
#'
#' @keywords internal
time_type_unit_abbr <- function(time_type) {
  maybe_unit_abbr <- time_type_unit_abbrs[time_type]
  if (is.na(maybe_unit_abbr)) {
    cli_abort("Cannot determine the units of time type {format_chr_with_quotes(time_type)}")
  }
  maybe_unit_abbr
}

#' Convert `time_delta` to an approximate difftime
#'
#' `r lifecycle::badge("experimental")`
#'
#' To assist in comparing `time_delta`s to default `difftime` thresholds when we
#' want to reduce friction.
#'
#' It may be better to try to do something like make `time_delta` validation
#' more accommodating (e.g., of difftimes with units of "days" when working on
#' weekly scale), and remain rigid on yearmonths. Applying deltas and comparing
#' time_values might also be an approach but seems more fraught as the least
#' common denominator would be start/mid/end datetimes of time intervals, but
#' those are also ambiguous (starting&representation wdays of weeks are unknown,
#' timezone of dates are unknown).
#'
#' Another alternative approach, below, converts difftimes to time_deltas
#' instead. It requires knowledge of which way to round in order to get
#' time_deltas representing an integer number of time steps, but avoids some
#' potential inconsistencies of the time-delta-to-difftime approach when we
#' think about applying it to, e.g., months / spans of months with varying
#' numbers of days, and also makes it easier to avoid "magical defaults".
#'
#' @keywords internal
time_delta_to_approx_difftime <- function(time_delta, time_type) {
  switch(time_type,
    day = ,
    week = {
      if (inherits(time_delta, "difftime")) {
        time_delta
      } else {
        time_delta_to_n_steps(time_delta, time_type) * unit_time_delta(time_type)
      }
    },
    yearmonth = time_delta * as.difftime(30, units = "days"),
    integer = ,
    cli_abort("Unsupported time_type for this operation: {time_type}")
  )
}

#' Closest time_delta that's approximately greater than given difftime
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param difftime a difftime object
#' @param time_type as in `validate_slide_window_arg`
#' @return An object representing an integerish number (or vector of numbers) of
#'   time steps between consecutive time_values of type `time_type`.
#'
#' @keywords internal
difftime_approx_ceiling_time_delta <- function(difftime, time_type) {
  assert_class(difftime, "difftime")
  switch(time_type,
    day = ,
    week = {
      units(difftime) <- paste0(time_type, "s")
      ceiling(difftime)
    },
    yearmonth = {
      units(difftime) <- "days"
      ceiling(as.numeric(difftime) / 30)
    },
    integer = ,
    cli_abort("Unsupported time_type for this operation: {time_type}")
  )
}
