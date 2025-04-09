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

#' Validate `.before` or `.window_size` argument
#' @keywords internal
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

time_delta_formats <- c("friendly", "fast")

#' Object that, added to time_values of time_type, advances by one time step/interval
#'
#' @param time_type string; `epi_df`'s or `epi_archive`'s `time_type`
#' @param format "friendly" or "fast"; for some time_types, there are multiple
#'   ways to represent time_deltas. "friendly" tries to output a format that
#'   will be more informative when printed, and produce errors in more cases
#'   when used in unexpected ways. "fast" tries to output a time_delta that will
#'   be faster in downstream operations.
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
unit_time_delta <- function(time_type, format = c("friendly", "fast")) {
  format <- rlang::arg_match0(format, time_delta_formats)
  switch(format,
    friendly = switch(time_type,
      day = as.difftime(1, units = "days"),
      week = as.difftime(1, units = "weeks"),
      yearmonth = 1,
      integer = 1L,
      cli_abort("Unsupported time_type: {time_type}")
    ),
    fast = switch(time_type,
      day = 1,
      week = 7,
      yearmonth = 1,
      integer = 1L,
      cli_abort("Unsupported time_type: {time_type}")
    )
  )
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

#' Convert from integerish/infinite/mix to time_delta
#'
#' @param n_steps integerish vector that can mix in infinite values
#' @param time_type as in [`validate_slide_window_arg`]
#' @param format optional; `"friendly"` to output a more descriptive/friendly
#'   class like `"difftime"` when possible; `"fast"` to output a class that's
#'   generally faster to work with when possible, like a vanilla `"numeric"`.
#'   Default is `"friendly"`.
#'
#' @keywords internal
n_steps_to_time_delta <- function(n_steps, time_type, format = c("friendly", "fast")) {
  if (!is_bare_integerish(n_steps)) {
    cli_abort("`n_steps` did not appear to be integerish (or infinite, or a mix)")
  }
  n_steps * unit_time_delta(time_type, format)
}

#' Standardize time_deltas to a multiple of [`unit_time_delta()`]
#'
#' @keywords internal
time_delta_standardize <- function(time_delta, time_type, format = c("friendly", "fast")) {
  time_delta_to_n_steps(time_delta, time_type) * unit_time_delta(time_type, format)
}

#' Helper data for [`time_type_unit_abbr`]
#'
#' @keywords internal
time_type_unit_abbrs <- c(
  day = "d",
  week = "w",
  yearmonth = "m"
)
# ^ Using these unit abbreviations happens to make our automatic slide output
# naming look like taking ISO-8601 duration designations, removing the P, and
# lowercasing any characters. Fortnightly or sub-daily time types would need an
# adjustment to remain consistent.

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

#' Helper data for [`format_time_delta`]
#'
#' Should not be altered on the basis of untrusted user input, as it is used as
#' a cli format string and may run code.
#'
#' @keywords internal
time_type_unit_pluralizer <- c(
  day = "day{?s}",
  week = "week{?s}",
  yearmonth = "month{?s}",
  integer = "time step{?s}"
)

#' Format a length-1 time delta to a character to assist messaging
#'
#' This is meant to address the following:
#' - `glue::glue("{as.difftime(1, units = 'days')}")` is "1"
#' - `glue::glue("{format(as.difftime(1, units = 'days'))}")` is "1 days"
#' - time deltas for yearmonths and integers don't have units attached at all
#'
#' @keywords internal
format_time_delta <- function(x, time_type) {
  n_steps <- time_delta_to_n_steps(x, time_type) # nolint: object_usage_linter
  # time_type_unit_pluralizer[[time_type]] is a format string controlled by us
  # and/or downstream devs, so we can paste it onto our format string safely:
  pluralize(paste0("{n_steps} ", time_type_unit_pluralizer[[time_type]]))
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
    week = time_delta_standardize(time_delta, time_type, "friendly"),
    yearmonth = time_delta * as.difftime(30, units = "days"),
    integer = ,
    cli_abort("Unsupported time_type for this operation: {time_type}")
  )
}

#' Closest time_delta that's approximately greater than or equal to given difftime
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param difftime a difftime object
#' @param time_type as in [`validate_slide_window_arg`]
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

#' Difference between two finite `time_value` vectors in terms of number of time "steps"
#'
#' @param x a time_value (vector) of time type `time_type`
#' @param y a time_value (vector) of time type `time_type`
#' @param time_type as in [`validate_slide_window_arg()`]
#' @return integerish vector such that `x + n_steps_to_time_delta_fast(result)`
#'   should equal `y`.
#'
#' @keywords internal
time_minus_time_in_n_steps <- function(x, y, time_type) {
  time_delta_to_n_steps(x - y, time_type)
}

#' Advance/retreat time_value(s) by bare-integerish number(s) of time "steps"
#'
#' Here, a "step" is based on the `time_type`, not just the class of `x`.
#'
#' @param x a time_value (vector) of time type `time_type`
#' @param y bare integerish (vector)
#' @param time_type as in [`validate_slide_window_arg()`]
#' @return a time_value (vector) of time type `time_type`
#'
#' @seealso [`time_plus_slide_window_arg`] if you're working with a `y` that is
#'   a slide window arg, which is scalar but otherwise more general (class-wise,
#'   Inf-wise) than an integerish vector.
#' @keywords internal
time_plus_n_steps <- function(x, y, time_type) {
  x + y * unit_time_delta(time_type, "fast")
}

#' @rdname time_plus_n_steps
time_minus_n_steps <- function(x, y, time_type) {
  x - y * unit_time_delta(time_type, "fast")
}

#' Advance/retreat time_value(s) by specified amount (slide window arg)
#'
#' @param x a time_value (vector) of time type `time_type`
#' @param y a (scalar) slide window arg; should pass [`validate_slide_window_arg()`]
#' @param time_type as in [`validate_slide_window_arg()`]
#' @param max_time_value when `y == Inf`, what should be the result of adding `y`?
#' @param min_time_value when `y == Inf`, what should be the result of subtracting `y`?
#' @return a time_value (vector) of time type `time_type`
#'
#' @keywords internal
#' @seealso [`time_plus_n_steps`], if you're working with an integerish vector
#'   number of time steps `y` (output from other `*n_steps` functions) instead.
time_plus_slide_window_arg <- function(x, y, time_type, max_time_value) {
  if (y == Inf) {
    rep(max_time_value, vec_size(x))
  } else {
    time_plus_n_steps(x, y, time_type)
  }
}

#' @rdname time_plus_slide_window_arg
time_minus_slide_window_arg <- function(x, y, time_type, min_time_value) {
  if (y == Inf) {
    rep(min_time_value, vec_size(x))
  } else {
    time_minus_n_steps(x, y, time_type)
  }
}
