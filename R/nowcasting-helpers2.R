
# extract2_horizon <- function(x, ekvs, var, horizon, ...) UseMethod("extract2_horizon")

# #' @export
# extract2_horizon.epi_archive <- function(x, ekvs, var, horizon, ...) {
#   assert_true(obj_is_tblish(ekvs))
#   ekvs <- tblish_cast_cols(ekvs, x$DT[, key(x$DT)]) # FIXME minus the time_value
#   assert_string(var) # XXX vs. tidyselect?
#   assert_subset(var, names(x$DT))
#   if (x$time_type == "week") {
#     if (!inherits(horizon, "difftime")) {
#       cli_abort("For weekly time_type, `horizon` must be a difftime.")
#     }
#     if (nrow(x$DT) == 0L) {
#       assert_subset(units_horizon, c("days", "weeks"))
#     } else {
#       if (length(unique(as.double(x$DT$version) %% 7)) != 1L) {
#         # FIXME this is probably going to eventually be pretty common,
#         # along with datetime version... TODO go back and consider
#         # adding the Hub version->reference_date concept.  Would have
#         # to be supplied since we don't yet store enough metadata.
#         # But then this would maybe handle this case and simplify the
#         # other complicated reasoning below.
#         cli_abort(c("`time_value`s are weekly, but `version`s are not; this is currently not supported",
#                     ">" = "please thin out the versions to a weekly cadence, or use daily time values if possible"),
#                   class = "epiprocess__extract2_horizon__weekly_time_nonweekly_version")
#       }
#       alignment_horizon_days_dbl <- as.double(x$DT$time_value[[1L]] - x$DT$version[[1L]]) %% 7
#       if (units(horizon) == "days" && as.double(horizon) %% 7 != alignment_horizon_days_dbl) {
#         cli_abort(c("`horizon` is requesting impossible `time_value`s",
#                     "i" = 'horizon: {horizon} days',
#                     "i" = '`time_value`s of `x` fall on {format(x$DT$time_value[[1L]], "%a")}',
#                     "i" = '`version`s of `x` fall on {format(x$DT$version[[1L]], "%a")}',
#                     "x" = "Adding {horizon} days to any `version` will not
#                            correspond to any possible `time_value`",
#                     "i" = "`epi_archive`s currently don't store enough information
#                            about the weekly time values to know how to recover",
#                     ">" = 'Update `horizon` so that adding it to a
#                            {format(x$DT$version[[1L]], "%a")} will yield a
#                            {format(x$DT$time_value[[1L]], "%a")}'
#                     ),
#                   class = "epiprocess__extract2_horizon__horizon_days_misaligned")
#       } else if (units(horizon) == "weeks" && alignment_horizon_days_dbl != 0) {
#         cli_abort(c("`horizon` is in terms of weeks, but `time_value`s and `version`s
#                      fall on differing weekdays; not sure how to align them",
#                     ">" = "provide `horizon` in terms of days instead"),
#                   class = "epiprocess__extract2_horizon__horizon_weeks_misaligned")
#       } else {
#         valid_units <- if (alignment_horizon_days_dbl == 0L) c("days", "weeks") else "days"
#         if (! units(horizon) %in% valid_units || ! rlang::is_integerish(as.double(horizon))) {
#           cli_abort('`horizon` must be in terms of integer number of
#                      {cli_vec(valid_units, style = list("vec-last" = " or "))}',
#                   class = "epiprocess__extract2_horizon__horizon_bad_units_or_not_integer")
#         }
#       }
#     }
#   } else {
#     validate_slide_window_arg(horizon, x$time_type, lower = -Inf, allow_inf = FALSE)
#   }
#   ektvs <- ekvs
#   tblish_col(ektvs, "time_value") <- tblish_col(ektvs, "version") + horizon
#   ektvs <- tblish_select(ektvs, key(x$DT)) # in case data.table relies on var order not names
#   result <- x$DT[ektvs, var, on = key(x$DT), with = FALSE, roll = TRUE]
#   result <- result[[var]]
#   # We don't actually have observations for versions past versions_end.
#   #
#   # XXX perhaps we should instead base this on the max version with a
#   # diff overall or by epikey.
#   vec_slice(result, ektvs$version > x$versions_end) <- NA
#   result
# }

# XXX mostly ditching tblish for now

#' For each `ekt_var_subset` key, the `version`s it had with a "real" diff for any `val_var_subset`
#'
#' Here, "real" means either (a) changing from unobserved to a non-NA
#' initial estimate, or (b) changing from one estimate to a different
#' estimate.  It does not count moving from unobserved to an NA
#' initial estimate, as these are often not actually present in the
#' underlying data and are simply present due to storing multiple val
#' vars in the same archive.
#'
#' The choice of `ekt_var_subset` is a tradeoff between (a) recognizing the
#' maturity of estimates that remained the same between versions for
#' reasons other than an outage, plus enabling models to treat more
#' estimates as equally mature, vs. (b) recognizing the immaturity of
#' estimates that remained the same between versions due to more
#' complex types of outages.
#'
#' * If some epikeys (geodemographic groups) can have pauses in
#'   reporting not shared by others, then you should probably include
#'   `c("geo_value", x$other_keys)` in `ekt_var_subset`.
#'
#' * If some `time_value`s can have pauses in reporting not shared by
#'   others, then you should probably include `"time_value"` in
#'   `ekt_var_subset`.  Depending on how downstream models treat NAs, you
#'   may be able to get away with not adding it if these pauses in
#'   reporting simply mean that no new `time_value`s are being added
#'   to the data set, but all previously-observed `time_value`s are
#'   receiving revised estimates.
#'
#' @param x an `epi_archive`
#' @param ekt_var_subset optional; var_subset of
#'   `key_colnames(x, exclude = "version")`; defaults to
#'   `key_colnames(x, exclude = c("time_value" ,"version"))`
#' @param val_var_subset technically optional; var_subset of
#'   `val_colnames(x)`, but likely just a single var; defaults to
#'   entire set
#' @return tibble with names matching `ekt_var_subset`; note that if you
#'   want downstream code to adapt to the choice of `ekt_var_subset`, you
#'   will also need to pass `ekt_var_subset` to later processing and may
#'   need to specify `relationship = "many-to-{one,many}"` if joining
#'   to other data.
#'
#' @keywords internal
epix_confkeys <- function(x,
                          val_var_subset,
                          ekt_var_subset = key_colnames(x, exclude = c("time_value", "version"))) {
  # XXX rename to epix_distinct_confkeys ?
  assert_class(x, "epi_archive")
  assert_subset(val_var_subset, val_colnames(x))
  assert_subset(ekt_var_subset, key_colnames(x, exclude = "version"))
  ektv_var_subset <- c(ekt_var_subset, "version")
  confkeys <- x$DT[, c(key_colnames(x), val_var_subset), with = FALSE] %>%
    setDF() %>%
    as_tibble() %>%
    filter(!update_is_locf(., key_colnames(archive), 0, TRUE)) %>%
    distinct(pick(all_of(ektv_var_subset)))
  confkeys
}
# XXX long format might be more convenient but less flexible?

# epix_confkeys_min_lag <- function(x,
#                                   val_var_subset,
#                                   ekt_var_subset = key_colnames(x, exclude = c("time_value", "version"))) {
#   assert_class(x, "epi_archive")
#   assert_subset(val_var_subset, val_colnames(x))
#   assert_subset(ekt_var_subset, key_colnames(x, exclude = "version"))
#   ektv_var_subset <- c(ekt_var_subset, "version")
#   x$DT[, c(key_colnames(x), val_var_subset), with = FALSE] %>%
#     setDF() %>%
#     as_tibble() %>%
#     filter(!update_is_locf(., key_colnames(archive), 0, TRUE)) %>%
#     transmute(ekt_subset = pick(all_of(ekt_var_subset)), version, time_value) %>%
#     summarize(.by = all_of(c("ekt_subset", "version")),
#               min_lag = min(version_get_containing_time_value(.data$version, x) - time_value))
# }

corresponding_confkey_versions <- function(edf,
                                           archive,
                                           val_var_subset,
                                           ekt_var_subset = key_colnames(archive, exclude = c("time_value", "version"))) {
  # XXX this is a tad slow.  maybe need to have a confkey table structure rather than archive
  assert_class(edf, "epi_df")
  all_confkeys <- epix_confkeys(archive, val_var_subset, ekt_var_subset)
  ektv_var_subset <- c(ekt_var_subset, "version")
  setDT(all_confkeys, key = ektv_var_subset)
  request_keys <- edf %>%
    as_tibble() %>%
    select(all_of(ekt_var_subset)) %>%
    mutate(version = attr(edf, "metadata")$as_of)
  result <- all_confkeys[as.list(request_keys), x.version, on = ektv_var_subset, roll = TRUE]
  # XXX some possibility of NA... let through or raise error?
  result
}

time_type <- function(x) UseMethod("time_type")

#' @export
time_type.epi_df <- function(x) attr(x, "metadata")$time_type

#' @export
time_type.epi_archive <- function(x) x$time_type

# Not making a `time_type<-` as that implies validation.

set_time_type0 <- function(x, value) UseMethod("set_time_type0")

#' @export
set_time_type0.epi_df <- function(x, value) {
  attr(x, "metadata")$time_type <- value
  x
}

#' @export
set_time_type0.epi_archive <- function(x, value) {
  x$time_type <- value
  x
}

# TODO consider standardizing this to any length integer vector
# instead?  that's what we actually get out of POSIXlt.  Then add
# length 1 checks where needed?
as_lt_wday <- function(wday_name = NULL, lt_wday = NULL, call = caller_env()) {
  provided <- names(which(!vapply(list(wday_name = wday_name, lt_wday = lt_wday), is.null, logical(1L))))
  if (length(provided) == 0L) {
      cli_abort("Either `wday_name` or `iso_wday` must be provided", call = call)
  } else if (length(provided) > 1L) {
      cli_abort("`wday_name` and `iso_wday` are mutually exclusive", call = call)
  }
  switch(provided,
    wday_name = {
      assert_string(wday_name)
      switch(wday_name,
        "Sun" = , "Sunday" = 0, # (dbl backing, since Dates are already dbl backed)
        "Mon" = , "Monday" = 1,
        "Tue" = , "Tuesday" = 2,
        "Wed" = , "Wednesday" = 3,
        "Thu" = , "Thursday" = 4,
        "Fri" = , "Friday" = 5,
        "Sat" = , "Saturday" = 6,
        cli_abort("`wday_name` must be a English wday name or 3-letter abbreviation,
                   not {format_chr_deparse(wday_name)}",
                  call = call)
      )
    },
    lt_wday = {
      # (Accepts either dbl or int backing.)
      assert_int(lt_wday, lower = 0, upper = 6)
      lt_wday
    }
  )
}

lt_wday_abbr <- function(x) {
  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[[x + 1L]]
}

default_week_ending_lt_wday <- function() {
  # Base on {lubridate}'s default.
  lubridate_starting_iso_wday <- getOption("lubridate.week.start", 7)
  starting_lt_wday <- lubridate_starting_iso_wday %% 7
  ending_lt_wday <- (starting_lt_wday + 6) %% 7
  cli_inform(c(
    'Guessing weeks are {lt_wday_abbr(starting_lt_wday)} to {lt_wday_abbr(ending_lt_wday)}
         based on lubridate default ({.code getOption("lubridate.week.start", 7)}).',
    ">" = "Override by calling {.code set_time_week_end} with the desired *ending* wday."
  ), class = "epiprocess__set_time_week_end__guessing_default")
  ending_lt_wday
}

set_time_week_end <- function(x, wday_name = NULL, ..., lt_wday = NULL) {
  check_dots_empty()
  time_type <- time_type(x)
  if (time_type != "week") {
    cli_abort('time_type of `x` must be "week", not {format_chr_deparse(time_type)}')
  }
  if (is.null(wday_name) && is.null(lt_wday)) {
    current_ending_lt_wday <- attr(time_type, "ending_lt_wday")
    if (!is.null(current_ending_lt_wday)) {
      return(x)
    }
    ending_lt_wday <- default_week_ending_lt_wday()
  } else {
    ending_lt_wday <- as_lt_wday(wday_name, lt_wday)
  }
  attr(time_type, "ending_lt_wday") <- ending_lt_wday
  x <- set_time_type0(x, time_type)
  x
}

time_value_obj <- function(x) UseMethod("time_value_obj")

#' @export
time_value_obj.epi_df <- function(x) {
  x$time_value
}

#' @export
time_value_obj.epi_archive <- function(x) {
  x$DT$time_value
}

session_tz <- function() {
  tz <- Sys.getenv("TZ")
  if (tz == "") {
    tz <- Sys.timezone()
  }
  tz
}

version_obj <- function(x) UseMethod("version_obj")

#' @export
version_obj.epi_df <- function(x) {
  attr(x, "metadata")$as_of
}

#' @export
version_obj.epi_archive <- function(x) {
  x$DT$version
}

force_time_tz <- function(x, tz = NULL) {
  if (is.null(tz)) {
    old_time_type <- time_type(x)
    if (!is.null(attr(old_time_type, "tzone"))) {
      return(x)
    }
    this_session_tz <- session_tz()
    x_version_obj <- version_obj(x)
    if (inherits(x_version_obj, "POSIXct")) {
      x_version_tz <- attr(x_version_obj, "tzone")
      if (!is.null(x_version_tz) && x_version_tz != tz) {
        cli_abort(c("
          This R session's default time zone is {format_chr_deparse(this_session_tz)},
          but {.var x} has {.var version}/{.var as_of} set to display with {format_chr_deparse(x_version_tz)};
          not sure which time zone to guess that {.var time_value}s are in terms of.
        ",
        ">" = "Manually call {.code force_time_tz(x, intended_time_value_time_zone)}, or",
        ">" = 'Set the {.code "tzone"} {.code attr} of {.var version}/{.var as_of} to {.code NULL},
               to let this function just guess the session time zone.'
        ))
      }
    }
    cli_inform("Guessing that times values should be interpreted
                with a time zone of {.code {format_chr_deparse(this_session_tz)}},
                based on this R session's timezone.")
    tz <- this_session_tz
  } else {
    assert_string(tz)
  }
  switch(
    time_type(x),
    day =, week =, yearmonth = {
        if (! tz %in% OlsonNames()) {
          cli_abort(c("{.var tz} must be a time zone listed in {.code OlsonNames()}, not {.code {format_chr_deparse(tz)}}"))
        }
        new_time_type <- old_time_type
        attr(new_time_type, "tzone") <- tz
        x <- set_time_type0(x, new_time_type)
    },
    cli_abort("Setting {.var time_value} time zone with a {.var time_type} of {format_chr_deparse(time_type)} is unsupported.")
  )
  x
}

time_get_zero_lag_version <- function(time_value, time_type, version_ptype) {
  if (rlang::is_bare_numeric(version_ptype)) {
    if (time_type == "integer") {
      time_value
    } else {
      cli_abort("time_type {format_chr_deparse(time_type)} with bare numeric versions is unsupported")
    }
  } else if (inherits(version_ptype, "yearmonth")) {
    if (time_type == "yearmonth") {
      time_value
    } else {
      cli_abort("time_type {format_chr_deparse(time_type)} with yearmonth versions is unsupported")
    }
  } else {
    time_ending_date <- switch(
      time_type,
      day = time_value,
      yearmonth = as.Date(time_value + 1L) - 1L,
      week = {
        ending_lt_wday <- attr(time_type, "ending_lt_wday")
        if (is.null(ending_lt_wday)) {
          cli_abort("`set_time_week_end` must be called first")
        }
        time_value + (ending_lt_wday - as.numeric(time_value)) %% 7
      },
    )
    if (inherits(version_ptype, "Date")) {
      time_ending_date
    } else if (inherits(version_ptype, "POSIXct")) {
        if (is.null(attr(time_type, "tzone"))) {
          cli_abort(c("We need information on the time_value's time zone
                         in order to calculate lags with datetime versions",
                      ">" = "Call `force_time_tz` to specify the time_value time zone beforehand."))
        }
        next_time_start_date <- time_ending_date + 1
        next_time_start_ct <- strptime(as.character(next_time_start_date), "%Y-%m-%d",
                                       tz = attr(time_type, "tzone"))
        # ^ Gets midnight in the given tz, and sets to print in terms
        # of given tz.  Like vec_cast, but faster.  Don't want to use
        # as.POSIXct(tz=) or as.POSIXlt(tz=), which get "UTC" or
        # non-DST-midnight-for-given-tz, respectively, then set to
        # print in terms of given tz.
        next_time_start_ct
    } else {
      cli_abort("time_type {format_chr_deparse(time_type)} with version class {format_chr_deparse(class(version_ptype))}")
    }
  }
}

version_get_containing_time_value <- function(version, dat) UseMethod("version_get_containing_time_value")

#' @export
version_get_containing_time_value.numeric <- function(version, dat) {
  time_type <- time_type(dat)
  if (time_type == "integer") {
    version
  } else {
    cli_abort("Unsupported time_type x version class:
               {format_chr_deparse(time_type)} x {format_chr_deparse(class(version))}")
  }
}

#' @export
version_get_containing_time_value.yearmonth <- function(version, dat) {
  time_type <- time_type(dat)
  if (time_type == "yearmonth") {
    version
  } else {
    cli_abort("Unsupported time_type x version class:
               {format_chr_deparse(time_type)} x {format_chr_deparse(class(version))}")
  }
}

#' @export
version_get_containing_time_value.Date <- function(version, dat) {
  # XXX not storing repr_lt_wday in time_type (or just using an
  # appropriate time_value class) makes us need to pass dat / its time
  # value obj.
  time_type <- time_type(dat)
  if (time_type == "day") {
    version
  } else if (time_type == "week") {
    ending_lt_wday <- attr(time_type, "ending_lt_wday")
    if (is.null(ending_lt_wday)) {
      cli_abort("`set_time_week_end` must be called first")
    }
    # TODO ^ refactor to helper?
    time_values <- time_value_obj(dat)
    if (length(time_values) == 0L) {
      cli_abort("Need nonzero number of time_values so we can tell which wday is used to represent weeks.")
    }
    repr_lt_wday <- as.POSIXlt(time_values[[1L]])$wday
    version + (ending_lt_wday - as.POSIXlt(version)$wday) - (ending_lt_wday - repr_lt_wday)
  } else {
    # TODO yearmonth support
    cli_abort("Unsupported time_type x version class:
               {format_chr_deparse(time_type)} x {format_chr_deparse(class(version))}")
  }
}

#' @export
version_get_containing_time_value.POSIXct <- function(version, dat) {
  time_type <- time_type(dat)
  if (time_type %in% c("day", "week", "yearmonth")) {
    version_date <- as.Date(format(version, "%Y-%m-%d"))
    # ^ can't just use as.Date; POSIXct assumes Dates are "UTC" Dates.
    version_get_containing_time_value(version_date, dat)
  } else {
    cli_abort("Unsupported time_type x version class:
               {format_chr_deparse(time_type)} x {format_chr_deparse(class(version))}")
  }
}

#' Validate that `version_lag` is an unambiguous, length-1, `version-time_value` possibility
#'
#' @keywords internal
validate_nice_version_lag <- function(version_lag, x, version_lag_arg = rlang::caller_arg(version_lag), call = rlang::caller_env()) {
  assert_vector(version_lag, len = 1L, .var.name = version_lag_arg)
  x_time_type <- time_type(x)
  x_version_obj <- version_obj(x)
  # x time_value class: bare integerish, Date, yearmonth, custom
  # x_time_type: day, week, yearmonth, integer, custom
  # x_version_obj: bare integerish, Date, yearmonth, POSIXct
  # version_lag: bare integerish, difftime
  if (inherits(version_lag, "difftime")) {
    if (inherits(x_version_obj, c("Date", "yearmonth", "POSIXct"))) { # (any)
      # TODO check if weekly versions and existing real lags are very uniform & version_lag not among?
      invisible(version_lag)
    } else {
      cli_abort("difftime version lags can only be used with Date, yearmonth, or POSIXct versions,
                 not versions of class {format_chr_deparse(class(x_version_obj))}")
    }
  } else if (rlang::is_bare_integerish(version_lag)) {
    if (inherits(x_version_obj, "Date") &&
          (x_time_type == "week" || any(diff(as.numeric(x_version_obj)) == 7))
        ) {
      cli_abort("Since versions appeared weekly in some/all of the data,
                 {.var {version_lag_arg}} must be a difftime, not bare integerish,
                 in order to clarify whether it is in terms of days or weeks.")
    }
    if (inherits(x_version_obj, c("Date", "yearmonth", "numeric", "integer"))) {
      invisible(version_lag)
    } else {
      cli_abort("Bare integerish {.var {version_lag_arg}} cannot be used with versions of class
                 {format_chr_deparse(class(x_version_obj))}.")
    }
  } else {
    cli_abort("{.var {version_lag_arg}} must be difftime or bare integerish,
               not an object of class {format_chr_deparse(class(version_lag))}")
  }
}

vtol_preprocess <- function(vtol, x, x_var_conf_ekvs) {
  ek_vars <- c("geo_value", x$other_keys)
  # TODO refactor a lot of this into basic helpers.  Need some idea of
  # non-integer numbers of steps in time interval cases.
  if (is.null(vtol)) {
    # First, we want to prevent vnominal1 + vdeparture1 + vtol from
    # regularly crossing vnominal2 + vdeparture2 (i.e., vactual1 +
    # vtol < vactual2).  With vtol approach we're not going to try to
    # infer the vnominals.  Let's just assume we've seen enough draws
    # that we can just select vtol < vactual2 - vactual1 for all/most
    # seen vactual1, vactual2.
    low_vstride <-
      x_var_conf_ekvs %>%
      reframe(.by = all_of(ek_vars),
              # re-using reserved name `version` for version gaps:
              version = diff(sort(unique(version)))) %>%
      .$version %>%
      # Don't be too strict; if schedule isn't perfectly regular
      # (e.g., there are delays around holidays) plus there are
      # special data set overhauls, we don't want presence of the
      # latter to make us balk at the former:
      quantile(probs = 0.10) %>%
      unname()
    # We also want to prevent version - lag from being ambiguous in
    # what time it refers to, so something like preventing t1 + lag +
    # vtol from crossing t2 + lag - vtol.  So choose vtol < (t2 -
    # t1)/2 = unit_time_delta(time_type)/2 <==> vtol < ceiling of RHS.
    approx_ceiling_half_tstride_in_vspace <-
      # TODO refactor to helper?
      switch(x$time_type,
             day =, week = , yearmonth = {
               approx_half_tstride_difftime <-
                 unit_time_delta(x$time_type) %>%
                 time_delta_to_approx_difftime(x$time_type) %>%
                 `/`(2)
               if (inherits(x$DT$version, "POSIXct")) {
                 approx_half_tstride_difftime
               } else if (inherits(x$DT$version, "Date")) {
                 approx_half_tstride_difftime %>%
                   difftime_approx_ceiling_time_delta("day") %>%
                   time_delta_standardize("day")
               } else if (inherits(x$DT$version, "yearmonth")) {
                 approx_half_tstride_difftime %>%
                   difftime_approx_ceiling_time_delta("yearmonth") %>%
                   time_delta_standardize("yearmonth")
               } else {
                 cli_abort("Unsupported time_type x version class:
                            {format_chr_deparse(x$time_type)} x {format_chr_deparse(class(x$DT$version))}")
               }
             },
             integer = {
               1L
             },
             cli_abort("Unsupported time_type {format_chr_deparse(x$time_type)}"))
    vtol <- exclusive(min(c(low_vstride, approx_ceiling_half_tstride_in_vspace)))
  } else {
    vtol <- as_inclusive_if_not_bound(vtol)
    if (is.difftime(vtol$threshold)) {
      if (inherits(x$DT$version, "POSIXct")) {
        # nothing to do
      } else if (inherits(x$DT$version, "Date")) {
        vtol$threshold <- difftime_approx_floor_time_delta(vtol$threshold, "day") %>%
          time_delta_standardize("day")
      } else if (inherits(x$DT$version, "yearmonth")) {
        vtol$threshold <- difftime_approx_floor_time_delta(vtol$threshold, "yearmonth") %>%
          time_delta_standardize("yearmonth")
      } else {
        cli_abort("difftime vtol threshold not supported with version class
                   {format_chr_deparse(class(x$DT$version))}")
      }
    } else {
      # vtol$threshold is definitely in terms of intervals
      if (inherits(x$DT$version, "Date")) {
        vtol$threshold <- time_delta_standardize(vtol$threshold, "day")
      } else if (inherits(x$DT$version, "yearmonth")) {
        vtol$threshold <- time_delta_standardize(vtol$threshold, "yearmonth")
      } else {
        cli_abort("unsupported vtol threshold class x version class
                   {format_chr_deparse(class(vtol$threshold))} x {format_chr_deparse(class(x$DT$version))}")
      }
    }
  }
  vtol
}

# XXX might be unclear in usage which time voffset is relative to.  also, with extract2, perhaps even less need to combine in toffset.
#
# XXX function name also doesn't suggest that it may be using nearby...
extract2_tvoffset <- function(x, ekts, var, toffset, voffset, vtol = NULL, ...) UseMethod("extract2_tvoffset")

#' @export
extract2_tvoffset.epi_archive <- function(x, ekts, var, toffset, voffset, vtol = NULL, ...) {
  # TODO allow this to be by wday/etc.?  Or make a group_modify.epi_archive?
  assert_class(ekts, "tbl_df")
  ekts <- tblish_cast_cols(ekts, x$DT[0L, key_colnames(x, exclude = "version"), with = FALSE])
  assert_string(var)
  assert_subset(var, names(x$DT))
  # assert_vector(toffset, len = 1L)
  validate_slide_window_arg(toffset, x$time_type, lower = -Inf)
  toffset <- time_delta_standardize(toffset, x$time_type, "fast")
  # version_type <- guess_time_type(x$DT$version)
  # assert_vector(voffset, len = 1L)
  # voffset <- time_delta_standardize(voffset, version_type, "fast")
  validate_nice_version_lag(voffset, x)
  # We want default vtol to be reasonably large to in order to adapt
  # to normal variance in pipeline schedules as well as transient
  # pipeline issues and holiday-shifted schedules, which seem pretty
  # common.
  ek_vars <- c("geo_value", x$other_keys)
  x_var_conf_ekvs <- epix_confkeys(x, var, ek_vars)
  vtol <- vtol_preprocess(vtol, x, x_var_conf_ekvs)
  check_dots_empty()

  lookup_ektvs <- ekts %>%
    mutate(time_value = time_value + toffset,
           version = time_get_zero_lag_version(time_value, x$time_type, x$DT$version) + voffset)

  # Find the closest "real" version for `var` for each lookup
  ekv_vars <- c(ek_vars, "version")
  setDT(x_var_conf_ekvs, key = ekv_vars)
  real_versions_info <- x_var_conf_ekvs[
    as.list(lookup_ektvs)[ekv_vars], on = ekv_vars, roll = "nearest",
    list(real_version = x.version, vdiff = x.version - i.version)
  ]
  # ^ `as.list` is needed to make `x.version` and `i.version` work

  lookup_ektvs$version <- real_versions_info$real_version
  result <- x$DT[
    as.list(lookup_ektvs), on = key(x$DT), roll = TRUE,
    var,
    with = FALSE
  ][[var]] # `with = FALSE` -> have to manually extract2
  if (vtol$inclusive) {
    result[real_versions_info[, abs(vdiff) > vtol$threshold]] <- NA
  } else {
    result[real_versions_info[, abs(vdiff) >= vtol$threshold]] <- NA
  }

  result
}

# extract2_version_lag <- function(x, ekts, var, version_lag, vtol = NULL, ...) UseMethod("extract2_version_lag")

# #' @export
# extract2_version_lag.epi_archive <- function(x, ekts, var, version_lag, vtol = NULL, ...) {
#   # TODO allow this to be by wday/etc.?  Or make a group_modify.epi_archive?
#   assert_class(ekts, "tbl_df")
#   ekts <- tblish_cast_cols(ekts, x$DT[0L, key_colnames(x, exclude = "version"), with = FALSE])
#   assert_string(var)
#   assert_subset(var, names(x$DT))
#   # version_type <- guess_time_type(x$DT$version)
#   # assert_vector(voffset, len = 1L)
#   # voffset <- time_delta_standardize(voffset, version_type, "fast")
#   validate_nice_version_lag(version_lag, x)
#   # We want default vtol to be reasonably large to in order to adapt
#   # to normal variance in pipeline schedules as well as transient
#   # pipeline issues and holiday-shifted schedules, which seem pretty
#   # common.
#   ek_vars <- c("geo_value", x$other_keys)
#   x_var_conf_ekvs <- epix_confkeys(x, var, ek_vars)
#   vtol <- vtol_preprocess(vtol, x, x_var_conf_ekvs)
#   check_dots_empty()

#   lookup_ektvs <- ekts %>%
#     mutate(version =
#              time_get_zero_lag_version(.data$time_value, .env$x$time_type, .env$x$DT$version) +
#              version_lag)

#   # Find the closest "real" version for `var` for each lookup
#   ekv_vars <- c(ek_vars, "version")
#   setDT(x_var_conf_ekvs, key = ekv_vars)
#   real_versions_info <- x_var_conf_ekvs[
#     as.list(lookup_ektvs)[ekv_vars], on = ekv_vars, roll = "nearest",
#     list(real_version = x.version, vdiff = x.version - i.version)
#   ]
#   # ^ `as.list` is needed to make `x.version` and `i.version` work

#   lookup_ektvs$version <- real_versions_info$real_version
#   result <- x$DT[
#     as.list(lookup_ektvs), on = key(x$DT), roll = TRUE,
#     var,
#     with = FALSE
#   ][[var]] # `with = FALSE` -> have to manually extract2
#   if (vtol$inclusive) {
#     result[real_versions_info[, abs(vdiff) > vtol$threshold]] <- NA
#   } else {
#     result[real_versions_info[, abs(vdiff) >= vtol$threshold]] <- NA
#   }

#   result
# }

# XXX holiday Fri -> Tue delays potentially problematic (or worse?
# check ILINet history); Fri excluding Tue is fine, but Tue will
# probably map to wrong training Fris?  Though... could this actually
# be somewhat valid and okay?  If do want to change, probably need to
# use the occasional-delays-never-early and/or source-actual ->
# source-nominal approach.



# XXX Reconsidering `vtol` arg... we are going to constrain ektvs to
# "real" ones anyway... except if vshift is nonzero, then we need the
# shifted version to be "real", not the anchor version to be "real".
# But then if we don't constrain ektvs to be "real", then the extract2
# approach may fall apart, and we will have to be joining tibbles
# again...  Also, vtol may be useful for lag analysis.
#
# No, we need to constrain analogue ektvs to also be "real" for the
# involved signal.  "Real" to "real".

# In production, there is the forecast time / the
# "safe"/late-enough-to-have-data forecast time, then there is the
# "real" version it uses.  Should we be finding analogues based on the
# former or the latter?  A shifting prediction schedule would be based
# on target real updates.  We may not want to apply that leeway to
# auxiliary covariates.  But for delayed forecasts we could also
# benefit from later versions of covariates, though that should likely
# be reflected in the test time characterization.  And this is about
# getting training data.  Prediction schedule doesn't really matter.
# Do we consider delayed target reporting valid for this purpose?
#
# Seems like may be forced into having analogue tvs be made via shifts
# of tvs, filtered to after the first real version for the target
# signal (so that predictor percentage availability metrics don't toss
# everything).  Vs. starting with the "real" rtvs.  Though... if we
# are judging by target signal anyway, perhaps can start with analogue
# rtvs based on the target signal anyway...

# Should default vtol be 3h or 3d?  Seems like may want to use floor
# of 1/2 of median inter-real version gap.  Though for daily-weekly
# need to make sure analogue rtvs are weekly, either via automatic
# rule or manual wday matching rule.

# How bad is large vtol?  Lining up with substantially more/less
# accurate data?

# Situation not just of data being more latent, but also less latent.
# Might need backoff to stale version that actually know patterns of.




# Assessing feature inclusion, availability proportion, avoiding
# overly large denominator: (a) get non-NA training targets, left_join
# all candidates, then exclude where all candidate features are NA; or
# (b) get non-NA training targets, inner_join target signal confkeys
# somehow?  but in case of lower than normal latency, don't want to
# just do based on same confkeyversion-reftime/version lag.  In (a)
# problem is having aux signal with longer history along with an
# initial batch report from the target signal.  Perhaps could do an
# approach like (a) with only lags of the target signal, or just logic
# around the feature selection window?  Or do something to try to
# remove some excess from batch reports?  Or perhaps just mark some
# larger lag that has to be there to even attempt stuff...


# Feature inclusion... what to do about features that are only helpful
# in a subset of epikeys, and unusably noisy in others?
