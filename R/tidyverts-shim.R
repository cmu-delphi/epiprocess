# standins to remove dependencies on parts of tidyverts
get_period <- function(x, ...) {
  UseMethod("get_period")
}

#' @export
get_period.numeric <- function(x, ...) {
  1
}

#' @export
get_period.epi_df <- function(x, ...) {
  get_period(x$time_value)
}

#' @export
get_period.Date <- function(x, ...) {

  itval <- tsibble::interval_pull(x)
  if (inherits(itval, "vctrs_vctr")) itval <- vctrs::vec_data(itval)
  
  freq_sec <- c(year = 31557600, week = 604800, day = 86400, hour = 3600, 
                minute = 60, second = 1,
                millisecond = 1e-3, microsecond = 1e-6, nanosecond = 1e-9)
  nm <- names(itval)[itval != 0]
  if (rlang::is_empty(itval)) return(NULL)
  freqs <- switch(paste(nm, collapse = ""),
                  "unit" = c("none" = 1),
                  "year" = c("year" = 1),
                  "quarter" = c("year" = 4 / itval[["quarter"]]),
                  "month" = c("year" = 12 / itval[["month"]]),
                  "week" = c("year" = 52 / itval[["week"]]),
                  "day" = c("year" = 365.25, "week" = 7) / itval[["day"]],
                  with(
                    list(secs = freq_sec / sum(as.numeric(x) * freq_sec[nm])), 
                    secs[secs > 1]
                  )
  )
  if (rlang::is_empty(freqs)) freqs <- 1
  period <- freqs[NROW(x) / freqs >= 2]
  min(period)
}

create_seasonal_period <- function(x, time_value, ...) {
  if (is.null(x)) return(get_period(edf))
  if (is.character(x)) {
    xx <- lubridate::as.period(x)
    if (is.na(xx)) Abort(paste("Unknown period:", x))
    itval <- tsibble::interval_pull(time_value)
    itval <- with(
      itval, 
      lubridate::years(year) + 
        lubridate::period(3*quarter + month, units = "month") + 
        lubridate::weeks(week) +
        lubridate::days(day) + lubridate::hours(hour) + 
        lubridate::minutes(minute) + 
        lubridate::seconds(second) + lubridate::milliseconds(millisecond) + 
        lubridate::microseconds(microsecond) + 
        lubridate::nanoseconds(nanosecond)
    )
    period <- suppressMessages(xx / itval)
  }
  if (is.numeric(x)) period <- x
  return(period)
}
