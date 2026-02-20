
extract2_horizon <- function(x, ekvs, col, horizon, ...) UseMethod("extract2_horizon")

#' @export
extract2_horizon.epi_archive <- function(x, ekvs, col, horizon, ...) {
  ekvs <- tblish_cast_cols(ekvs, x$DT[key(x$DT)])
  validate_slide_window_arg(horizon, x$time_type, lower = -Inf, allow_inf = FALSE)
  ektvs <- ekvs
  ektvs <- tblish_inset2(ektvs, "time_value", tblish_extract2(ekvs, "version") - horizon)
  ektvs <- tblish_extract(ektvs, key(x$DT)) # in case data.table relies on col order not names
  stop("TODO finish")
  # XXX make sure to give NAs after versions_end
}
