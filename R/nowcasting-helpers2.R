
extract2_horizon <- function(x, ekvs, col, horizon, ...) UseMethod("extract2_horizon")

#' @export
extract2_horizon.epi_archive <- function(x, ekvs, col, horizon, ...) {
  ekvs <- tblish_cast_cols(ekvs, x$DT[key(x$DT)])
  validate_slide_window_arg(horizon, x$time_type, lower = -Inf, allow_inf = FALSE)
  ektvs <- ekvs
  tblish_col(ektvs, "time_value") <- tblish_col(ektvs, "version") - horizon
  ektvs <- tblish_select(ektvs, key(x$DT)) # in case data.table relies on col order not names
  stop("TODO finish")
  # XXX make sure to give NAs after versions_end
}
