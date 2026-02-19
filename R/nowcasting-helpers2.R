
extract2_horizon <- function(x, ekvs, col, horizon, ...) UseMethod("extract2_horizon")

#' @export
extract2_horizon.epi_archive <- function(x, ekvs, col, horizon, ...) {
  ekvs <- tblish_cast_cols(ekvs, x$DT[key(x$DT)])
  validate_slide_window_arg(horizon, x$time_type, lower = -Inf, allow_inf = FALSE)
  ektvs <- ekvs
  ektvs$time_value <- ekvs$version - horizon
  ektvs <- ektvs[key(x$DT)] # just in case
  stop("TODO finish")
}
