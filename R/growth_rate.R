#' Estimate growth rate
#' 
#' Estimates the growth rate of a signal at given points along the underlying 
#' sequence. Several methodologies are available; see the [growth rate
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/growth_rate.html)
#' for examples.
#'
#' @param x Design points corresponding to the signal values `y`. Default is 
#'   `seq_along(y)` (that is, equally-spaced points from 1 to the length of 
#'   `y`).
#' @param y Signal values whose growth rates are of interest.
#' @param x0 Points at which we should estimate the growth rate. Must be a
#'   subset of `x` (no extrapolation allowed). Default is `x`. 
#' @param method Either "pct_change", "lin_reg", "smooth_spline", or
#'   "trend_filter", indicating the method to use for the growth rate
#'   calculation. The first two are local methods: they are run in a sliding
#'   fashion over the sequence (in order to estimate derivatives and hence
#'   growth rates); the latter two are global methods: they are run once over
#'   the entire sequence. See details for more explanation.
#' @param log_scale Should growth rates be estimated using the parametrization
#'   on the log scale? See details for an explanation. Default is `FALSE`.
#' @param n Number of time steps to use in the sliding window, when `method` is
#'   "pct_change" or "lin_reg". 
#'
#' a value on November 5, we apply the given function or formula to
#'   data in between November 1 and 5. Default is 14. 
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `FALSE`.
#' @param ... Additional arguments to pass to the method used to estimate the
#'   derivative. See details below.
#' @return Vector of growth rate estimates at the specified points `x0`.
#'
#' @importFrom rlang abort
#' @export
growth_rate = function(x = seq_along(y), y, x0 = x,
                       method = c("pct_change",
                                  "lin_reg",
                                  "smooth_spline",
                                  "trend_filter"),
                       log_scale = FALSE, n = 14,
                       align = c("center", "right", "left"), 
                       before, complete = FALSE, time_step,
                       na_rm = FALSE, ...) {
  # Checks on x, y, x0
  x = as.numeric(x)
  y = as.numeric(y)
  x0 = as.numeric(x0)
  if (length(x) != length(y)) abort("`x` and `y` must have the same length.")
  if (!all(x0 %in% x)) abort("`x0` must be a subset of `x`.")
}


pct_change = function(y, na_rm = FALSE) {
  N = length(y)
  if (N == 1) return(NA)
  if (N %% 2 == 1) { y = y[-1]; N = N-1 }
  a = sum(y[1:(N/2)], na.rm = na_rm)
  b = sum(y[(N/2+1):N], na.rm = na_rm)
  return(100 * (b - a) / a)
}

lin_reg = function(x, y, na_rm = FALSE) {
}
