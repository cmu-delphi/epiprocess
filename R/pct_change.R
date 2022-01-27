#' Compute percentage change 
#' 
#' Computes the percentage change over a sequence of values, defined as 100 * (B
#' / A - 1), where A is the sum of the first half of the values and B is the sum
#' of the second half. This is a convenience function to be used inside of a
#' call to `epi_slide()`, see the [percentage change
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/pct_change.html)
#' for examples.
#'
#' @param var The variable whose percentage change value is to be computed. If
#'   the length of `var` is odd, then the first value is ignored for the sake of
#'   the computation.
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `FALSE`.
#'
#' @export
pct_change = function(var, na_rm = FALSE) {
  N = length(var)
  if (N == 1) return(NA)
  if (N %% 2 == 1) { var = var[-1]; N = N-1 }
  a = sum(var[1:(N/2)], na.rm = na_rm)
  b = sum(var[(N/2+1):N], na.rm = na_rm)
  return(100 * (b - a) / a)
}
