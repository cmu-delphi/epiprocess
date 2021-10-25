#' Compute correlations between variables in an `epi_signal` object
#'
#' Computes correlations between variables in an `epi_signal` object, allowing
#' for slicing by geo location, or by time. See the [correlations
#' vignette](https://cmu-delphi.github.io/epitools/articles/correlations.html)
#' for examples.
#'
#' @param x The `epi_signal` object under consideration.
#' @param var1,var2 The variables in `x` to correlate. 
#' @param dt1,dt2 Time shifts to consider for the two variables, respectively, 
#'   before computing correlations. Negative shifts translate into in a lag
#'   value and positive shifts into a lead value; for example, if `dt = -1`,
#'   then the new value on June 2 is the original value on June 1; if `dt = 1`,
#'   then the new value on June 2 is the original value on June 3; if `dt = 0`,
#'   then the values are left as is. Default is 0 for both `dt1` and `dt2`.
#' @param by If `geo_value`, then correlations are computed for each geo value,
#'   over all time (each correlation is measured between two time series at the
#'   same location). If `time_value`, then correlations are computed for each
#'   time, over all geo values (each correlation is measured between two
#'   vectors at one time). Default is `geo_value`.
#' @param use,method Arguments to pass to `cor()`, with "na.or.complete" the
#'   default for `use` (different than `cor()`) and "pearson" the default for
#'   `method` (same as `cor()`).
#'
#' @return An tibble with first column `geo_value` or `time_value` (depending on
#'   `by`), and second column `cor`, which gives the correlation.
#'
#' @importFrom dplyr arrange group_by mutate summarize ungroup 
#' @importFrom stats cor
#' @importFrom rlang .data enquo
#' @export
sliced_cor = function(x, var1, var2, dt1 = 0, dt2 = 0,
                      by = geo_value, use = "na.or.complete",
                      method = c("pearson", "kendall", "spearman")) {
  # Check we have an `epi_signal` object
  if (!inherits(x, "epi_signal")) abort("`x` be of class `epi_signal`.")

  # Check that we have variables to do computations on
  if (missing(var1)) abort("`var1` must be specified.")
  if (missing(var2)) abort("`var2` must be specified.")
  var1 = enquo(var1)
  var2 = enquo(var2)
  
  # Which way to slice? Which method?
  by = enquo(by)
  method = match.arg(method)

  ## # Join the two data frames together by pairs of geo_value and time_value
  ## z = dplyr::full_join(x, y, by = c("geo_value", "time_value"))

  ## # Make sure that we have a complete record of dates for each geo_value
  ## z_all = dplyr::group_by(z, .data$geo_value) %>%
  ##   dplyr::summarize(time_value = seq.Date(
  ##                      as.Date(min(.data$time_value)),
  ##                      as.Date(max(.data$time_value)),
  ##                      by = "day")) %>%
  ##   dplyr::ungroup()
  
  ## z = dplyr::full_join(z, z_all, by = c("geo_value", "time_value"))

  # Perform time shifts, then compute appropriate correlations and return
  return(x %>%
         group_by(.data$geo_value) %>%
         arrange(.data$time_value) %>%
         mutate(var1 = shift(!!var1, n = dt1),
                var2 = shift(!!var2, n = dt2)) %>%
         ungroup() %>%
         group_by(!!by) %>%
         summarize(cor = cor(x = .data$var1, y = .data$var2,
                             use = use, method = method)))
}

# Function to perform time shifts, lag or lead
shift = function(var, n) {
  if (n < 0) return(dplyr::lag(var, -n))
  else return(dplyr::lead(var, n))
}
