#' Compute lagged correlations between variables in an `epi_df` object
#'
#' Computes lagged correlations between variables in an `epi_df` object,
#' allowing for grouping by geo value, time value, or any other variables. See
#' the [correlation
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/correlation.html)
#' for examples.
#'
#' @param x The `epi_df` object under consideration.
#' @param var1,var2 The variables in `x` to correlate. 
#' @param dt1,dt2 Time shifts to consider for the two variables, respectively, 
#'   before computing correlations. Negative shifts translate into in a lag
#'   value and positive shifts into a lead value; for example, if `dt = -1`,
#'   then the new value on June 2 is the original value on June 1; if `dt = 1`,
#'   then the new value on June 2 is the original value on June 3; if `dt = 0`,
#'   then the values are left as is. Default is 0 for both `dt1` and `dt2`. Note
#'   that the time shifts are always performed *per geo value*; see details. 
#' @param by The variable(s) to group by, for the correlation computation. If
#'   `geo_value`, the default, then correlations are computed for each geo
#'   value, over all time; if `time_value`, then correlations are computed for
#'   each time, over all geo values. A grouping can also be any specified using
#'   number of columns of `x`; for example, we can use `by = c(geo_value,
#'   age_group)`, assuming `x` has a column `age_group`, in order to compute
#'   correlations for each pair of geo value and age group. To omit a grouping
#'   entirely, use `by = NULL`. Note that the grouping here is always applied
#'   *after* the time shifts; see details. 
#' @param use,method Arguments to pass to `cor()`, with "na.or.complete" the
#'   default for `use` (different than `cor()`) and "pearson" the default for
#'   `method` (same as `cor()`).
#'
#' @return An tibble with the grouping columns first (`geo_value`, `time_value`,
#'   or possibly others), and then a column `cor`, which gives the correlation.  
#'
#' @details Time shifts are always performed first, grouped by geo value (this
#'   way they amount to shifting each individual time series). After this, the
#'   geo grouping is removed, and the grouping specified in the `by` argument is
#'   applied. Then, correlations are computed. 
#' 
#' @importFrom dplyr arrange mutate summarize  
#' @importFrom stats cor
#' @importFrom rlang .data !! enquo
#' @export
epi_cor = function(x, var1, var2, dt1 = 0, dt2 = 0, by = geo_value,
                   use = "na.or.complete",
                   method = c("pearson", "kendall", "spearman")) {
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) abort("`x` must be of class `epi_df`.")

  # Check that we have variables to do computations on
  if (missing(var1)) abort("`var1` must be specified.")
  if (missing(var2)) abort("`var2` must be specified.")
  var1 = enquo(var1)
  var2 = enquo(var2)
  
  # What is the grouping? Which method?
  by = enquo(by)
  method = match.arg(method)

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
