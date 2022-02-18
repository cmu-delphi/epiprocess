#' Compute correlations between variables in an `epi_df` object
#'
#' Computes correlations between variables in an `epi_df` object, allowing for
#' grouping by geo value, time value, or any other variables. See the
#' correlation vignette for examples.
#'
#' @param x The `epi_df` object under consideration.
#' @param var1,var2 The variables in `x` to correlate.
#' @param dt1,dt2 Time shifts to consider for the two variables, respectively, 
#'   before computing correlations. Negative shifts translate into in a lag
#'   value and positive shifts into a lead value; for example, if `dt = -1`,
#'   then the new value on June 2 is the original value on June 1; if `dt = 1`,
#'   then the new value on June 2 is the original value on June 3; if `dt = 0`,
#'   then the values are left as is. Default is 0 for both `dt1` and `dt2`.
#' @param shift_by The variables(s) to group by, for the time shifts. The
#'   default is `geo_value`. However, we could also use, for example, `shift_by
#'   = c(geo_value, age_group)`, assuming `x` has a column `age_group`, to
#'   perform time shifts per geo value and age group. To omit a grouping
#'   entirely, use `cor_by = NULL`. Note that the grouping here is always undone
#'   *before* the correlation computations.
#' @param cor_by The variable(s) to group by, for the correlation
#'   computations. If `geo_value`, the default, then correlations are computed
#'   for each geo value, over all time; if `time_value`, then correlations are
#'   computed for each time, over all geo values. A grouping can also be any
#'   specified using number of columns of `x`; for example, we can use `cor_by =
#'   c(geo_value, age_group)`, assuming `x` has a column `age_group`, in order
#'   to compute correlations for each pair of geo value and age group. To omit a
#'   grouping entirely, use `cor_by = NULL`. Note that the grouping here is
#'   always done *after* the time shifts.
#' @param use,method Arguments to pass to `cor()`, with "na.or.complete" the
#'   default for `use` (different than `cor()`) and "pearson" the default for
#'   `method` (same as `cor()`).
#'
#' @return An tibble with the grouping columns first (`geo_value`, `time_value`,
#'   or possibly others), and then a column `cor`, which gives the correlation. 
#' 
#' @importFrom stats cor
#' @importFrom rlang .data !! !!! enquo syms
#' @importFrom tidyselect eval_select
#' @export
epi_cor = function(x, var1, var2, dt1 = 0, dt2 = 0, shift_by = geo_value,
                   cor_by = geo_value, use = "na.or.complete",
                   method = c("pearson", "kendall", "spearman")) {
  # Check we have an `epi_df` object
  if (!inherits(x, "epi_df")) abort("`x` must be of class `epi_df`.")

  # Check that we have variables to do computations on
  if (missing(var1)) abort("`var1` must be specified.")
  if (missing(var2)) abort("`var2` must be specified.")
  var1 = enquo(var1)
  var2 = enquo(var2)
  
  # Defuse grouping variables. This looks a bit more involved since we want to
  # accomodate the option of specifying multiple variables for each grouping.
  # Hence use the power of tidyselect::eval_select(), which can accomodate any
  # arguments of the form:
  # * cor_by = a
  # * cor_by = "a"
  # * cor_by = c(a, b)
  # * cor_by = c("a", "b")
  # and so on, and similarly for shift_by. Note: make sure to follow with !!!
  cor_by = syms(names(eval_select(enquo(cor_by), x)))
  shift_by = syms(names(eval_select(enquo(shift_by), x)))

  # Which method?
  method = match.arg(method)

  # Perform time shifts, then compute appropriate correlations and return
  return(x %>%
         dplyr::group_by(!!!shift_by) %>%
         dplyr::arrange(.data$time_value) %>%
         dplyr::mutate(var1 = shift(!!var1, n = dt1),
                       var2 = shift(!!var2, n = dt2)) %>%
         dplyr::ungroup() %>%
         dplyr::group_by(!!!cor_by) %>%
         dplyr::summarize(cor = cor(x = .data$var1, y = .data$var2,
                                    use = use, method = method)))
}

# Function to perform time shifts, lag or lead
shift = function(var, n) {
  if (n < 0) return(dplyr::lag(var, -n))
  else return(dplyr::lead(var, n))
}
