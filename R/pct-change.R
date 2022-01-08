#' Compute percentage change of a variable in an `epi_tibble` object
#' 
#' Computes the percentage change of a variable in an `epi_tibble` object. See
#' the [percentage change
#' vignette](https://cmu-delphi.github.io/epitools/articles/pct-change.html) for  
#' examples.   
#'
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` whose percentage change values are to be
#'   computed.    
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 10`, one time step is one day, and the alignment is "right", then the
#'   percentage change value computed on November 10 is 100 * (B - A) / A, where
#'   A is the sum of the variable in between November 6 and 10, and A is the sum
#'   in between November 1 and 5. Note that `n` must be even; default is 14.
#' @param align One of "right", "center", or "left", indicating the alignment of
#'   the sliding window relative to the reference time point. If the alignment
#'   is "center" and `n` is even, then one more time point will be used before
#'   the reference time point than after. Default is "right".
#' @param before Optional integer, in the range to 0 to `n-1` (inclusive),
#'   specifying the number of time points to use in the sliding window strictly
#'   before the reference time point. For example, setting `before = n-1` would
#'   be the same as setting `align = "right"`. The current argument allows for
#'   more flexible specification of alignment than the `align` parameter, and if
#'   specified, then it overrides `align`.
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.  
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. Read the documentation for [epi_slide()] for more details.
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `TRUE`. 
#' @return An `epi_tibble` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the percentage change
#'   values.  
#'
#' @seealso [epi_slide()]
#' @importFrom rlang abort enquo
#' @export
pct_change = function(x, var, n = 14, align = c("right", "center", "left"),
                      before, new_col_name = "pct_change", time_step,
                      na_rm = TRUE) { 
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)
    
  # Check that n is even
  if (n %% 2 == 1) abort("`n` must be even.")

  # Slide the percentage change function and return
  return(epi_slide(x, pct_change_fun, n, align = align, before = before,
                   new_col_name = new_col_name, time_step = time_step,
                   var = var, N = n, na_rm = na_rm)) 
}

#' Compute percentage change function
#' @noRd
pct_change_fun = function(x, ...) {
  params = list(...)
  var = params$var
  N = params$N
  na_rm = params$na_rm
  
  if (nrow(x) != N) return(NA)
  v = x %>% pull(!!var)

  a = sum(v[1:(N/2)], na.rm = na_rm)
  b = sum(v[(N/2+1):N], na.rm = na_rm)
  return(100 * (b - a) / a)
}
