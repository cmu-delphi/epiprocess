#' Compute percentage change of a variable in an `epi_tibble` object
#' 
#' Computes the percentage change of a variable in an `epi_tibble` object. See
#' the [percentage change
#' vignette](https://cmu-delphi.github.io/epitools/articles/pct-change.html) for  
#' examples.   
#'
#' @param x The `epi_tibble` object.
#' @param var The variable in `x` whose percentage change values are to be
#'   computed.    
#' @param n Number of time steps to use in the trailing window. For example, if 
#'   `n = 10`, and one time step is one day, then the percentage change value 
#'   computed on November 10 is 100 * (B - A) / A, where A is the sum of the
#'   variable in between November 6 and 10, and A is the sum in between November
#'   1 and 5. Note that `n` must be even; default is 14.
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "slide_value"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this column.  
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. Read the documentation for [epi_slide()] for more details.
#' @return An `epi_tibble` object given by appending a new column to `x`, named 
#'   according to the `new_col_name` argument, containing the percentage change
#'   values.  
#'
#' @seealso [epi_slide()]
#' @importFrom rlang abort enquo
#' @export
pct_change = function(x, var, n = 14, new_col_name = "pct_change", time_step) {
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)
    
  # Check that n is even
  if (n %% 2 == 1) abort("`n` must be even.")

  # Slide the percentage change function and return
  return(epi_slide(x, pct_change_fun, n, new_col_name, var = var, N = n))
}

#' Compute percentage change function
#' @noRd
pct_change_fun = function(x, ...) {
  params = list(...)
  var = params$var
  N = params$N
  if (nrow(x) != N) return(NA)
  v = x %>% pull(!!var)

  a = Sum(v[1:(N/2)])
  b = Sum(v[(N/2+1):N])
  return(100 * (b - a) / a)
}
