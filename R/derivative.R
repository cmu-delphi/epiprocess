#' Estimate derivatives of a variable in an `epi_tibble` object
#' 
#' Estimates derivatives of a variable in an `epi_tibble` object, using a local
#' (in time) linear regression or alternative smoother. See the [estimating
#' derivatives  
#' vignette](https://cmu-delphi.github.io/epitools/articles/derivatives.html)   
#' for examples.  
#'
#' @param x The `epi_tibble` object under consideration.
#' @param var The variable in `x` whose derivatives are to be estimated.
#' @param method One of "lin", "ss", or "tf" indicating the method to use for
#'   the derivative calculation. To estimate the derivative at any time point,
#'   we run the given method on the last `n` time points of data, and use the 
#'   corresponding predicted derivative (that is, the derivative of the
#'   underlying estimated function, linear or spline) at the current time
#'   point. Default is "lin". See details for more explanation.
#' @param n Number of time steps to use in the running window. For example, if
#'   `n = 10`, one time step is one day, and the alignment is "right", then to
#'   estimate the derivative on November 10, we train the given method on data
#'   in between November 1 and 10. Default is 14.  
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
#' @param complete Should the computation be run over complete windows only?
#'   Default is `FALSE`, which allows for computation on partial windows.  
#' @param new_col_name String indicating the name of the new column that will
#'   contain the derivative values. Default is "deriv"; note that setting
#'   `new_col_name` equal to an existing column name will overwrite this
#'   column. 
#' @param keep_obj Should the fitted object (from linear regression, smoothing 
#'   spline, or trend filtering) be kept as a separate column? If `TRUE`, then
#'   this column name is given by  appending "_obj" to `new_col_name`. Default
#'   is `FALSE`.
#' @param deriv Order of derivative to estimate. Only orders 1 or 2 are allowed,
#'   with the default being 1. (In some cases, a second-order derivative will
#'   return a trivial result: for example: when `method` is "lin", this will 
#'   always be zero.)
#' @param time_step Optional function used to define the meaning of one time
#'   step, which if specified, overrides the default choice based on the
#'   metadata. Read the documentation for [epi_slide()] for more details.
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `TRUE`. 
#' @param ... Additional arguments to pass to the function that estimates
#'   derivatives. See details below.    
#' @return An `epi_tibble` object given by appending a new column to `x`, named
#'   according to the `new_col_name` argument, containing the derivative values.
#'
#' @details Derivatives are estimated using:
#'
#' \itemize{
#' \item Linear regression, when `method` is "lin", via `stats::lsfit()`.  
#' \item Cubic smoothing spline, when `method` is "ss", via
#'   `stats::smooth.spline()`. 
#' \item Polynomial trend filtering, when `method` is "tf", via
#'   `genlasso::trendfilter()`.
#' }
#'
#' The second and third cases base the derivative calculation on a nonparametric
#'   fit and should typically be used with a larger window `n`. The third case
#'   (trend filtering) is more locally adaptive than the second (smoothing
#'   spline) and can work better when there are sharp changes in the smoothness
#'   of the underlying values.
#' 
#' In the first and second cases (linear regression and smoothing spline), the
#'   additional arguments in `...` are directly passed to the underlying 
#'   estimation function (`stats::lsfit()` and `stats::smooth.spline()`).   
#'
#' The third case (trend filtering) works a little differently: here, a custom 
#'   set of arguments is allowed (and are internally distributed as appropriate
#'   to the functions `genlasso::trendfilter()`, `genlasso::cv.trendfilter()`,
#'   and `genlasso::coef.genlasso()`): 
#'
#' \describe{
#' \item{`ord`}{Order of piecewise polynomial for the trend filtering fit,
#'   default is 2.}
#' \item{`maxsteps`}{Maximum number of steps to take in the solution path before
#'   terminating, default is 100.} 
#' \item{`cv`}{Boolean indicating whether cross-validation should be used to
#'   choose an effective degrees of freedom for the fit, default is `FALSE`.}
#' \item{`k`}{Number of folds if cross-validation is to be used. Default is 5.} 
#' \item{`df`}{Desired effective degrees of freedom for the trend filtering
#'   fit. If `cv = FALSE`, then `df` must be an integer; if `cv = TRUE`, then
#'   `df` should be one of "min" or "1se" indicating the selection rule to use
#'   based on the cross-validation error curve (minimum or 1-standard-error
#'   rule, respectively). Default is 8 when `cv = FALSE`, and "1se" when `cv =
#'   TRUE`.} 
#' }
#' 
#' @seealso [epi_slide()]
#' @importFrom dplyr rowwise
#' @importFrom rlang abort enquo
#' @export
estimate_deriv = function(x, var, method = c("lin", "ss", "tf"), n = 14,
                          align = c("right", "center", "left"), before,
                          complete = FALSE, new_col_name = "deriv",
                          keep_obj = FALSE, deriv = 1, time_step, na_rm = TRUE,
                          ...) {
  # Check that we have a variable to do computations on
  if (missing(var)) abort("`var` must be specified.")
  var = enquo(var)
  
  # Check the method, define the slider function
  method = match.arg(method)
  slide_fun = switch(method,
                     "lin" = estimate_deriv_lin,
                     "ss" = estimate_deriv_ss,
                     "tf" = estimate_deriv_tf)
  
  # Check the derivative order
  if (!(deriv == 1 || deriv == 2)) {
    stop("`deriv` must be either 1 or 2.")
  }

  # Slide the derivative function into a temporary column
  x = epi_slide(x, slide_fun, n, align = align, before = before,
                complete = complete, new_col_name = "temp",
                new_col_type = "list", time_step = time_step,
                keep_obj = keep_obj, deriv = deriv, var = var,
                na_rm = na_rm, ...) 

  # Save the metadata (dplyr drops it)
  metadata = attributes(x)$metadata
  
  # Grab the derivative result
  x = x %>% rowwise() %>% mutate(!!new_col_name := temp$result)

  # Grab the derivative object, if we're asked to
  if (keep_obj) {
    x = x %>% rowwise() %>%
      mutate(!!paste0(new_col_name, "_obj") := list(temp$object))
  }
  
  # Delete the temp column and ungroup
  x = select(x, -temp) %>% ungroup()

  # Attach the class and metadata and return
  class(x) = c("epi_tibble", class(x))
  attributes(x)$metadata = metadata
  return(x)
}

#' Compute derivatives function via linear regression
#' @importFrom dplyr select
#' @importFrom stats lsfit coef
#' @importFrom tidyr drop_na
#' @noRd
estimate_deriv_lin = function(x, ...) { 
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here

  # If derivative order is 2, then return trivial result
  if (params$deriv == 2) return(list(object = NULL, result = 0))

  keep_obj = params$keep_obj
  var = params$var
  na_rm = params$na_rm
  
  params$keep_obj = NULL
  params$deriv = NULL
  params$var = NULL
  params$na_rm = NULL

  x = x %>% select(time_value, !!var)
  if (na_rm) x = x %>% drop_na()
  params$x = as.numeric(unlist(x[,1]))
  params$y = as.numeric(unlist(x[,2]))
  
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(lsfit, params)
    result = End(coef(object))
    if (!keep_obj) object = NULL # For memory sake
    list(object = object, result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}

#' Compute derivatives function via smoothing spline
#' @importFrom dplyr select
#' @importFrom stats smooth.spline predict
#' @importFrom tidyr drop_na
#' @noRd
estimate_deriv_ss = function(x, ...) {
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here

  keep_obj = params$keep_obj  
  deriv = params$deriv
  var = params$var
  na_rm = params$na_rm
  
  params$keep_obj = NULL
  params$deriv = NULL
  params$var = NULL
  params$na_rm = NULL
  
  x = x %>% select(time_value, !!var)
  if (na_rm) x = x %>% drop_na()
  params$x = as.numeric(unlist(x[,1]))
  params$y = as.numeric(unlist(x[,2]))
  
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(smooth.spline, params)
    result = predict(object, x = End(params$x), deriv = deriv)$y
    if (!keep_obj) object = NULL # For memory sake
    list(object = object, result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}

#' Compute derivatives function via trend filtering
#' @importFrom dplyr select
#' @importFrom genlasso trendfilter cv.trendfilter coef.genlasso
#' @importFrom tidyr drop_na
#' @noRd
estimate_deriv_tf = function(data, ...) {
  params = list(...)
  params[[1]] = NULL # dplyr::group_modify() includes the group here
  
  keep_obj = params$keep_obj
  deriv = params$deriv
  var = params$var
  na_rm = params$na_rm
  
  params$keep_obj = NULL
  params$deriv = NULL
  params$var = NULL
  params$na_rm = NULL
  
  cv = params$cv
  ord = params$ord
  maxsteps = params$maxsteps
  k = params$k
  df = params$df

  if (is.null(cv)) cv = FALSE
  if (is.null(ord)) ord = 2
  if (is.null(maxsteps)) maxsteps = 100
  if (is.null(k)) k = 5
  if (is.null(df)) df = ifelse(cv, "1se", 8)

  data = data %>% select(time_value, !!var)
  if (na_rm) data = data %>% drop_na()
  x = as.numeric(unlist(data[,1]))
  y = as.numeric(unlist(data[,2]))

  return(tryCatch(suppressWarnings(suppressMessages({
    object = trendfilter(y = y, pos = x, ord = ord, max = maxsteps)
    if (cv) {
      cv_object = quiet(cv.trendfilter(object, k = k, mode = "df"))
      df = ifelse(df == "1se", cv_object$df.1se, cv_object$df.min)
    }
    else cv_object = NULL
    
    beta = coef.genlasso(object, df = df)$beta
    beta = approx(x, beta, Min(x):Max(x))$y # TODO use poly interpolation 
    result = End(diff(beta, diff = deriv))
    
    if (!keep_obj) { object = NULL; cv_object = NULL } # For memory sake
    list(object = list(trendfilter = object, cv.trendfilter = cv_object),
         result = result)
  })),
  error = function(e) list(object = NA, result = NA)))
}
