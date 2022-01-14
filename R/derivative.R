#' Estimate derivative
#' 
#' Estimates the derivative at the end of a sequence of values, using linear
#' regression, a smoothing spline, or trend filtering. This is a convenience
#' function to be used inside of a call to `epi_slide()`, see the [estimating
#' derivatives
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/derivative.html)
#' for examples.
#'
#' @param var The variable whose derivative is to be estimated. 
#' @param method One of "lin", "ss", or "tf" indicating the method to use for
#'   the derivative calculation. We run the given method using `var` as the
#'   response variable, assuming an equally-spaced sequence of design points,
#'   and return the corresponding estimated derivative (that is, the derivative 
#'   of the underlying estimated function, linear or spline) at the end of the 
#'   sequence. Default is "lin". See details for more explanation.
#' @param deriv Order of derivative to estimate. Only orders 1 or 2 are
#'   supported, with the default being 1. (In some cases, a second-order
#'   derivative will return a trivial result; for example: when `method` is
#'   "lin", this will always be zero.)
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `FALSE`. 
#' @param keep_obj Should the fitted object (from linear regression, smoothing
#'   spline, or trend filtering) be returned, along with the estimated
#'   derivative, in a list? This is typically to perform some kind of post hoc
#'   inspection. Default is `FALSE`.
#' @param ... Additional arguments to pass to the method used to estimate the 
#'   derivative. See details below. 
#' 
#' @details The derivative is estimated using:
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
#'   fit and should typically be used with a larger window `n` in a parent call
#'   to `epi_slide()`. The third case (trend filtering) is more locally adaptive
#'   than the second (smoothing spline) and can work better when there are sharp
#'   changes in the smoothness of the underlying values.
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
#' @export
estimate_deriv = function(var, method = c("lin", "ss", "tf"), deriv = 1,
                          na_rm = FALSE, keep_obj = FALSE, ...) {
  # Check the method, define the deriv function
  method = match.arg(method)
  estimate_deriv_fun = switch(method,
                              "lin" = estimate_deriv_lin,
                              "ss" = estimate_deriv_ss,
                              "tf" = estimate_deriv_tf)

  # Check the derivative order
  if (!(deriv == 1 || deriv == 2)) stop("`deriv` must be either 1 or 2.")

  return(estimate_deriv_fun(var, deriv, na_rm, keep_obj, ...))
}

#' Compute derivative function via linear regression
#' @importFrom stats lsfit coef
#' @noRd
estimate_deriv_lin = function(var, deriv, na_rm, keep_obj, ...) { 
  params = list(...)

  # If derivative order is 2, then return trivial result
  if (deriv == 2) {
    if (!keep_obj) return(0)
    else return(list(deriv = 0, object = NULL))
  }

  N = length(var)
  params$x = 1:N
  params$y = var

  # Manually remove NAs if asked to
  if (na_rm) {
    o = !is.na(var)
    params$x = params$x[o]
    params$y = params$y[o]
  }
  
  # Try to run linear regression
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(lsfit, params)
    deriv = End(coef(object))
    if (!keep_obj) return(deriv)
    else return(list(deriv = deriv, object = object))
  })),
  error = function(e) return(NA)))
}

#' Compute derivative function via smoothing spline
#' @importFrom stats smooth.spline predict
#' @noRd
estimate_deriv_ss = function(var, deriv, na_rm, keep_obj, ...) { 
  params = list(...)

  N = length(var)
  params$x = 1:N
  params$y = var

  # Manually remove NAs if asked to
  if (na_rm) {
    o = !is.na(var)
    params$x = params$x[o]
    params$y = params$y[o]
  }

  # Try to fit a smoothing spline
  return(tryCatch(suppressWarnings(suppressMessages({
    object = do.call(smooth.spline, params)
    deriv = predict(object, x = End(params$x), deriv = deriv)$y
    if (!keep_obj) return(deriv)
    else return(list(deriv = deriv, object = object))
  })),
  error = function(e) return(NA)))
}

#' Compute derivative function via trend filtering
#' @importFrom genlasso trendfilter cv.trendfilter coef.genlasso
#' @noRd
estimate_deriv_tf = function(var, deriv, na_rm, keep_obj, ...) { 
  params = list(...)
  
  N = length(var)
  x = 1:N
  y = var

  # Manually remove NAs if asked to
  if (na_rm) {
    o = !is.na(var)
    x = x[o]
    y = y[o]
  }

  cv = params$cv
  ord = params$ord
  maxsteps = params$maxsteps
  k = params$k
  df = params$df
  
  # Setup some default parameters
  if (is.null(cv)) cv = FALSE
  if (is.null(ord)) ord = 2
  if (is.null(maxsteps)) maxsteps = 100
  if (is.null(k)) k = 5
  if (is.null(df)) df = ifelse(cv, "1se", 8)

  # Try to run trend filtering
  return(tryCatch(suppressWarnings(suppressMessages({
    object = trendfilter(y = y, pos = x, ord = ord, max = maxsteps)
    if (cv) {
      cv_object = quiet(cv.trendfilter(object, k = k, mode = "df"))
      df = ifelse(df == "1se", cv_object$df.1se, cv_object$df.min)
    }
    else cv_object = NULL
    
    beta = coef.genlasso(object, df = df)$beta
    beta = approx(x, beta, Min(x):Max(x))$y # TODO use poly interpolation?
    deriv = End(diff(beta, diff = deriv))
    
    if (!keep_obj) return(deriv)
    else return(list(deriv = deriv, object = object, cv_object = cv_object))
  })),
  error = function(e) return(NA)))
}
