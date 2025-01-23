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
#' @param y Signal values.
#' @param x0 Points at which we should estimate the growth rate. Must be a
#'   contained in the range of `x` (no extrapolation allowed). Default is `x`.
#' @param method Either "rel_change", "linear_reg", "smooth_spline", or
#'   "trend_filter", indicating the method to use for the growth rate
#'   calculation. The first two are local methods: they are run in a sliding
#'   fashion over the sequence (in order to estimate derivatives and hence
#'   growth rates); the latter two are global methods: they are run once over
#'   the entire sequence. See details for more explanation.
#' @param h Bandwidth for the sliding window, when `method` is "rel_change" or
#'   "linear_reg". See details for more explanation.
#' @param log_scale Should growth rates be estimated using the parametrization
#'   on the log scale? See details for an explanation. Default is `FALSE`.
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `FALSE`.
#' @param params Additional arguments to pass to the method used to estimate the
#'   derivative. This should be created with `growth_rate_global_params()`.
#' @return Vector of growth rate estimates at the specified points `x0`.
#'
#' @details The growth rate of a function f defined over a continuously-valued
#'   parameter t is defined as f'(t) / f(t), where f'(t) is the derivative of f
#'   at t. To estimate the growth rate of a signal in discrete-time (which can
#'   be thought of as evaluations or discretizations of an underlying function
#'   in continuous-time), we can therefore estimate the derivative and divide by
#'   the signal value itself (or possibly a smoothed version of the signal
#'   value).
#'
#' The following methods are available for estimating the growth rate:
#'
#' * "rel_change": uses (B/A - 1) / h, where B is the average of `y` over the
#'   second half of a sliding window of bandwidth h centered at the reference
#'   point `x0`, and A the average over the first half. This can be seen as
#'   using a first-difference approximation to the derivative.
#' * "linear_reg": uses the slope from a linear regression of `y` on `x` over a
#'   sliding window centered at the reference point `x0`, divided by the fitted
#'   value from this linear regression at `x0`.
#' * "smooth_spline": uses the estimated derivative at `x0` from a smoothing
#'   spline fit to `x` and `y`, via [stats::smooth.spline()], divided by the
#'   fitted value of the spline at `x0`.
#' * "trend_filter": uses the estimated derivative at `x0` from polynomial trend
#'   filtering (a discrete spline) fit to `x` and `y`, via
#'   [trendfilter::trendfilter()], divided by the fitted value of the discrete
#'   spline at `x0`. This method requires the
#'   [`{trendfilter}` package](https://github.com/glmgen/trendfilter)
#'   to be installed (if it isn't, `method = "rel_change"` will be used and a
#'   warning issued).
#'
#' ## Log Scale
#'
#'  An alternative view for the growth rate of a function f in general is given
#'   by defining g(t) = log(f(t)), and then observing that g'(t) = f'(t) /
#'   f(t). Therefore, any method that estimates the derivative can be simply
#'   applied to the log of the signal of interest, and in this light, each
#'   method above ("rel_change", "linear_reg", "smooth_spline", and
#'   "trend_filter") has a log scale analog, which can be used by setting
#'   `log_scale = TRUE`.
#'
#' ## Sliding Windows
#'
#' For the local methods, "rel_change" and "linear_reg", we use a sliding window
#'   centered at the reference point of bandiwidth `h`. In other words, the
#'   sliding window consists of all points in `x` whose distance to the
#'   reference point is at most `h`. Note that the unit for this distance is
#'   implicitly defined by the `x` variable; for example, if `x` is a vector of
#'   `Date` objects, `h = 7`, and the reference point is January 7, then the
#'   sliding window contains all data in between January 1 and 14 (matching the
#'   behavior of `epi_slide()` with `before = h - 1` and `after = h`).
#'
#' ## Additional Arguments
#'
#' For the global methods, "smooth_spline" and "trend_filter", additional
#'   arguments can be specified via `params` for the underlying estimation
#'   function. These additional arguments are
#'   passed to [stats::smooth.spline()], [trendfilter::trendfilter()], or
#'   [trendfilter::cv_trendfilter()]. The defaults are exactly
#'   as specified in those functions, except when the arguments are shared
#'   between these. These cases are as follows:
#'
#' * `df`: desired effective degrees of freedom. For "smooth_spline", this must be numeric (or `NULL`) and will
#'   be passed along to the underlying function. For "trend_filter", if
#'   `cv = FALSE`, then `df` must be a positive number (integer is most sensible);
#'   if `cv = TRUE`, then `df` must be one of "min" or "1se" indicating the
#'   selection rule to use
#'   based on the cross-validation error curve: minimum or 1-standard-error
#'   rule, respectively. The default is "min" (going along with the default
#'   `cv = TRUE`).
#' * `lambda`: For "smooth_spline", this should be a scalar value or `NULL`.
#'   For "trend_filter", this is allowed to also be a vector, as long as either
#'   `cv = TRUE` or `df` is specified.
#' * `cv`: should cross-validation be used to choose an effective degrees of
#'   freedom for the fit? The default is `FALSE` to match [stats::smooth.spline()].
#'   In that case, as in that function, GCV is used instead.
#'   For :trend_filter", this will be coerced to `TRUE` if neither
#'   `df` nor `lambda` are specified (the default).
#'   Note that passing both `df` and a scalar `lambda` will always be an error.
#'
#' @export
#' @examples
#' # COVID cases growth rate by state using default method relative change
#' cases_deaths_subset %>%
#'   group_by(geo_value) %>%
#'   mutate(cases_gr = growth_rate(x = time_value, y = cases))
#'
#' # Degree 3 polynomial and 5-fold cross validation on the log scale
#' # some locations report 0 cases, so we replace these with 1
#' cases_deaths_subset %>%
#'   group_by(geo_value) %>%
#'   mutate(gr_poly = growth_rate(
#'     x = time_value, y = pmax(cases, 1), method = "trend_filter",
#'     log_scale = TRUE, na_rm = TRUE
#'   ))
growth_rate <- function(
    y, x = seq_along(y), x0 = x,
    method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
    h = 7, log_scale = FALSE, na_rm = FALSE,
    params = growth_rate_global_params()) {
  # Check x, y, x0
  if (length(x) != length(y)) cli_abort("`x` and `y` must have the same length.")
  method <- rlang::arg_match(method)
  assert_class(params, "growth_rate_params")
  if (any(is.na(x)) || any(is.na(x0))) {
    cli_abort("Neither `x` nor `x0` may contain `NA`s.")
  }
  dups <- duplicated(x)
  if (any(dups)) {
    cli_abort(
      "`x` contains duplicate values. (If being run on a
        column in an `epi_df`, did you group by relevant key variables?)"
    )
  }
  if (method == "trend_filter" && !requireNamespace("trendfilter", quietly = TRUE)) {
    method <- "rel_change"
    cli_warn(c(
      "The {.pkg trendfilter} package must be installed to use this option.",
      i = "It is available at {.url https://github.com/glmgen/trendfilter}.",
      i = 'The computation will proceed using `method = "rel_change"` instead.'
    ))
  }

  # Arrange in increasing order of x
  if (min(x0) < min(x) || max(x0) > max(x)) {
    cli_abort("`x0` must be contained in `[min(x), max(x)]`.")
  }
  o <- order(x)
  x <- x[o]
  y <- y[o]
  n <- length(y)
  x0 <- sort(x0)

  # Convert to log(y) if we need to
  y <- as.numeric(y)
  if (log_scale) {
    if (any(y <= 0)) {
      cli_warn("`y` contains 0 or negative values. Taking logs may produce
               strange results.")
    }
    y <- suppressWarnings(log(y))
  }

  if (!is.finite(y[1]) || !is.finite(y[n])) {
    cli_abort("Either the first or last `y` values are not finite. This may be
              due to `log_scale = TRUE`.")
  }
  good_obs <- (!is.na(y) | !na_rm) & is.finite(y)
  x <- x[good_obs]
  y <- y[good_obs]
  x <- as.numeric(x)
  x0 <- as.numeric(x0)

  # Local methods
  if (method == "rel_change" || method == "linear_reg") {
    g <- purrr::map_dbl(x, function(x_ref) {
      # Form the local window
      ii <- (x > x_ref - h) & (x <= x_ref + h)
      xx <- x[ii]
      yy <- y[ii]

      # Convert to numerics
      x_ref <- as.numeric(x_ref)
      xx <- as.numeric(xx)

      # Relative change
      if (method == "rel_change") {
        right <- xx > x_ref
        left <- xx <= x_ref
        b <- mean(yy[right])
        a <- mean(yy[left])
        hh <- mean(xx[right]) - mean(xx[left])
        if (log_scale) {
          return((b - a) / hh)
        } else {
          return((b / a - 1) / hh)
        }
      } else {
        # Linear regression
        xm <- xx - mean(xx)
        ym <- yy - mean(yy)
        b <- sum(xm * ym) / sum(xm^2)
        a <- mean(yy - b * xx)
        if (log_scale) {
          return(b)
        } else {
          return(b / (a + b * x_ref))
        }
      }
    })
    return(stats::approx(x, g, x0)$y)
  }

  # Global methods
  if (method == "smooth_spline" || method == "trend_filter") {
    if (any(is.na(x) | is.na(y) | !is.finite(x) | !is.finite(y))) {
      cli_abort(c(
        "{.val {method}} requires all real values without missingness.",
        i = "Set `na_rm = TRUE` and / or check for infinite values.",
        i = "Using `log_scale = TRUE` may induce either case."
      ))
    }

    if (method == "smooth_spline") {
      if (is.character(params$df)) params$df <- NULL
      if (length(params$lambda) > 1L) {
        cli_abort("{.val smooth_spline} requires 1 `lambda` but more were used.")
      }
      params <- params[c("df", "spar", "lambda", "cv", "all.knots", "df.offset", "penalty")]
      params <- params[!sapply(params, is.null)]
      obj <- rlang::inject(stats::smooth.spline(x = x, y = y, !!!params))
      f0 <- stats::predict(obj, x = x0)$y
      d0 <- stats::predict(obj, x = x0, deriv = 1)$y
      if (log_scale) {
        return(d0)
      } else {
        return(d0 / f0)
      }
    } else { # Trend filtering
      params <- parse_trendfilter_params(params)
      sdy <- stats::sd(y) # speeds computation significantly
      y <- y / sdy
      if (params$cv) {
        obj <- trendfilter::cv_trendfilter(
          y, x,
          k = params$k, error_measure = params$error_measure,
          nfolds = params$nfolds, family = params$family, lambda = params$lambda,
          nlambda = params$nlambda, lambda_max = params$lambda_max,
          lambda_min = params$lambda_min, lambda_min_ratio = params$lambda_min_ratio
        )
        lam <- params$df
        which_lambda <- paste0("lambda_", lam)
        f <- stats::predict(obj, newx = x0, which_lambda = which_lambda) * sdy
      } else {
        obj <- trendfilter::trendfilter(
          y, x,
          k = params$k, family = params$family, lambda = params$lambda,
          nlambda = params$nlambda, lambda_max = params$lambda_max,
          lambda_min = params$lambda_min, lambda_min_ratio = params$lambda_min_ratio
        )
        single_lambda <- length(obj$lambda == 1L)
        lam <- ifelse(single_lambda, obj$lambda, obj$lambda[which.min(abs(params$df - obj$dof))])
        f <- stats::predict(obj, newx = x0, lambda = lam) * sdy
      }

      d <- diff(f) / diff(x0)
      # Extend by one element
      d <- c(d, d[length(d)])
      if (log_scale) {
        return(d)
      } else {
        return(d / f)
      }
    }
  }
}

#' Optional parameters for global growth rate methods
#'
#' Construct an object containing non-standard arguments for [growth_rate()].
#'
#' @param df Numeric or NULL for "smooth_spline". May also be one of "min" or
#'   "max" in the case of "trend_filter". The desired equivalent number of
#'   degrees of freedom of the fit. Lower values give smoother estimates.
#' @param lambda The desired smoothing parameter. For "smooth_spline", this
#'   can be specified instead of `spar`. For "trend_filter", this sequence
#'   determines the balance between data fidelity and smoothness of the
#'   estimated curve; larger `lambda` results in a smoother estimate. The
#'   default, `NULL` results in an automatic computation based on `nlambda`,
#'   the largest value of `lambda` that would result in a maximally smooth
#'   estimate, and `lambda_min_ratio`. Supplying a value of `lambda` overrides
#'   this behaviour.
#' @param cv For "smooth_spline", ordinary leave-one-out (`TRUE`) or ‘generalized’
#'   cross-validation (GCV) when `FALSE`; is used for smoothing parameter computation
#'   only when both `spar` and `df` are not specified. For "trend_filter",
#'   `cv` determines whether or not cross-validation is used to choose the
#'   tuning parameter. If `FALSE`, then the user must specify either `lambda`
#'   or `df`.
#' @inheritParams stats::smooth.spline
#' @inheritParams trendfilter::trendfilter
#' @inheritParams trendfilter::cv_trendfilter
#'
#' @return A list of parameter configurations.
#' @importFrom checkmate assert_number
#' @export
growth_rate_global_params <- function(
    df = NULL,
    lambda = NULL,
    cv = FALSE,
    spar = NULL,
    all.knots = FALSE,
    df.offset = 0,
    penalty = 1,
    k = 3L,
    family = c("gaussian", "logistic", "poisson"),
    nlambda = 50L,
    lambda_max = NULL,
    lambda_min = NULL,
    lambda_min_ratio = 1e-5,
    error_measure = c("deviance", "mse", "mae"),
    nfolds = 3L) {
  if (is.character(df)) {
    df <- rlang::arg_match0(df, c("min", "1se"))
  } else {
    assert_number(df, lower = 0, null.ok = TRUE, finite = TRUE)
  }
  assert_number(spar, null.ok = TRUE, finite = TRUE)
  assert_numeric(lambda, lower = 0, null.ok = TRUE, finite = TRUE)
  assert_logical(cv, len = 1)
  assert_logical(all.knots, len = 1)
  assert_number(df.offset, lower = 0, finite = TRUE)
  assert_number(penalty, lower = 0, finite = TRUE)
  checkmate::assert_integerish(k, lower = 0, len = 1)
  family <- arg_match(family)
  assert_number(nlambda, lower = 0, finite = TRUE)
  assert_number(lambda_max, lower = 0, finite = TRUE, null.ok = TRUE)
  assert_number(lambda_min, lower = 0, finite = TRUE, null.ok = TRUE)
  assert_number(lambda_min_ratio, lower = 0, upper = 1)
  error_measure <- arg_match(error_measure)
  checkmate::assert_integerish(nfolds, lower = 2, len = 1)

  structure(enlist(
    df, lambda, cv, # shared by all
    spar, all.knots, df.offset, penalty, # smooth.spline
    k, family, nlambda, lambda_max, lambda_min, lambda_min_ratio, # all TF
    error_measure, nfolds # cv_trendfilter
  ), class = "growth_rate_params")
}

#' @export
print.growth_rate_params <- function(x, ...) {
  utils::str(x, give.attr = FALSE)
}

parse_trendfilter_params <- function(params) {
  assert_class(params, "growth_rate_params")
  vec_lambda <- checkmate::test_numeric(params$lambda, min.len = 2L, null.ok = TRUE)
  df_cv <- checkmate::test_character(params$df, null.ok = TRUE)
  if (df_cv && vec_lambda) {
    params$cv <- TRUE # Turn CV on (or leave it on)
    params$df <- params$df %||% "min" # use the original arg or provide the minimizer
    return(params)
  }
  if (params$cv) { # CV = TRUE on input but conflicts with other custom args
    cli_abort(
      "When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be
      `NULL` or a vector."
    )
  } else { # CV should stay FALSE
    if (!vec_lambda) {
      if (is.character(params$df)) {
        cli_abort(
          "`df` a character implies using CV, but also setting `lambda` to a
          single value implies no CV."
        )
      }
      if (is.numeric(params$df)) {
        cli_abort("`df` and `lambda` cannot both be scalars.")
      }
    }
  }
  # If we got here, we fit TF. There are two possibilities:
  # 1. df is NULL and lambda is a scalar
  # 2. df is numeric and lambda is either NULL or a vector (vec_lambda = TRUE)
  return(params)
}
