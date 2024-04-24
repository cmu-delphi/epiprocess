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
#'   subset of `x` (no extrapolation allowed). Default is `x`.
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
#' @param dup_rm Should we check and remove duplicates in `x` (and corresponding
#'   elements of `y`) before the computation? Some methods might handle
#'   duplicate `x` values gracefully, whereas others might fail (either quietly
#'   or loudly). Default is `FALSE`.
#' @param na_rm Should missing values be removed before the computation? Default
#'   is `FALSE`.
#' @param ... Additional arguments to pass to the method used to estimate the
#'   derivative.
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
#'   spline fit to `x` and `y`, via `stats::smooth.spline()`, divided by the
#'   fitted value of the spline at `x0`.
#' * "trend_filter": uses the estimated derivative at `x0` from polynomial trend
#'   filtering (a discrete spline) fit to `x` and `y`, via
#'   `genlasso::trendfilter()`, divided by the fitted value of the discrete
#'   spline at `x0`.
#'
#' @section Log Scale:
#'  An alternative view for the growth rate of a function f in general is given
#'   by defining g(t) = log(f(t)), and then observing that g'(t) = f'(t) /
#'   f(t). Therefore, any method that estimates the derivative can be simply
#'   applied to the log of the signal of interest, and in this light, each
#'   method above ("rel_change", "linear_reg", "smooth_spline", and
#'   "trend_filter") has a log scale analog, which can be used by setting
#'   `log_scale = TRUE`.
#'
#' @section Sliding Windows:
#' For the local methods, "rel_change" and "linear_reg", we use a sliding window
#'   centered at the reference point of bandiwidth `h`. In other words, the
#'   sliding window consists of all points in `x` whose distance to the
#'   reference point is at most `h`. Note that the unit for this distance is
#'   implicitly defined by the `x` variable; for example, if `x` is a vector of
#'   `Date` objects, `h = 7`, and the reference point is January 7, then the
#'   sliding window contains all data in between January 1 and 14 (matching the
#'   behavior of `epi_slide()` with `before = h - 1` and `after = h`).
#'
#' @section Additional Arguments:
#' For the global methods, "smooth_spline" and "trend_filter", additional
#'   arguments can be specified via `...` for the underlying estimation
#'   function. For the smoothing spline case, these additional arguments are
#'   passed directly to `stats::smooth.spline()` (and the defaults are exactly
#'   as in this function). The trend filtering case works a bit differently:
#'   here, a custom set of arguments is allowed (which are distributed
#'   internally to `genlasso::trendfilter()` and `genlasso::cv.trendfilter()`):
#'
#' * `ord`: order of piecewise polynomial for the trend filtering fit. Default
#'   is 3.
#' * `maxsteps`: maximum number of steps to take in the solution path before
#'   terminating. Default is 1000.
#' * `cv`: should cross-validation be used to choose an effective degrees of
#'   freedom for the fit? Default is `TRUE`.
#' * `k`: number of folds if cross-validation is to be used. Default is 3.
#' * `df`: desired effective degrees of freedom for the trend filtering fit. If
#'   `cv = FALSE`, then `df` must be a positive integer; if `cv = TRUE`, then
#'   `df` must be one of "min" or "1se" indicating the selection rule to use
#'   based on the cross-validation error curve: minimum or 1-standard-error
#'   rule, respectively. Default is "min" (going along with the default `cv =
#'   TRUE`). Note that if `cv = FALSE`, then we require `df` to be set by the
#'   user.
#'
#' @export
#' @examples
#' # COVID cases growth rate by state using default method relative change
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   mutate(cases_gr = growth_rate(x = time_value, y = cases))
#'
#' # Log scale, degree 4 polynomial and 6-fold cross validation
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   mutate(gr_poly = growth_rate(x = time_value, y = cases, log_scale = TRUE, ord = 4, k = 6))
growth_rate <- function(x = seq_along(y), y, x0 = x,
                        method = c(
                          "rel_change", "linear_reg",
                          "smooth_spline", "trend_filter"
                        ),
                        h = 7, log_scale = FALSE,
                        dup_rm = FALSE, na_rm = FALSE, ...) {
  # Check x, y, x0
  if (length(x) != length(y)) cli_abort("`x` and `y` must have the same length.")
  if (!all(x0 %in% x)) cli_abort("`x0` must be a subset of `x`.")
  method <- match.arg(method)

  # Arrange in increasing order of x
  o <- order(x)
  x <- x[o]
  y <- y[o]

  # Convert to log(y) if we need to
  y <- as.numeric(y)
  if (log_scale) y <- log(y)

  # Remove duplicates if we need to
  if (dup_rm) {
    o <- !duplicated(x)
    if (any(!o)) {
      cli_warn(
        "`x` contains duplicate values. (If being run on a
        column in an `epi_df`, did you group by relevant key variables?)"
      )
    }
    x <- x[o]
    y <- y[o]
  }


  # Remove NAs if we need to
  if (na_rm) {
    o <- !(is.na(x) & is.na(y))
    x <- x[o]
    y <- y[o]
  }

  # Useful indices for later
  i0 <- x %in% x0

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

    return(g[i0])
  }

  # Global methods
  if (method == "smooth_spline" || method == "trend_filter") {
    # Convert to numerics
    x <- as.numeric(x)
    x0 <- as.numeric(x0)

    # Collect parameters
    params <- list(...)

    # Smoothing spline
    if (method == "smooth_spline") {
      params$x <- x
      params$y <- y
      obj <- do.call(stats::smooth.spline, params)
      f0 <- stats::predict(obj, x = x0)$y
      d0 <- stats::predict(obj, x = x0, deriv = 1)$y
      if (log_scale) {
        return(d0)
      } else {
        return(d0 / f0)
      }
    } else {
      # Trend filtering
      ord <- params$ord
      maxsteps <- params$maxsteps
      cv <- params$cv
      df <- params$df
      k <- params$k

      # Default parameters
      ord <- ord %||% 3
      maxsteps <- maxsteps %||% 1000
      cv <- cv %||% TRUE
      df <- df %||% "min"
      k <- k %||% 3

      # Check cv and df combo
      if (is.numeric(df)) cv <- FALSE
      if (!cv && !(is.numeric(df) && df == round(df))) {
        cli_abort("If `cv = FALSE`, then `df` must be an integer.")
      }

      # Compute trend filtering path
      obj <- genlasso::trendfilter(y = y, pos = x, ord = ord, max = maxsteps)

      # Use CV to find df, if we need to
      if (cv) {
        cv_obj <- quiet(genlasso::cv.trendfilter(obj, k = k, mode = "df"))
        df <- ifelse(df == "min", cv_obj$df.min, cv_obj$df.1se)
      }

      # Estimate growth rate and return
      f <- genlasso::coef.genlasso(obj, df = df)$beta
      d <- diff(f) / diff(x)
      # Extend by one element
      d <- c(d, d[length(d)])
      if (log_scale) {
        return(d[i0])
      } else {
        return((d / f)[i0])
      }
    }
  }
}
