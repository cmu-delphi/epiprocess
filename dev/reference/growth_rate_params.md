# Optional parameters for growth rate methods

Construct an object containing non-standard arguments for
[`growth_rate()`](https://cmu-delphi.github.io/epiprocess/dev/reference/growth_rate.md).

## Usage

``` r
growth_rate_params(
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
  lambda_min_ratio = 0.00001,
  error_measure = c("deviance", "mse", "mae"),
  nfolds = 3L
)
```

## Arguments

- df:

  Numeric or NULL for "smooth_spline". May also be one of "min" or "max"
  in the case of "trend_filter". The desired equivalent number of
  degrees of freedom of the fit. Lower values give smoother estimates.

- lambda:

  The desired smoothing parameter. For "smooth_spline", this can be
  specified instead of `spar`. For "trend_filter", this sequence
  determines the balance between data fidelity and smoothness of the
  estimated curve; larger `lambda` results in a smoother estimate. The
  default, `NULL` results in an automatic computation based on
  `nlambda`, the largest value of `lambda` that would result in a
  maximally smooth estimate, and `lambda_min_ratio`. Supplying a value
  of `lambda` overrides this behaviour.

- cv:

  For "smooth_spline", ordinary leave-one-out (`TRUE`) or ‘generalized’
  cross-validation (GCV) when `FALSE`; is used for smoothing parameter
  computation only when both `spar` and `df` are not specified. For
  "trend_filter", `cv` determines whether or not cross-validation is
  used to choose the tuning parameter. If `FALSE`, then the user must
  specify either `lambda` or `df`.

- spar:

  smoothing parameter, typically (but not necessarily) in \\(0,1\]\\.
  When `spar` is specified, the coefficient \\\lambda\\ of the integral
  of the squared second derivative in the fit (penalized log likelihood)
  criterion is a monotone function of `spar`, see the details below.
  Alternatively `lambda` may be specified instead of the *scale free*
  `spar`=\\s\\.

- all.knots:

  if `TRUE`, all distinct points in `x` are used as knots. If `FALSE`
  (default), a subset of `x[]` is used, specifically `x[j]` where the
  `nknots` indices are evenly spaced in `1:n`, see also the next
  argument `nknots`.

  Alternatively, a strictly increasing
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector specifying
  “all the knots” to be used; must be rescaled to \\\[0, 1\]\\ already
  such that it corresponds to the `ans $ fit$knots` sequence returned,
  not repeating the boundary knots.

- df.offset:

  allows the degrees of freedom to be increased by `df.offset` in the
  GCV criterion.

- penalty:

  the coefficient of the penalty for degrees of freedom in the GCV
  criterion.

- k:

  Integer. Degree of the piecewise polynomial curve to be estimated. For
  example, `k = 0` corresponds to a piecewise constant curve.

- family:

  Character or function. Specifies the loss function to use. Valid
  options are:

  - `"gaussian"` - least squares loss (the default),

  - `"binomial"` - logistic loss (classification),

  - `"poisson"` - Poisson loss for count data

  For any other type, a valid
  [`stats::family()`](https://rdrr.io/r/stats/family.html) object may be
  passed. Note that these will generally be much slower to estimate than
  the built-in options passed as strings. So for example,
  `family = "gaussian"` and `family = gaussian()` will produce the same
  results, but the first will be much faster.character.

- nlambda:

  Integer. Number of lambda values to use in the sequence.

- lambda_max:

  Optional value for the largest `lambda` to use.

- lambda_min:

  Optional value for the smallest `lambda` to use (\> 0).

- lambda_min_ratio:

  If neither `lambda` nor `lambda_min` is specified,
  `lambda_min = lambda_max * lambda_min_ratio`. A very small value will
  lead to the solution `theta = y` (for the Gaussian loss). This
  argument has no effect if there is a user-defined `lambda` sequence.

- error_measure:

  Metric used to calculate cross validation scores. May be `mse`, `mae`,
  or `deviance`.

- nfolds:

  Integer. The number of folds to use. For leave-vth-out cross
  validation, every vth `y` value and its corresponding position (and
  weight) are placed into the same fold. The first and last observations
  are not assigned to any folds. This value must be at least 2. As an
  example, with 15 data points and `nfolds = 4`, the points are assigned
  to folds in the following way: \$\$ 0 \\ 1 \\ 2 \\ 3 \\ 4 \\ 1 \\ 2 \\
  3 \\ 4 \\ 1 \\ 2 \\ 3 \\ 4 \\ 1 \\ 0 \$\$ where 0 indicates no
  assignment. Therefore, the folds are not random and running
  `cv_trendfilter()` twice will give the same result.

## Value

A list of parameter configurations.
