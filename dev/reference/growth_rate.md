# Estimate growth rate

Estimates the growth rate of a signal at given points along the
underlying sequence. Several methodologies are available; see the
[growth rate
vignette](https://cmu-delphi.github.io/epiprocess/articles/growth_rate.html)
for examples.

## Usage

``` r
growth_rate(
  y,
  x = seq_along(y),
  x0 = x,
  method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
  h = 7,
  log_scale = FALSE,
  na_rm = FALSE,
  params = growth_rate_params()
)
```

## Arguments

- y:

  Signal values.

- x:

  Design points corresponding to the signal values `y`. Default is
  `seq_along(y)` (that is, equally-spaced points from 1 to the length of
  `y`).

- x0:

  Points at which we should estimate the growth rate. Must be a
  contained in the range of `x` (no extrapolation allowed). Default is
  `x`.

- method:

  Either "rel_change", "linear_reg", "smooth_spline", or "trend_filter",
  indicating the method to use for the growth rate calculation. The
  first two are local methods: they are run in a sliding fashion over
  the sequence (in order to estimate derivatives and hence growth
  rates); the latter two are global methods: they are run once over the
  entire sequence. See details for more explanation.

- h:

  Bandwidth for the sliding window, when `method` is "rel_change" or
  "linear_reg". See details for more explanation.

- log_scale:

  Should growth rates be estimated using the parametrization on the log
  scale? See details for an explanation. Default is `FALSE`.

- na_rm:

  Should missing values be removed before the computation? Default is
  `FALSE`.

- params:

  Additional arguments to pass to the method used to estimate the
  derivative. This should be created with
  [`growth_rate_params()`](https://cmu-delphi.github.io/epiprocess/dev/reference/growth_rate_params.md).

## Value

Vector of growth rate estimates at the specified points `x0`.

## Details

The growth rate of a function f defined over a continuously-valued
parameter t is defined as f'(t) / f(t), where f'(t) is the derivative of
f at t. To estimate the growth rate of a signal in discrete-time (which
can be thought of as evaluations or discretizations of an underlying
function in continuous-time), we can therefore estimate the derivative
and divide by the signal value itself (or possibly a smoothed version of
the signal value).

The following methods are available for estimating the growth rate:

- "rel_change": uses (B/A - 1) / h, where B is the average of `y` over
  the second half of a sliding window of bandwidth h centered at the
  reference point `x0`, and A the average over the first half. This can
  be seen as using a first-difference approximation to the derivative.

- "linear_reg": uses the slope from a linear regression of `y` on `x`
  over a sliding window centered at the reference point `x0`, divided by
  the fitted value from this linear regression at `x0`.

- "smooth_spline": uses the estimated derivative at `x0` from a
  smoothing spline fit to `x` and `y`, via
  [`stats::smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html),
  divided by the fitted value of the spline at `x0`.

- "trend_filter": uses the estimated derivative at `x0` from polynomial
  trend filtering (a discrete spline) fit to `x` and `y`, via
  [`trendfilter::trendfilter()`](https://rdrr.io/pkg/trendfilter/man/trendfilter.html),
  divided by the fitted value of the discrete spline at `x0`. This
  method requires the [`{trendfilter}`
  package](https://github.com/glmgen/trendfilter) to be installed.

### Log Scale

An alternative view for the growth rate of a function f in general is
given by defining g(t) = log(f(t)), and then observing that g'(t) =
f'(t) / f(t). Therefore, any method that estimates the derivative can be
simply applied to the log of the signal of interest, and in this light,
each method above ("rel_change", "linear_reg", "smooth_spline", and
"trend_filter") has a log scale analog, which can be used by setting
`log_scale = TRUE`.

### Sliding Windows

For the local methods, "rel_change" and "linear_reg", we use a sliding
window centered at the reference point of bandiwidth `h`. In other
words, the sliding window consists of all points in `x` whose distance
to the reference point is at most `h`. Note that the unit for this
distance is implicitly defined by the `x` variable; for example, if `x`
is a vector of `Date` objects, `h = 7`, and the reference point is
January 7, then the sliding window contains all data in between January
1 and 14 (matching the behavior of
[`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
with `before = h - 1` and `after = h`).

### Additional Arguments

For the global methods, "smooth_spline" and "trend_filter", additional
arguments can be specified via `params` for the underlying estimation
function. These additional arguments are passed to
[`stats::smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html),
[`trendfilter::trendfilter()`](https://rdrr.io/pkg/trendfilter/man/trendfilter.html),
or
[`trendfilter::cv_trendfilter()`](https://rdrr.io/pkg/trendfilter/man/cv_trendfilter.html).
The defaults are exactly as specified in those functions, except when
those defaults conflict among these functions. These cases are as
follows:

- `df`: desired effective degrees of freedom. For "smooth_spline", this
  must be numeric (or `NULL`) and will be passed along to the underlying
  function. For "trend_filter", if `cv = FALSE`, then `df` must be a
  positive number (integer is most sensible); if `cv = TRUE`, then `df`
  must be one of "min" or "1se" indicating the selection rule to use
  based on the cross-validation error curve: minimum or 1-standard-error
  rule, respectively. The default is "min" (going along with the default
  `cv = TRUE`).

- `lambda`: For "smooth_spline", this should be a scalar value or
  `NULL`. For "trend_filter", this is allowed to also be a vector, as
  long as either `cv = TRUE` or `df` is specified.

- `cv`: should cross-validation be used to choose an effective degrees
  of freedom for the fit? The default is `FALSE` to match
  [`stats::smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html).
  In that case, as in that function, GCV is used instead. For
  "trend_filter", this will be coerced to `TRUE` if neither `df` nor
  `lambda` are specified (the default). Note that passing both `df` and
  a scalar `lambda` will always be an error.

## Examples

``` r
# COVID cases growth rate by state using default method relative change
cases_deaths_subset %>%
  group_by(geo_value) %>%
  mutate(cases_gr = growth_rate(x = time_value, y = cases))
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag across all time series = 810–811 days
#> 
#> # A tibble: 4,026 × 7
#> # Groups:   geo_value [6]
#>    geo_value time_value case_rate_7d_av death_rate_7d_av cases cases_7d_av
#>    <chr>     <date>               <dbl>            <dbl> <dbl>       <dbl>
#>  1 ca        2020-03-01         0.00327         0            6        1.29
#>  2 ca        2020-03-02         0.00435         0            4        1.71
#>  3 ca        2020-03-03         0.00617         0            6        2.43
#>  4 ca        2020-03-04         0.00980         0.000363    11        3.86
#>  5 ca        2020-03-05         0.0134          0.000363    10        5.29
#>  6 ca        2020-03-06         0.0200          0.000363    18        7.86
#>  7 ca        2020-03-07         0.0294          0.000363    26       11.6 
#>  8 ca        2020-03-08         0.0341          0.000363    19       13.4 
#>  9 ca        2020-03-09         0.0410          0.000726    23       16.1 
#> 10 ca        2020-03-10         0.0468          0.000726    22       18.4 
#> # ℹ 4,016 more rows
#> # ℹ 1 more variable: cases_gr <dbl>

# Degree 3 polynomial and 5-fold cross validation on the log scale
# some locations report 0 cases, so we replace these with 1
cases_deaths_subset %>%
  group_by(geo_value) %>%
  mutate(gr_poly = growth_rate(
    x = time_value, y = pmax(cases, 1), method = "trend_filter",
    log_scale = TRUE, na_rm = TRUE
  ))
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag across all time series = 810 days
#> 
#> # A tibble: 4,026 × 7
#> # Groups:   geo_value [6]
#>    geo_value time_value case_rate_7d_av death_rate_7d_av cases cases_7d_av
#>    <chr>     <date>               <dbl>            <dbl> <dbl>       <dbl>
#>  1 ca        2020-03-01         0.00327         0            6        1.29
#>  2 ca        2020-03-02         0.00435         0            4        1.71
#>  3 ca        2020-03-03         0.00617         0            6        2.43
#>  4 ca        2020-03-04         0.00980         0.000363    11        3.86
#>  5 ca        2020-03-05         0.0134          0.000363    10        5.29
#>  6 ca        2020-03-06         0.0200          0.000363    18        7.86
#>  7 ca        2020-03-07         0.0294          0.000363    26       11.6 
#>  8 ca        2020-03-08         0.0341          0.000363    19       13.4 
#>  9 ca        2020-03-09         0.0410          0.000726    23       16.1 
#> 10 ca        2020-03-10         0.0468          0.000726    22       18.4 
#> # ℹ 4,016 more rows
#> # ℹ 1 more variable: gr_poly <dbl>
```
