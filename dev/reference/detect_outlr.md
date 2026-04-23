# Detect outliers

Applies one or more outlier detection methods to a given signal
variable, and optionally aggregates the outputs to create a consensus
result. See the [outliers
vignette](https://cmu-delphi.github.io/epiprocess/articles/outliers.html)
for examples.

`detect_outlr_rm` detects outliers based on a distance from the rolling
median specified in terms of multiples of the rolling interquartile
range (IQR).

`detect_outlr_stl` detects outliers based on a seasonal-trend
decomposition using LOESS (STL).

## Usage

``` r
detect_outlr(
  x = seq_along(y),
  y,
  methods = tibble::tibble(method = "rm", args = list(list()), abbr = "rm"),
  combiner = c("median", "mean", "none")
)

detect_outlr_rm(
  x = seq_along(y),
  y,
  n = 21,
  log_transform = FALSE,
  detect_negatives = FALSE,
  detection_multiplier = 2,
  min_radius = 0,
  replacement_multiplier = 0
)

detect_outlr_stl(
  x = seq_along(y),
  y,
  n_trend = 21,
  n_seasonal = 21,
  n_threshold = 21,
  seasonal_period,
  seasonal_as_residual = FALSE,
  log_transform = FALSE,
  detect_negatives = FALSE,
  detection_multiplier = 2,
  min_radius = 0,
  replacement_multiplier = 0
)
```

## Arguments

- x:

  Design points corresponding to the signal values `y`. Default is
  `seq_along(y)` (that is, equally-spaced points from 1 to the length of
  `y`).

- y:

  Signal values.

- methods:

  A tibble specifying the method(s) to use for outlier detection, with
  one row per method, and the following columns:

  - `method`: Either "rm" or "stl", or a custom function for outlier
    detection; see details for further explanation.

  - `args`: Named list of arguments that will be passed to the detection
    method.

  - `abbr`: Abbreviation to use in naming output columns with results
    from this method.

- combiner:

  String, one of "median", "mean", or "none", specifying how to combine
  results from different outlier detection methods for the thresholds
  determining whether a particular observation is classified as an
  outlier, as well as a replacement value for any outliers. If "none",
  then no summarized results are calculated. Note that if the number of
  `methods` (number of rows) is odd, then "median" is equivalent to a
  majority vote for purposes of determining whether a given observation
  is an outlier.

- n:

  Number of time steps to use in the rolling window. Default is 21. This
  value is centrally aligned. When `n` is an odd number, the rolling
  window extends from `(n-1)/2` time steps before each design point to
  `(n-1)/2` time steps after. When `n` is even, then the rolling range
  extends from `n/2-1` time steps before to `n/2` time steps after.

- log_transform:

  Should a log transform be applied before running outlier detection?
  Default is `FALSE`. If `TRUE`, and zeros are present, then the log
  transform will be padded by 1.

- detect_negatives:

  Should negative values automatically count as outliers? Default is
  `FALSE`.

- detection_multiplier:

  Value determining how far the outlier detection thresholds are from
  the rolling median, which are calculated as (rolling median) +/-
  (detection multiplier) \* (rolling IQR). Default is 2.

- min_radius:

  Minimum distance between rolling median and threshold, on transformed
  scale. Default is 0.

- replacement_multiplier:

  Value determining how far the replacement values are from the rolling
  median. The replacement is the original value if it is within the
  detection thresholds, or otherwise it is rounded to the nearest
  (rolling median) +/- (replacement multiplier) \* (rolling IQR).
  Default is 0.

- n_trend:

  Number of time steps to use in the rolling window for trend. Default
  is 21.

- n_seasonal:

  Number of time steps to use in the rolling window for seasonality.
  Default is 21. Can also be the string "periodic". See `s.window` in
  [`stats::stl`](https://rdrr.io/r/stats/stl.html).

- n_threshold:

  Number of time steps to use in rolling window for the IQR outlier
  thresholds.

- seasonal_period:

  Integer specifying period of "seasonality". For example, for daily
  data, a period 7 means weekly seasonality. It must be strictly larger
  than 1. Also impacts the size of the low-pass filter window; see
  `l.window` in [`stats::stl`](https://rdrr.io/r/stats/stl.html).

- seasonal_as_residual:

  Boolean specifying whether the seasonal(/weekly) component should be
  treated as part of the residual component instead of as part of the
  predictions. The default, FALSE, treats them as part of the
  predictions, so large seasonal(/weekly) components will not lead to
  flagging points as outliers. `TRUE` may instead consider the extrema
  of large seasonal variations to be outliers; `n_seasonal` and
  `seasonal_period` will still have an impact on the result, though, by
  impacting the estimation of the trend component.

## Value

An tibble with number of rows equal to `length(y)` and columns giving
the outlier detection thresholds (`lower` and `upper`) and replacement
values from each detection method (`replacement`).

## Details

Each outlier detection method, one per row of the passed `methods`
tibble, is a function that must take as its first two arguments `x` and
`y`, and then any number of additional arguments. The function must
return a tibble with the number of rows equal to `length(y)`, and with
columns `lower`, `upper`, and `replacement`, representing lower and
upper bounds for what would be considered an outlier, and a posited
replacement value, respectively.

For convenience, the outlier detection method can be specified (in the
`method` column of `methods`) by a string "rm", shorthand for
`detect_outlr_rm()`, which detects outliers via a rolling median; or by
"stl", shorthand for `detect_outlr_stl()`, which detects outliers via an
STL decomposition.

The STL decomposition is computed using
[`stats::stl()`](https://rdrr.io/r/stats/stl.html). Once computed, the
outlier detection method is analogous to the rolling median method in
`detect_outlr_rm()`, except with the fitted values and residuals from
the STL decomposition taking the place of the rolling median and
residuals to the rolling median, respectively.

The last set of arguments, `log_transform` through
`replacement_multiplier`, are exactly as in `detect_outlr_rm()`.

## Examples

``` r
detection_methods <- dplyr::bind_rows(
  dplyr::tibble(
    method = "rm",
    args = list(list(
      detect_negatives = TRUE,
      detection_multiplier = 2.5
    )),
    abbr = "rm"
  ),
  dplyr::tibble(
    method = "stl",
    args = list(list(
      detect_negatives = TRUE,
      detection_multiplier = 2.5,
      seasonal_period = 7
    )),
    abbr = "stl_seasonal"
  ),
  dplyr::tibble(
    method = "stl",
    args = list(list(
      detect_negatives = TRUE,
      detection_multiplier = 2.5,
      seasonal_period = 7,
      seasonal_as_residual = TRUE
    )),
    abbr = "stl_reseasonal"
  )
)

x <- covid_incidence_outliers %>%
  dplyr::select(geo_value, time_value, cases) %>%
  as_epi_df() %>%
  group_by(geo_value) %>%
  mutate(outlier_info = detect_outlr(
    x = time_value, y = cases,
    methods = detection_methods,
    combiner = "median"
  )) %>%
  unnest(outlier_info)
# Detect outliers based on a rolling median
covid_incidence_outliers %>%
  dplyr::select(geo_value, time_value, cases) %>%
  as_epi_df() %>%
  group_by(geo_value) %>%
  mutate(outlier_info = detect_outlr_rm(
    x = time_value, y = cases
  ))
#> An `epi_df` object, 730 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2021-10-28
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 150 days
#> 
#> # A tibble: 730 × 4
#> # Groups:   geo_value [2]
#>    geo_value time_value cases outlier_info$lower $upper $replacement
#>    <chr>     <date>     <dbl>              <dbl>  <dbl>        <dbl>
#>  1 fl        2020-06-01   667               530   2010           667
#>  2 nj        2020-06-01   486               150.   840.          486
#>  3 fl        2020-06-02   617               582.  1992.          617
#>  4 nj        2020-06-02   658               210.   771.          658
#>  5 fl        2020-06-03  1317               635   1975          1317
#>  6 nj        2020-06-03   541               270    702           541
#>  7 fl        2020-06-04  1419               713   1909          1419
#>  8 nj        2020-06-04   478               174.   790.          478
#>  9 fl        2020-06-05  1305               553   2081          1305
#> 10 nj        2020-06-05   825               118.   838.          825
#> # ℹ 720 more rows
# Detects outliers based on a seasonal-trend decomposition using LOESS
covid_incidence_outliers %>%
  dplyr::select(geo_value, time_value, cases) %>%
  as_epi_df() %>%
  group_by(geo_value) %>%
  mutate(outlier_info = detect_outlr_stl(
    x = time_value, y = cases,
    seasonal_period = 7 # weekly seasonality for daily data
  ))
#> An `epi_df` object, 730 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2021-10-28
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 150 days
#> 
#> # A tibble: 730 × 4
#> # Groups:   geo_value [2]
#>    geo_value time_value cases outlier_info$lower $upper $replacement
#>    <chr>     <date>     <dbl>              <dbl>  <dbl>        <dbl>
#>  1 fl        2020-06-01   667             -1193.  1233.          667
#>  2 nj        2020-06-01   486               281.   762.          486
#>  3 fl        2020-06-02   617              -691.  1890.          617
#>  4 nj        2020-06-02   658               317.   891.          658
#>  5 fl        2020-06-03  1317              -144.  2396.         1317
#>  6 nj        2020-06-03   541               292.   809.          541
#>  7 fl        2020-06-04  1419               260.  2696.         1419
#>  8 nj        2020-06-04   478               315.   792.          478
#>  9 fl        2020-06-05  1305               548.  2950.         1305
#> 10 nj        2020-06-05   825               382.   835.          825
#> # ℹ 720 more rows
```
