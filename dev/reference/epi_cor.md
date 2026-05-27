# Compute correlations between variables in an `epi_df` object

Computes correlations between variables in an `epi_df` object, allowing
for grouping by geo value, time value, or any other variables. See the
[correlation
vignette](https://cmu-delphi.github.io/epiprocess/articles/correlation.html)
for examples.

## Usage

``` r
epi_cor(
  x,
  var1,
  var2,
  dt1 = 0,
  dt2 = 0,
  shift_by = geo_value,
  cor_by = geo_value,
  use = "na.or.complete",
  method = c("pearson", "kendall", "spearman")
)
```

## Arguments

- x:

  The `epi_df` object under consideration.

- var1, var2:

  The variables in `x` to correlate.

- dt1, dt2:

  Time shifts to consider for the two variables, respectively, before
  computing correlations. Negative shifts translate into in a lag value
  and positive shifts into a lead value; for example, if `dt = -1`, then
  the new value on June 2 is the original value on June 1; if `dt = 1`,
  then the new value on June 2 is the original value on June 3; if
  `dt = 0`, then the values are left as is. Default is 0 for both `dt1`
  and `dt2`.

- shift_by:

  The variables(s) to group by, for the time shifts. The default is
  `geo_value`. However, we could also use, for example,
  `shift_by = c(geo_value, age_group)`, assuming `x` has a column
  `age_group`, to perform time shifts per geo value and age group. To
  omit a grouping entirely, use `cor_by = NULL`. Note that the grouping
  here is always undone *before* the correlation computations.

- cor_by:

  The variable(s) to group by, for the correlation computations. If
  `geo_value`, the default, then correlations are computed for each geo
  value, over all time; if `time_value`, then correlations are computed
  for each time, over all geo values. A grouping can also be any
  specified using number of columns of `x`; for example, we can use
  `cor_by = c(geo_value, age_group)`, assuming `x` has a column
  `age_group`, in order to compute correlations for each pair of geo
  value and age group. To omit a grouping entirely, use `cor_by = NULL`.
  Note that the grouping here is always done *after* the time shifts.

- use, method:

  Arguments to pass to [`cor()`](https://rdrr.io/r/stats/cor.html), with
  "na.or.complete" the default for `use` (different than
  [`cor()`](https://rdrr.io/r/stats/cor.html)) and "pearson" the default
  for `method` (same as [`cor()`](https://rdrr.io/r/stats/cor.html)).

## Value

An tibble with the grouping columns first (`geo_value`, `time_value`, or
possibly others), and then a column `cor`, which gives the correlation.

## Examples

``` r

# linear association of case and death rates on any given day
epi_cor(
  x = cases_deaths_subset,
  var1 = case_rate_7d_av,
  var2 = death_rate_7d_av,
  cor_by = "time_value"
)
#> Warning: There were 3 warnings in `dplyr::summarize()`.
#> The first warning was:
#> ℹ In argument: `cor = cor(x = .data$var1, y = .data$var2, use = use, method =
#>   method)`.
#> ℹ In group 1: `time_value = 2020-03-01`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
#> # A tibble: 671 × 2
#>    time_value    cor
#>    <date>      <dbl>
#>  1 2020-03-01 NA    
#>  2 2020-03-02 NA    
#>  3 2020-03-03 NA    
#>  4 2020-03-04  0.746
#>  5 2020-03-05  0.549
#>  6 2020-03-06  0.692
#>  7 2020-03-07  0.277
#>  8 2020-03-08 -0.226
#>  9 2020-03-09 -0.195
#> 10 2020-03-10 -0.227
#> # ℹ 661 more rows

# correlation of death rates and lagged case rates
epi_cor(
  x = cases_deaths_subset,
  var1 = case_rate_7d_av,
  var2 = death_rate_7d_av,
  cor_by = time_value,
  dt1 = -2
)
#> Warning: There was 1 warning in `dplyr::summarize()`.
#> ℹ In argument: `cor = cor(x = .data$var1, y = .data$var2, use = use, method =
#>   method)`.
#> ℹ In group 3: `time_value = 2020-03-03`.
#> Caused by warning in `cor()`:
#> ! the standard deviation is zero
#> # A tibble: 671 × 2
#>    time_value    cor
#>    <date>      <dbl>
#>  1 2020-03-01 NA    
#>  2 2020-03-02 NA    
#>  3 2020-03-03 NA    
#>  4 2020-03-04  0.989
#>  5 2020-03-05  0.907
#>  6 2020-03-06  0.746
#>  7 2020-03-07  0.549
#>  8 2020-03-08 -0.158
#>  9 2020-03-09 -0.126
#> 10 2020-03-10 -0.163
#> # ℹ 661 more rows

# correlation grouped by location
epi_cor(
  x = cases_deaths_subset,
  var1 = case_rate_7d_av,
  var2 = death_rate_7d_av,
  cor_by = geo_value
)
#> # A tibble: 6 × 2
#>   geo_value   cor
#>   <chr>     <dbl>
#> 1 ca        0.573
#> 2 fl        0.488
#> 3 ga        0.465
#> 4 ny        0.285
#> 5 pa        0.708
#> 6 tx        0.750

# correlation grouped by location and incorporates lagged cases rates
epi_cor(
  x = cases_deaths_subset,
  var1 = case_rate_7d_av,
  var2 = death_rate_7d_av,
  cor_by = geo_value,
  dt1 = -2
)
#> # A tibble: 6 × 2
#>   geo_value   cor
#>   <chr>     <dbl>
#> 1 ca        0.618
#> 2 fl        0.576
#> 3 ga        0.525
#> 4 ny        0.337
#> 5 pa        0.734
#> 6 tx        0.784
```
