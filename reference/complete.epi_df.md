# "Complete" an `epi_df`, adding missing rows and/or replacing `NA`s

A
[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
analogue for `epi_df` objects. This function can be used, for example,
to add rows for missing combinations of `geo_value` and `time_value`,
filling other columns with `NA`s. See the examples for usage details.

## Usage

``` r
# S3 method for class 'epi_df'
complete(data, ..., fill = list(), explicit = TRUE)
```

## Arguments

- data:

  an `epi_df`

- ...:

  see
  [`tidyr::complete`](https://tidyr.tidyverse.org/reference/complete.html)

- fill:

  see
  [`tidyr::complete`](https://tidyr.tidyverse.org/reference/complete.html)

- explicit:

  see
  [`tidyr::complete`](https://tidyr.tidyverse.org/reference/complete.html)

## Examples

``` r
start_date <- as.Date("2020-01-01")
daily_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 3, 3,
  2, start_date + 2, 2,
  2, start_date + 3, 3,
) %>%
  as_epi_df(as_of = start_date + 3)
# Complete without grouping puts all the geo_values on the same min and max
# time_value index
daily_edf %>%
  complete(geo_value, time_value = full_seq(time_value, period = 1))
#> An `epi_df` object, 6 x 3 with metadata:
#> * geo_type  = hhs
#> * time_type = day
#> * as_of     = 2020-01-04
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 0 days
#> 
#> # A tibble: 6 × 3
#>   geo_value time_value value
#>       <dbl> <date>     <dbl>
#> 1         1 2020-01-02     1
#> 2         1 2020-01-03    NA
#> 3         1 2020-01-04     3
#> 4         2 2020-01-02    NA
#> 5         2 2020-01-03     2
#> 6         2 2020-01-04     3
# Complete with grouping puts all the geo_values on individual min and max
# time_value indices
daily_edf %>%
  group_by(geo_value) %>%
  complete(time_value = full_seq(time_value, period = 1))
#> An `epi_df` object, 5 x 3 with metadata:
#> * geo_type  = hhs
#> * time_type = day
#> * as_of     = 2020-01-04
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 0 days
#> 
#> # A tibble: 5 × 3
#> # Groups:   geo_value [2]
#>   geo_value time_value value
#>       <dbl> <date>     <dbl>
#> 1         1 2020-01-02     1
#> 2         1 2020-01-03    NA
#> 3         1 2020-01-04     3
#> 4         2 2020-01-03     2
#> 5         2 2020-01-04     3
# Complete has explicit=TRUE by default, but if it's FALSE, then complete
# only fills the implicit gaps, not those that are explicitly NA
daily_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 2, NA,
  1, start_date + 3, 3,
  2, start_date + 2, 2,
  2, start_date + 3, 3,
) %>%
  as_epi_df(as_of = start_date + 3)
daily_edf %>%
  complete(
    geo_value,
    time_value = full_seq(time_value, period = 1),
    fill = list(value = 0),
    explicit = FALSE
  )
#> An `epi_df` object, 6 x 3 with metadata:
#> * geo_type  = hhs
#> * time_type = day
#> * as_of     = 2020-01-04
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 0 days
#> 
#> # A tibble: 6 × 3
#>   geo_value time_value value
#>       <dbl> <date>     <dbl>
#> 1         1 2020-01-02     1
#> 2         1 2020-01-03    NA
#> 3         1 2020-01-04     3
#> 4         2 2020-01-02     0
#> 5         2 2020-01-03     2
#> 6         2 2020-01-04     3
# Complete works for weekly data and can take a fill value
# No grouping
weekly_edf <- tibble::tribble(
  ~geo_value, ~time_value, ~value,
  1, start_date + 1, 1,
  1, start_date + 15, 3,
  2, start_date + 8, 2,
  2, start_date + 15, 3,
) %>%
  as_epi_df(as_of = start_date + 3)
weekly_edf %>%
  complete(
    geo_value,
    time_value = full_seq(time_value, period = 7),
    fill = list(value = 0)
  )
#> An `epi_df` object, 6 x 3 with metadata:
#> * geo_type  = hhs
#> * time_type = week
#> * as_of     = 2020-01-04
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = -1 weeks
#> 
#> # A tibble: 6 × 3
#>   geo_value time_value value
#>       <dbl> <date>     <dbl>
#> 1         1 2020-01-02     1
#> 2         1 2020-01-09     0
#> 3         1 2020-01-16     3
#> 4         2 2020-01-02     0
#> 5         2 2020-01-09     2
#> 6         2 2020-01-16     3
# With grouping
weekly_edf %>%
  group_by(geo_value) %>%
  complete(
    time_value = full_seq(time_value, period = 7),
    fill = list(value = 0)
  )
#> An `epi_df` object, 5 x 3 with metadata:
#> * geo_type  = hhs
#> * time_type = week
#> * as_of     = 2020-01-04
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = -1 weeks
#> 
#> # A tibble: 5 × 3
#> # Groups:   geo_value [2]
#>   geo_value time_value value
#>       <dbl> <date>     <dbl>
#> 1         1 2020-01-02     1
#> 2         1 2020-01-09     0
#> 3         1 2020-01-16     3
#> 4         2 2020-01-09     2
#> 5         2 2020-01-16     3
```
