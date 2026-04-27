# More general form of [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md) for rolling/running computations

Most rolling/running computations can be handled by
[`epi_slide_mean`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md),
[`epi_slide_sum`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md),
or the medium-generality
[`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)
functions, which have been specialized to run more quickly.
`epi_slide()` is a slower but even more general function for
rolling/running computations, and uses a different interface to specify
the computations; you typically only need to consider using
`epi_slide()` if you have a computation that depends on multiple columns
simultaneously, outputs multiple columns simultaneously, or produces
non-numeric output. For example, this computation depends on multiple
columns:

## Usage

``` r
epi_slide(
  .x,
  .f,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .ref_time_values = NULL,
  .new_col_name = NULL,
  .all_rows = FALSE
)
```

## Arguments

- .x:

  An `epi_df` object. If ungrouped, we temporarily group by `geo_value`
  and any columns in `other_keys`. If grouped, we make sure the grouping
  is by `geo_value` and `other_keys`.

- .f, ...:

  The computation to slide. The input will be a time window of the data
  for a single subpopulation (i.e., a single `geo_value` and single
  value for any
  [`other_keys`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_df.md)
  you set up, such as age groups, race, etc.). The input will always
  have the same size, determined by `.window_size`, and will fill in any
  missing `time_values`, using `NA` values for missing measurements. The
  output should be a scalar value or a 1-row data frame; these outputs
  will be collected into a new column or columns in the `epi_slide()`
  result. Data frame outputs will be unpacked into multiple columns in
  the result by default, or
  [`tidyr::pack`](https://tidyr.tidyverse.org/reference/pack.html)ed
  into a single data-frame-type column if you provide a name for such a
  column (e.g., via `.new_col_name`).

  You can specify the computation in one of the following ways:

  - Don't provide `.f`, and instead use one or more
    [`dplyr::summarize`](https://dplyr.tidyverse.org/reference/summarise.html)-esque
    ["data-masking"](https://rlang.r-lib.org/reference/args_data_masking.html)
    expressions in `...`, e.g.,
    `cfr_estimate_v0 = death_rate_7d_av[[22]]/case_rate_7d_av[[1]]`.
    This way is sometimes more convenient, but also has the most
    computational overhead.

  - Provide a formula in `.f`, e.g.,
    `~ .x$death_rate_7d_av[[22]]/.x$case_rate_7d_av[[1]]`. In this
    formula, `.x` is an `epi_df` containing data for a single time
    window as described above, taken from the original `.x` fed into
    `epi_slide()`.

  - Provide a function in `.f`, e.g.,
    `function(x, g, t) x$death_rate_7d_av[[22]]/x$case_rate_7d_av[[1]]`.
    The function should be of the form `function(x, g, t)` or
    `function(x, g, t, <additional configuration arguments>)`, where:

    - `x` is a data frame with the same column names as the original
      object, minus any grouping variables, with only the windowed data
      for one group-`.ref_time_value` combination

    - `g` is a one-row tibble specifying the `geo_value` and value of
      any `other_keys` for this computation

    - `t` is the `.ref_time_value` for the current window

    - If you have a complex `.f` containing
      `<additional configuration arguments>`, you can provide values for
      those arguments in the `...` argument to `epi_slide()`.

    The values of `g` and `t` are also available to data-masking
    expression and formula-based computations as `.group_key` and
    `.ref_time_value`, respectively. Formula computations also let you
    use `.y` or `.z`, respectively, as additional names for these same
    quantities (similar to
    [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)).

- .window_size:

  The size of the sliding window. The accepted values depend on the type
  of the `time_value` column in `.x`:

  - if time type is `Date` and the cadence is daily, then `.window_size`
    can be an integer (which will be interpreted in units of days) or a
    difftime with units "days"

  - if time type is `Date` and the cadence is weekly, then
    `.window_size` must be a `difftime` with units "weeks"

  - if time type is a `yearmonth` or an integer, then `.window_size`
    must be an integer

- .align:

  The alignment of the sliding window.

  - If "right" (default), then the window has its end at the reference
    time. This is likely the most common use case, e.g. `.window_size=7`
    and `.align="right"` slides over the past week of data.

  - If "left", then the window has its start at the reference time.

  - If "center", then the window is centered at the reference time. If
    the window size is odd, then the window will have
    floor(window_size/2) points before and after the reference time; if
    the window size is even, then the window will be asymmetric and have
    one more value before the reference time than after.

- .ref_time_values:

  The time values at which to compute the slides values. By default,
  this is all the unique time values in `.x`.

- .new_col_name:

  Name for the new column that will contain the computed values. The
  default is "slide_value" unless your slide computations output data
  frames, in which case they will be unpacked (as in
  [`tidyr::unpack()`](https://tidyr.tidyverse.org/reference/pack.html))
  into the constituent columns and those names used. New columns should
  not be given names that clash with the existing columns of `.x`.

- .all_rows:

  If `.all_rows = FALSE`, the default, then the output `epi_df` will
  have only the rows that had a `time_value` in `.ref_time_values`.
  Otherwise, all the rows from `.x` are included by with a missing value
  marker (typically NA, but more technically the result of
  [`vctrs::vec_cast`](https://vctrs.r-lib.org/reference/vec_cast.html)-ing
  `NA` to the type of the slide computation output).

## Value

An `epi_df` object with one or more new slide computation columns added.
It will be ungrouped if `.x` was ungrouped, and have the same groups as
`.x` if `.x` was grouped.

## Details

    cases_deaths_subset %>%
      epi_slide(
        cfr_estimate_v0 = death_rate_7d_av[[22]]/case_rate_7d_av[[1]],
        .window_size = 22
      ) %>%
      print(n = 30)

(Here, the value 22 was selected using
[`epi_cor()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_cor.md)
and averaging across `geo_value`s. See [this
manuscript](https://www.medrxiv.org/content/10.1101/2024.12.27.24319518v1)
for some warnings & information using similar types of CFR estimators.)

See
[`vignette("epi_df")`](https://cmu-delphi.github.io/epiprocess/dev/articles/epi_df.md)
for more examples.

### Motivation and lower-level alternatives

`epi_slide()` is focused on preventing errors and providing a convenient
interface. If you need computational speed, many computations can be
optimized by one of the following:

- Performing core sliding operations with
  [`epi_slide_opt()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)
  with `frollapply`, and using potentially-grouped
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)s to
  transform or combine the results.

- Grouping by `geo_value` and any `other_keys`;
  [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)ing
  with
  [`full_seq()`](https://tidyr.tidyverse.org/reference/full_seq.html) to
  fill in time gaps;
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)ing
  by `time_value`s within each group; using
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) with
  vectorized operations and shift operators like
  [`dplyr::lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  and
  [`dplyr::lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  to perform the core operations, being careful to give the desired
  results for the least and most recent `time_value`s (often `NA`s for
  the least recent); ungrouping; and
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)ing
  back down to only rows that existed before the
  [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  stage if necessary.

### Advanced uses of `.f` via tidy evaluation

If specifying `.f` via tidy evaluation, in addition to the standard
[`.data`](https://rlang.r-lib.org/reference/dot-data.html) and
[`.env`](https://rlang.r-lib.org/reference/dot-data.html), we make some
additional "pronoun"-like bindings available:

- .x, which is like `.x` in
  [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html);
  an ordinary object like an `epi_df` rather than an rlang
  [pronoun](https://rlang.r-lib.org/reference/as_data_mask.html) like
  `.data`; this allows you to use additional `dplyr`, `tidyr`, and
  `epiprocess` operations. If you have multiple expressions in `...`,
  this won't let you refer to the output of the earlier expressions, but
  `.data` will.

- .group_key, which is like `.y` in
  [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html).

- .ref_time_value, which is the element of `.ref_time_values` that
  determined the time window for the current computation.

## See also

[`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)
for optimized slide functions

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Generate some simple time-varying CFR estimates:
with_cfr_estimates <- cases_deaths_subset %>%
  epi_slide(
    cfr_estimate_v0 = death_rate_7d_av[[22]] / case_rate_7d_av[[1]],
    .window_size = 22
  )
with_cfr_estimates %>%
  print(n = 30)
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 810 days
#> 
#> # A tibble: 4,026 × 7
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
#> 11 ca        2020-03-11         0.0519          0.00109     25       20.4 
#> 12 ca        2020-03-12         0.0639          0.00145     43       25.1 
#> 13 ca        2020-03-13         0.0766          0.00109     53       30.1 
#> 14 ca        2020-03-14         0.0875          0.00145     56       34.4 
#> 15 ca        2020-03-15         0.0947          0.00181     39       37.3 
#> 16 ca        2020-03-16         0.144           0.00145    159       56.7 
#> 17 ca        2020-03-17         0.167           0.00218     84       65.6 
#> 18 ca        2020-03-18         0.221           0.00435    176       87.1 
#> 19 ca        2020-03-19         0.275           0.00544    190      108.  
#> 20 ca        2020-03-20         0.350           0.00689    261      138.  
#> 21 ca        2020-03-21         0.385           0.00762    152      152.  
#> 22 ca        2020-03-22         0.480           0.0109     301      189   
#> 23 ca        2020-03-23         0.559           0.0123     376      220   
#> 24 ca        2020-03-24         0.684           0.0156     428      269.  
#> 25 ca        2020-03-25         0.806           0.0181     512      317.  
#> 26 ca        2020-03-26         1.05            0.0218     866      414.  
#> 27 ca        2020-03-27         1.20            0.0279     670      472.  
#> 28 ca        2020-03-28         2.22            0.0588    2965      874   
#> 29 ca        2020-03-29         1.38            0.0352   -2019      543.  
#> 30 ca        2020-03-30         1.74            0.0396    1369      684.  
#> # ℹ 3,996 more rows
#> # ℹ 1 more variable: cfr_estimate_v0 <dbl>
# (Here, the value 22 was selected using `epi_cor()` and averaging across
# `geo_value`s. See
# https://www.medrxiv.org/content/10.1101/2024.12.27.24319518v1 for some
# warnings & information using CFR estimators along these lines.)

# In addition to the [`dplyr::mutate`]-like syntax, you can feed in a
# function or formula in a way similar to [`dplyr::group_modify`]; these
# often run much more quickly:
my_computation <- function(window_data) {
  tibble(
    cfr_estimate_v0 = window_data$death_rate_7d_av[[nrow(window_data)]] /
      window_data$case_rate_7d_av[[1]]
  )
}
with_cfr_estimates2 <- cases_deaths_subset %>%
  epi_slide(
    ~ my_computation(.x),
    .window_size = 22
  )
with_cfr_estimates3 <- cases_deaths_subset %>%
  epi_slide(
    function(window_data, g, t) {
      tibble(
        cfr_estimate_v0 = window_data$death_rate_7d_av[[nrow(window_data)]] /
          window_data$case_rate_7d_av[[1]]
      )
    },
    .window_size = 22
  )


#### Advanced: ####

# The tidyverse supports ["packing"][tidyr::pack] multiple columns into a
# single tibble-type column contained within some larger tibble. Like dplyr,
# we normally don't pack output columns together. However, packing behavior can be turned on
# by providing a name for a tibble-type output:
cases_deaths_subset %>%
  epi_slide(
    slide_packed = tibble(
      cases_7sd = sd(.x$cases, na.rm = TRUE),
      cases_7dav = mean(.x$cases, na.rm = TRUE)
    ),
    .window_size = 7
  ) %>%
  select(geo_value, time_value, cases, slide_packed)
#> An `epi_df` object, 4,026 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 810 days
#> 
#> # A tibble: 4,026 × 4
#>    geo_value time_value cases slide_packed$cases_7sd $cases_7dav
#>    <chr>     <date>     <dbl>                  <dbl>       <dbl>
#>  1 ca        2020-03-01     6                  NA           6   
#>  2 ca        2020-03-02     4                   1.41        5   
#>  3 ca        2020-03-03     6                   1.15        5.33
#>  4 ca        2020-03-04    11                   2.99        6.75
#>  5 ca        2020-03-05    10                   2.97        7.4 
#>  6 ca        2020-03-06    18                   5.08        9.17
#>  7 ca        2020-03-07    26                   7.87       11.6 
#>  8 ca        2020-03-08    19                   7.87       13.4 
#>  9 ca        2020-03-09    23                   7.34       16.1 
#> 10 ca        2020-03-10    22                   6.02       18.4 
#> # ℹ 4,016 more rows
cases_deaths_subset %>%
  epi_slide(
    ~ tibble(
      cases_7sd = sd(.x$cases, na.rm = TRUE),
      cases_7dav = mean(.x$cases, na.rm = TRUE)
    ),
    .new_col_name = "slide_packed",
    .window_size = 7
  ) %>%
  select(geo_value, time_value, cases, slide_packed)
#> An `epi_df` object, 4,026 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 810 days
#> 
#> # A tibble: 4,026 × 4
#>    geo_value time_value cases slide_packed$cases_7sd $cases_7dav
#>    <chr>     <date>     <dbl>                  <dbl>       <dbl>
#>  1 ca        2020-03-01     6                  NA           6   
#>  2 ca        2020-03-02     4                   1.41        5   
#>  3 ca        2020-03-03     6                   1.15        5.33
#>  4 ca        2020-03-04    11                   2.99        6.75
#>  5 ca        2020-03-05    10                   2.97        7.4 
#>  6 ca        2020-03-06    18                   5.08        9.17
#>  7 ca        2020-03-07    26                   7.87       11.6 
#>  8 ca        2020-03-08    19                   7.87       13.4 
#>  9 ca        2020-03-09    23                   7.34       16.1 
#> 10 ca        2020-03-10    22                   6.02       18.4 
#> # ℹ 4,016 more rows

# You can also get ["nested"][tidyr::nest] format by wrapping your results in
# a list:
cases_deaths_subset %>%
  group_by(geo_value) %>%
  epi_slide(
    function(x, g, t) {
      list(tibble(
        cases_7sd = sd(x$cases, na.rm = TRUE),
        cases_7dav = mean(x$cases, na.rm = TRUE)
      ))
    },
    .window_size = 7
  ) %>%
  ungroup() %>%
  select(geo_value, time_value, slide_value)
#> An `epi_df` object, 4,026 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * No time series detected
#> # A tibble: 4,026 × 3
#>    geo_value time_value slide_value     
#>    <chr>     <date>     <list>          
#>  1 ca        2020-03-01 <tibble [1 × 2]>
#>  2 ca        2020-03-02 <tibble [1 × 2]>
#>  3 ca        2020-03-03 <tibble [1 × 2]>
#>  4 ca        2020-03-04 <tibble [1 × 2]>
#>  5 ca        2020-03-05 <tibble [1 × 2]>
#>  6 ca        2020-03-06 <tibble [1 × 2]>
#>  7 ca        2020-03-07 <tibble [1 × 2]>
#>  8 ca        2020-03-08 <tibble [1 × 2]>
#>  9 ca        2020-03-09 <tibble [1 × 2]>
#> 10 ca        2020-03-10 <tibble [1 × 2]>
#> # ℹ 4,016 more rows


# Use the geo_value or the ref_time_value in the slide computation
cases_deaths_subset %>%
  epi_slide(~ .x$geo_value[[1]], .window_size = 7)
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 810 days
#> 
#> # A tibble: 4,026 × 7
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
#> # ℹ 1 more variable: slide_value <chr>

cases_deaths_subset %>%
  epi_slide(~ .x$time_value[[1]], .window_size = 7)
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 810 days
#> 
#> # A tibble: 4,026 × 7
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
#> # ℹ 1 more variable: slide_value <date>
```
