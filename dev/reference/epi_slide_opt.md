# Calculate rolling or running means, sums, etc., or custom calculations

These methods take each subpopulation (i.e., a single `geo_value` and
combination of any `other_keys` you set up for age groups, etc.) and
perform a `.window_size`-width time window rolling/sliding computation,
or alternatively, a running/cumulative computation (with
`.window_size = Inf`) on the requested columns. Explicit `NA`
measurements are temporarily added to fill in any time gaps, and, for
rolling computations, to pad the time series to ensure that the first &
last computations use exactly `.window_size` values.

`epi_slide_opt` allows you to use any
[data.table::froll](https://rdrr.io/pkg/data.table/man/froll.html) or
[slider::summary-slide](https://slider.r-lib.org/reference/summary-slide.html)
function. If none of those specialized functions fit your usecase, you
can use
[`data.table::frollapply`](https://rdrr.io/pkg/data.table/man/frollapply.html)
together with a non-rolling function (e.g., `median`). See
[`epi_slide`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
if you need to work with multiple columns at once or output a custom
type.

`epi_slide_mean` is a wrapper around `epi_slide_opt` with
`.f = data.table::frollmean`.

`epi_slide_sum` is a wrapper around `epi_slide_opt` with
`.f = data.table::frollsum`.

## Usage

``` r
epi_slide_opt(
  .x,
  .col_names,
  .f,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)

epi_slide_mean(
  .x,
  .col_names,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)

epi_slide_sum(
  .x,
  .col_names,
  ...,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  .prefix = NULL,
  .suffix = NULL,
  .new_col_names = NULL,
  .ref_time_values = NULL,
  .all_rows = FALSE
)
```

## Arguments

- .x:

  An `epi_df` object. If ungrouped, we temporarily group by `geo_value`
  and any columns in `other_keys`. If grouped, we make sure the grouping
  is by `geo_value` and `other_keys`.

- .col_names:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  An unquoted column name (e.g., `cases`), multiple column names (e.g.,
  `c(cases, deaths)`), [other tidy-select
  expression](https://tidyselect.r-lib.org/reference/language.html), or
  a vector of characters (e.g. `c("cases", "deaths")`). Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

  The tidy-selection renaming interface is not supported, and cannot be
  used to provide output column names; if you want to customize the
  output column names, use
  [`dplyr::rename`](https://dplyr.tidyverse.org/reference/rename.html)
  after the slide.

- .f:

  Function; together with `...` specifies the computation to slide. `.f`
  must be one of `data.table`'s rolling functions (`frollmean`,
  `frollsum`, `frollapply`. See
  [data.table::roll](https://rdrr.io/pkg/data.table/man/froll.html)) or
  one of `slider`'s specialized sliding functions (`slide_mean`,
  `slide_sum`, etc. See
  [slider::summary-slide](https://slider.r-lib.org/reference/summary-slide.html)).

  The optimized `data.table` and `slider` functions can't be directly
  passed as the computation function in `epi_slide` without careful
  handling to make sure each computation group is made up of the
  `.window_size` dates rather than `.window_size` points.
  `epi_slide_opt` (and wrapper functions `epi_slide_mean` and
  `epi_slide_sum`) take care of window completion automatically to
  prevent associated errors.

- ...:

  Additional arguments to pass to the slide computation `.f`, for
  example, `algo` or `na.rm` in data.table functions. You don't need to
  specify `.x`, `.window_size`, or `.align` (or `before`/`after` for
  slider functions).

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

- .prefix:

  Optional
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) format
  string; name the slide result column(s) by attaching this prefix to
  the corresponding input column(s). Some shorthand is supported for
  basing the output names on `.window_size` or other arguments; see
  "Prefix and suffix shorthand" below.

- .suffix:

  Optional
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) format
  string; like `.prefix`. The default naming behavior is equivalent to
  `.suffix = "_{.n}{.time_unit_abbr}{.align_abbr}{.f_abbr}"`. Can be
  used in combination with `.prefix`.

- .new_col_names:

  Optional character vector with length matching the number of input
  columns from `.col_names`; name the slide result column(s) with these
  names. Cannot be used in combination with `.prefix` and/or `.suffix`.

- .ref_time_values:

  The time values at which to compute the slides values. By default,
  this is all the unique time values in `.x`.

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

## Prefix and suffix shorthand

[`glue::glue`](https://glue.tidyverse.org/reference/glue.html) format
strings specially interpret content within curly braces. E.g.,
`glue::glue("ABC{2 + 2}")` evaluates to `"ABC4"`. For `.prefix` and
`.suffix`, we provide `glue` with some additional variable bindings:

- `{.n}` will be the number of time steps in the computation
  corresponding to the `.window_size`.

- `{.time_unit_abbr}` will be a lower-case letter corresponding to the
  `time_type` of `.x`

- `{.align_abbr}` will be `""` if `.align` is the default of `"right"`;
  otherwise, it will be the first letter of `.align`

- `{.f_abbr}` will be a character vector containing a short abbreviation
  for `.f` factoring in the input column type(s) for `.col_names`

## See also

[`epi_slide`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
for the more general slide function

## Examples

``` r
library(dplyr)

# Add a column (`cases_7dsum`) containing a 7-day trailing sum on `cases`:
cases_deaths_subset %>%
  select(geo_value, time_value, cases) %>%
  epi_slide_sum(cases, .window_size = 7)
#> An `epi_df` object, 4,026 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> 
#> # A tibble: 4,026 × 4
#>    geo_value time_value cases cases_7dsum
#>    <chr>     <date>     <dbl>       <dbl>
#>  1 ca        2020-03-01     6          NA
#>  2 ca        2020-03-02     4          NA
#>  3 ca        2020-03-03     6          NA
#>  4 ca        2020-03-04    11          NA
#>  5 ca        2020-03-05    10          NA
#>  6 ca        2020-03-06    18          NA
#>  7 ca        2020-03-07    26          81
#>  8 ca        2020-03-08    19          94
#>  9 ca        2020-03-09    23         113
#> 10 ca        2020-03-10    22         129
#> # ℹ 4,016 more rows

# Add a column (`cases_rate_7dav`) containing a 7-day trailing average on `case_rate`:
covid_case_death_rates_extended %>%
  epi_slide_mean(case_rate, .window_size = 7)
#> An `epi_df` object, 37,576 x 5 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> 
#> # A tibble: 37,576 × 5
#>    geo_value time_value case_rate death_rate case_rate_7dav
#>    <chr>     <date>         <dbl>      <dbl>          <dbl>
#>  1 ak        2020-03-01         0          0             NA
#>  2 ak        2020-03-02         0          0             NA
#>  3 ak        2020-03-03         0          0             NA
#>  4 ak        2020-03-04         0          0             NA
#>  5 ak        2020-03-05         0          0             NA
#>  6 ak        2020-03-06         0          0             NA
#>  7 ak        2020-03-07         0          0              0
#>  8 ak        2020-03-08         0          0              0
#>  9 ak        2020-03-09         0          0              0
#> 10 ak        2020-03-10         0          0              0
#> # ℹ 37,566 more rows

# Use a less common specialized slide function:
cases_deaths_subset %>%
  epi_slide_opt(cases, slider::slide_min, .window_size = 7)
#> An `epi_df` object, 4,026 x 7 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
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
#> # ℹ 1 more variable: cases_7dmin <dbl>

# Specify output column names and/or a naming scheme:
cases_deaths_subset %>%
  select(geo_value, time_value, cases) %>%
  group_by(geo_value) %>%
  epi_slide_sum(cases, .window_size = 7, .new_col_names = "case_sum") %>%
  ungroup()
#> An `epi_df` object, 4,026 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> 
#> # A tibble: 4,026 × 4
#>    geo_value time_value cases case_sum
#>    <chr>     <date>     <dbl>    <dbl>
#>  1 ca        2020-03-01     6       NA
#>  2 ca        2020-03-02     4       NA
#>  3 ca        2020-03-03     6       NA
#>  4 ca        2020-03-04    11       NA
#>  5 ca        2020-03-05    10       NA
#>  6 ca        2020-03-06    18       NA
#>  7 ca        2020-03-07    26       81
#>  8 ca        2020-03-08    19       94
#>  9 ca        2020-03-09    23      113
#> 10 ca        2020-03-10    22      129
#> # ℹ 4,016 more rows
cases_deaths_subset %>%
  select(geo_value, time_value, cases) %>%
  group_by(geo_value) %>%
  epi_slide_sum(cases, .window_size = 7, .prefix = "sum_") %>%
  ungroup()
#> An `epi_df` object, 4,026 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> 
#> # A tibble: 4,026 × 4
#>    geo_value time_value cases sum_cases
#>    <chr>     <date>     <dbl>     <dbl>
#>  1 ca        2020-03-01     6        NA
#>  2 ca        2020-03-02     4        NA
#>  3 ca        2020-03-03     6        NA
#>  4 ca        2020-03-04    11        NA
#>  5 ca        2020-03-05    10        NA
#>  6 ca        2020-03-06    18        NA
#>  7 ca        2020-03-07    26        81
#>  8 ca        2020-03-08    19        94
#>  9 ca        2020-03-09    23       113
#> 10 ca        2020-03-10    22       129
#> # ℹ 4,016 more rows

# Additional settings can be sent to the {data.table} and {slider} functions
# via `...`. This example passes some arguments to `frollmean` settings for
# speed, accuracy, and to allow partially-missing windows:
covid_case_death_rates_extended %>%
  epi_slide_mean(
    case_rate,
    .window_size = 7,
    na.rm = TRUE, algo = "exact", hasNA = TRUE
  )
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> Warning: hasNA is deprecated, use has.nf instead
#> An `epi_df` object, 37,576 x 5 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> 
#> # A tibble: 37,576 × 5
#>    geo_value time_value case_rate death_rate case_rate_7dav
#>    <chr>     <date>         <dbl>      <dbl>          <dbl>
#>  1 ak        2020-03-01         0          0              0
#>  2 ak        2020-03-02         0          0              0
#>  3 ak        2020-03-03         0          0              0
#>  4 ak        2020-03-04         0          0              0
#>  5 ak        2020-03-05         0          0              0
#>  6 ak        2020-03-06         0          0              0
#>  7 ak        2020-03-07         0          0              0
#>  8 ak        2020-03-08         0          0              0
#>  9 ak        2020-03-09         0          0              0
#> 10 ak        2020-03-10         0          0              0
#> # ℹ 37,566 more rows

# If the more specialized possibilities for `.f` don't cover your needs, you
# can use `epi_slide_opt` with `.f = data.table::frollapply` to apply a
# custom function at the cost of more computation time. See also `epi_slide`
# if you need something even more general.
cases_deaths_subset %>%
  select(geo_value, time_value, case_rate_7d_av, death_rate_7d_av) %>%
  epi_slide_opt(c(case_rate_7d_av, death_rate_7d_av),
    data.table::frollapply,
    FUN = median, .window_size = 28,
    .suffix = "_{.n}{.time_unit_abbr}_median"
  ) %>%
  print(n = 40)
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> An `epi_df` object, 4,026 x 6 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> 
#> # A tibble: 4,026 × 6
#>    geo_value time_value case_rate_7d_av death_rate_7d_av case_rate_7d_av_28d_m…¹
#>    <chr>     <date>               <dbl>            <dbl>                   <dbl>
#>  1 ca        2020-03-01         0.00327         0                        NA     
#>  2 ca        2020-03-02         0.00435         0                        NA     
#>  3 ca        2020-03-03         0.00617         0                        NA     
#>  4 ca        2020-03-04         0.00980         0.000363                 NA     
#>  5 ca        2020-03-05         0.0134          0.000363                 NA     
#>  6 ca        2020-03-06         0.0200          0.000363                 NA     
#>  7 ca        2020-03-07         0.0294          0.000363                 NA     
#>  8 ca        2020-03-08         0.0341          0.000363                 NA     
#>  9 ca        2020-03-09         0.0410          0.000726                 NA     
#> 10 ca        2020-03-10         0.0468          0.000726                 NA     
#> 11 ca        2020-03-11         0.0519          0.00109                  NA     
#> 12 ca        2020-03-12         0.0639          0.00145                  NA     
#> 13 ca        2020-03-13         0.0766          0.00109                  NA     
#> 14 ca        2020-03-14         0.0875          0.00145                  NA     
#> 15 ca        2020-03-15         0.0947          0.00181                  NA     
#> 16 ca        2020-03-16         0.144           0.00145                  NA     
#> 17 ca        2020-03-17         0.167           0.00218                  NA     
#> 18 ca        2020-03-18         0.221           0.00435                  NA     
#> 19 ca        2020-03-19         0.275           0.00544                  NA     
#> 20 ca        2020-03-20         0.350           0.00689                  NA     
#> 21 ca        2020-03-21         0.385           0.00762                  NA     
#> 22 ca        2020-03-22         0.480           0.0109                   NA     
#> 23 ca        2020-03-23         0.559           0.0123                   NA     
#> 24 ca        2020-03-24         0.684           0.0156                   NA     
#> 25 ca        2020-03-25         0.806           0.0181                   NA     
#> 26 ca        2020-03-26         1.05            0.0218                   NA     
#> 27 ca        2020-03-27         1.20            0.0279                   NA     
#> 28 ca        2020-03-28         2.22            0.0588                    0.0911
#> 29 ca        2020-03-29         1.38            0.0352                    0.119 
#> 30 ca        2020-03-30         1.74            0.0396                    0.155 
#> 31 ca        2020-03-31         2.00            0.0432                    0.194 
#> 32 ca        2020-04-01         2.27            0.0483                    0.248 
#> 33 ca        2020-04-02         2.50            0.0566                    0.312 
#> 34 ca        2020-04-03         2.74            0.0639                    0.368 
#> 35 ca        2020-04-04         1.93            0.0381                    0.433 
#> 36 ca        2020-04-05         3.26            0.0762                    0.519 
#> 37 ca        2020-04-06         3.31            0.0806                    0.621 
#> 38 ca        2020-04-07         3.30            0.0922                    0.745 
#> 39 ca        2020-04-08         3.38            0.105                     0.928 
#> 40 ca        2020-04-09         3.18            0.110                     1.13  
#> # ℹ 3,986 more rows
#> # ℹ abbreviated name: ¹​case_rate_7d_av_28d_median
#> # ℹ 1 more variable: death_rate_7d_av_28d_median <dbl>
```
