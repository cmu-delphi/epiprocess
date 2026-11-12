# Run a specialized slide computation on a single `epi_df` epikey, with temporary completion

Run a specialized slide computation on a single `epi_df` epikey, with
temporary completion

## Usage

``` r
epi_slide_opt_edf_one_epikey(
  inp_tbl,
  in_colnames,
  f_dots_baked,
  f_from_package,
  before_n_steps,
  after_n_steps,
  unit_step,
  time_type,
  out_colnames,
  out_filter_time_range,
  out_filter_time_set,
  inf_slide_time_min
)
```

## Arguments

- inp_tbl:

  tibble; should have a `time_value` column and columns named in
  `in_colnames`; should not contain any columns named in `out_colnames`

- in_colnames:

  chr; names of columns to which to apply `f_dots_baked`

- f_dots_baked:

  supported sliding function from `{data.table}` or `{slider}`,
  potentially with some arguments baked in with
  [`purrr::partial`](https://purrr.tidyverse.org/reference/partial.html)

- f_from_package:

  string; name of package from which `f_dots_baked` (pre-`partial`)
  originates

- before_n_steps:

  integerish `>=0` or `Inf`; number of time steps before each
  `ref_time_value` to include in the sliding window computation; `Inf`
  to include all times beginning with the min `time_value`

- after_n_steps:

  integerish `>=0`; number of time steps after each `ref_time_value` to
  include in the sliding window computation

- time_type:

  as in `new_epi_archive`

- out_colnames:

  chr, same length as `in_colnames`; column names to use for results

- out_filter_time_range, out_filter_time_set:

  `time_value` filter; `time_values` in the output should match the
  result of applying this filter to `inp_tbl$time_value`. Exactly one of
  the two must be provided (non-`NULL`) and the other must be `NULL`.
  `out_filter_time_range`, if provided, should be a length-2 vector/list
  containing the minimum and maximum `time_value` to allow in the
  output. `out_filter_time_set`, if provided, should be a vector of
  `time_values` to intersect with the input `time_value`s.

- inf_slide_time_min:

  `NULL` or a single `time_value`; when `before_n_steps` is `Inf`, the
  time at which the "sliding" (actually, running) windows start. `NULL`
  will use each epikey's min `time_value` rather than the overall; pass
  the minimum `time_value` across all epikeys to treat times before this
  epikey's first `time_value` as (NA-padded) gaps, matching `epi_slide`.

## Value

tibble; like `inp_tbl` with addition of `out_colnames` holding the slide
computation results, with times filtered down as specified. If there is
nothing to slide over (0 rows output), the `out_colnames` columns will
be omitted; when combining with other epikeys' results, e.g., via
[`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html),
they will be filled in with the appropriate types.

## Examples

``` r

library(dplyr)
tbl <- tibble(
  time_value = c(11:12, 15:18) + 0,
  value = c(c(1, 2), c(4, 8, 16, 32))
)

tbl %>%
  epiprocess:::epi_slide_opt_edf_one_epikey(
    "value",
    data.table::frollmean, "data.table",
    1L, 0L, 1L, "integer",
    "slide_value",
    c(11L, 16L), NULL,
    NULL
  )
#> # A tibble: 4 × 3
#>   time_value value slide_value
#>        <int> <dbl>       <dbl>
#> 1         11     1        NA  
#> 2         12     2         1.5
#> 3         15     4        NA  
#> 4         16     8         6  

tbl %>%
  epiprocess:::epi_slide_opt_edf_one_epikey(
    "value",
    data.table::frollmean, "data.table",
    0L, 1L, 1L, "integer",
    "slide_value",
    NULL, c(11, 15, 16, 17, 18),
    NULL
  )
#> # A tibble: 5 × 3
#>   time_value value slide_value
#>        <dbl> <dbl>       <dbl>
#> 1         11     1         1.5
#> 2         15     4         6  
#> 3         16     8        12  
#> 4         17    16        24  
#> 5         18    32        NA  

tbl %>%
  epiprocess:::epi_slide_opt_edf_one_epikey(
    "value",
    data.table::frollmean, "data.table",
    Inf, 0L, 1L, "integer",
    "slide_value",
    NULL, c(12, 17),
    NULL
  )
#> # A tibble: 2 × 3
#>   time_value value slide_value
#>        <dbl> <dbl>       <dbl>
#> 1         12     2         1.5
#> 2         17    16        NA  
```
