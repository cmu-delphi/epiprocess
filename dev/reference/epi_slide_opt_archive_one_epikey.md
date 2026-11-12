# Core operation of `epi_slide_opt.epi_archive` for a single epikey's history

Core operation of `epi_slide_opt.epi_archive` for a single epikey's
history

## Usage

``` r
epi_slide_opt_archive_one_epikey(
  inp_updates,
  in_colnames,
  f_dots_baked,
  f_from_package,
  before_n_steps,
  after_n_steps,
  time_type,
  out_colnames,
  inf_slide_time_min_changes
)
```

## Arguments

- inp_updates:

  tibble with a `version` column and measurement columns for a single
  epikey, without the epikey labeling columns (e.g., from
  `group_modify`). Interpretation is analogous to an `epi_archive` `DT`,
  but a specific row order is not required.

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

- inf_slide_time_min_changes:

  `NULL` (required if `before_n_steps` is finite) or a tibble with
  columns `version` and `time_min`, sorted by `version`, recording each
  version at which the minimum `time_value` across all epikeys in the
  archive snapshot decreased (including the first version), and the new
  minimum. When `before_n_steps` is `Inf`, this is used to start running
  windows at the archive-wide minimum `time_value` as of each version,
  matching `epi_slide` on `epix_as_of` snapshots. `NULL` with `Inf`
  `before_n_steps` will instead start at this epikey's own minimum.

## Value

tibble with a `version` column, pre-existing measurement columns, and
new measurement columns; (compactified) diff data to put into an
`epi_archive`. May not match column ordering; may not ensure any row
ordering.

## Examples

``` r

library(dplyr)
inp_updates <- bind_rows(
  tibble(version = 30, time_value = 1:20, value = 1:20),
  tibble(version = 32, time_value = 4:5, value = 5:4),
  tibble(version = 33, time_value = 8, value = 9),
  tibble(version = 34, time_value = 11, value = NA),
  tibble(version = 35, time_value = -10, value = -10),
  tibble(version = 56, time_value = 50, value = 50)
) %>%
  mutate(across(c(version, time_value), ~ as.Date("2020-01-01") - 1 + .x))

f <- purrr::partial(data.table::frollmean, algo = "exact")

inp_updates %>%
  epiprocess:::epi_slide_opt_archive_one_epikey(
    "value", f, "data.table", 2L, 0L, "day", "slide_value",
    NULL
  )
#> # A tibble: 31 × 4
#>    time_value value slide_value version   
#>    <date>     <dbl>       <dbl> <date>    
#>  1 2019-12-21   -10       NA    2020-02-04
#>  2 2020-01-01     1       NA    2020-01-30
#>  3 2020-01-02     2       NA    2020-01-30
#>  4 2020-01-03     3        2    2020-01-30
#>  5 2020-01-04     4        3    2020-01-30
#>  6 2020-01-04     5        3.33 2020-02-01
#>  7 2020-01-05     5        4    2020-01-30
#>  8 2020-01-05     4        4    2020-02-01
#>  9 2020-01-06     6        5    2020-01-30
#> 10 2020-01-07     7        6    2020-01-30
#> # ℹ 21 more rows
```
