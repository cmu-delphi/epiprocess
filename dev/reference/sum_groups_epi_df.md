# Aggregate an `epi_df` object

Aggregates an `epi_df` object by the specified group columns, summing
the `value` column, and returning an `epi_df`. If aggregating over
`geo_value`, the resulting `epi_df` will have `geo_value` set to
`"total"`.

## Usage

``` r
sum_groups_epi_df(.x, sum_cols, group_cols = "time_value")
```

## Arguments

- .x:

  an `epi_df`

- sum_cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  An unquoted column name (e.g., `cases`), multiple column names (e.g.,
  `c(cases, deaths)`), [other tidy-select
  expression](https://tidyselect.r-lib.org/reference/language.html), or
  a vector of characters (e.g. `c("cases", "deaths")`). Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

- group_cols:

  character vector of column names to group by. "time_value" is included
  by default.

## Value

an `epi_df` object

## Examples

``` r
# This data has other_keys age_group and edu_qual:
grad_employ_subset
#> An `epi_df` object, 1,445 x 7 with metadata:
#> * geo_type  = custom
#> * time_type = integer
#> * other_keys = age_group, edu_qual
#> * as_of     = 2024-09-18
#> 
#> # A tibble: 1,445 × 7
#>    geo_value           age_group edu_qual time_value num_graduates med_income_2y
#>  * <chr>               <fct>     <fct>         <int>         <dbl>         <dbl>
#>  1 Newfoundland and L… 15 to 34… Career,…       2010           430         48800
#>  2 Newfoundland and L… 35 to 64… Career,…       2010           140         38100
#>  3 Newfoundland and L… 15 to 34… Career,…       2010           630         49500
#>  4 Newfoundland and L… 35 to 64… Career,…       2010           140         48400
#>  5 Newfoundland and L… 15 to 34… Other c…       2010            60         32700
#>  6 Newfoundland and L… 35 to 64… Other c…       2010            40         30500
#>  7 Newfoundland and L… 15 to 34… Undergr…       2010            20         55400
#>  8 Newfoundland and L… 35 to 64… Undergr…       2010            30         70600
#>  9 Newfoundland and L… 15 to 34… Undergr…       2010          1050         63600
#> 10 Newfoundland and L… 35 to 64… Undergr…       2010           130         85700
#> # ℹ 1,435 more rows
#> # ℹ 1 more variable: med_income_5y <dbl>

# Aggregate num_graduates within each geo_value (and time_value):
grad_employ_subset %>%
  sum_groups_epi_df(num_graduates, group_cols = "geo_value")
#> An `epi_df` object, 86 x 3 with metadata:
#> * geo_type  = custom
#> * time_type = integer
#> * as_of     = 2024-09-18
#> 
#> # A tibble: 86 × 3
#>    geo_value        time_value num_graduates
#>    <chr>                 <int>         <dbl>
#>  1 Alberta                2010         19920
#>  2 Alberta                2011         22290
#>  3 Alberta                2012         23710
#>  4 Alberta                2013         25200
#>  5 Alberta                2014         25790
#>  6 Alberta                2015         26590
#>  7 Alberta                2016         26350
#>  8 Alberta                2017         27230
#>  9 British Columbia       2010         27190
#> 10 British Columbia       2011         29410
#> # ℹ 76 more rows
```
