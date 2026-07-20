# Drop any `epi_df` metadata and class on a data frame

Useful in implementing
[`?dplyr_extending`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
when manipulations cause invariants of `epi_df`s to be violated and we
need to return some other class. Note that this will maintain any
grouping (keeping the `grouped_df` class and associated attributes, if
present).

## Usage

``` r
decay_epi_df(x)
```

## Arguments

- x:

  an `epi_df` or other data frame

## Value

`x` with any metadata dropped and the `"epi_df"` class, if previously
present, dropped
