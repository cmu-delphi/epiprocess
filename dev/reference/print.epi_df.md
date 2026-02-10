# Base S3 methods for an `epi_df` object

Print and summary functions for an `epi_df` object.

Prints a variety of summary statistics about the `epi_df` object, such
as the time range included and geographic coverage.

## Usage

``` r
# S3 method for class 'epi_df'
print(x, ...)

# S3 method for class 'epi_df'
summary(object, ...)

# S3 method for class 'epi_df'
group_by(.data, ...)

# S3 method for class 'epi_df'
ungroup(x, ...)

# S3 method for class 'epi_df'
group_modify(.data, .f, ..., .keep = FALSE)

# S3 method for class 'epi_df'
unnest(data, ...)
```

## Arguments

- x:

  an `epi_df`

- ...:

  Additional arguments, for compatibility with
  [`summary()`](https://rdrr.io/r/base/summary.html). Currently unused.

- object:

  an `epi_df`

- .data:

  an `epi_df`

- .f:

  function or formula; see
  [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)

- .keep:

  Boolean; see
  [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)

- data:

  an `epi_df`
