# Base S3 methods for an `epi_df` object

The `print` and `summary` methods provide informative displays of an
`epi_df` object. Both show the data's dimensions and core metadata such
as `geo_type`, `time_type`, `other_keys`, and `as_of`.

[`summary()`](https://rdrr.io/r/base/summary.html) provides detailed
statistics about the `epi_df` object, including the time range included,
gap analysis, and per-signal latency. See the `print` method
documentation for a comprehensive description of the output. See
Details: section for what specifically is included.

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

  Additional arguments; unused in
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html); forwarded to
  underlying `{dplyr}` methods in the rest.

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

## Details

### print method

This method also includes a brief latency summary, representing the
difference between the `as_of` date and the most recent observation
found in the data.

### summary method

This method provides a more detailed look at the data's structure:

- The time range section reports the global minimum and maximum time
  values found across all time series. It notes whether every series
  covers this full span or if some start later or end earlier, providing
  insight into staggered data availability across different geographic
  units.

- The gap analysis section identifies missing data patterns by
  distinguishing between missing rows (implicit gaps where time steps
  are skipped) and missing values (explicit NAs within the observed
  range of a series). It also reports the average number of rows per
  time value to help detect potential coverage issues.

- The latency section details the time difference between the most
  recent observation and the `as_of` date of the `epi_df`. This
  breakdown is provided per signal to highlight specific key
  combinations that are lagging or entirely empty.
