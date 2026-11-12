# `epi_df` with its `time_value` column and `time_type` replaced

Keeps all other attributes, including grouping, which is assumed not to
involve `time_value`.

## Usage

``` r
edf_with_time_values(x, time_values, time_type)
```

## Arguments

- x:

  an `epi_df`; if grouped, must not include `time_value` in grouping
  (unchecked)

- time_values:

  vector with as many entries as `x` has rows, to become the result's
  `time_value` column.

- time_type:

  string; the result's `time_type`.

## Value

an `epi_df`.
