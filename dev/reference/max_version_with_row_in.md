# `max(x$version)`, with error if `x` has 0 rows

Exported to make defaults more easily copyable.

## Usage

``` r
max_version_with_row_in(x)
```

## Arguments

- x:

  `x` argument of
  [`as_epi_archive`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_archive.md)

## Value

`max(x$version)` if it has any rows; raises error if it has 0 rows or an
`NA` version value
