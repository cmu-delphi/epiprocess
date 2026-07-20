# Get the latest snapshot from an `epi_archive` object

**\[deprecated\]**; please use
[`epix_as_of_latest`](https://cmu-delphi.github.io/epiprocess/reference/epix_as_of_latest.md)
instead.

The latest snapshot is an `epi_df` snapshot of the data, as of the last
recorded version in `x`.

## Usage

``` r
epix_as_of_current(x)
```

## Arguments

- x:

  An `epi_archive` object

## Value

The latest snapshot, in `epi_df` format
