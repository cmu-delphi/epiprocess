# Get names of columns that form a (unique) key associated with an object

This is entirely based on metadata and arguments passed; there are no
explicit checks that the key actually is unique in any associated data
structures.

## Usage

``` r
key_colnames(x, ..., exclude = character())

# S3 method for class 'data.frame'
key_colnames(x, ..., geo_keys, other_keys, time_keys, exclude = character())

# S3 method for class 'epi_df'
key_colnames(
  x,
  ...,
  geo_keys = "geo_value",
  other_keys = attr(x, "metadata")$other_keys,
  time_keys = "time_value",
  exclude = character()
)

# S3 method for class 'tbl_ts'
key_colnames(x, ..., exclude = character())

# S3 method for class 'epi_archive'
key_colnames(x, ..., exclude = character())
```

## Arguments

- x:

  an object, often a data frame or something similar. `{epiprocess}`
  includes implementations for
  [`epi_df`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.md)s,
  [`epi_archive`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)s,
  [`tsibble::tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)s,
  and other data frames (including
  [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)s);
  other packages, like `{epipredict}`, can add more.

- ...:

  additional arguments passed on to methods

- exclude:

  an optional character vector of key column names to exclude from the
  result

- geo_keys, other_keys, time_keys:

  character vectors, sometimes optional; which variables (if any) should
  be considered as part of a unique key/identifier for data in `x`,
  dealing respectively with the associated geographical region,
  demographic/strain/other information needed in addition to the
  geographical region to identify individual time series in `x`, and
  time interval during which associated events occurred.

  Mandatory if `x` is a regular `data.frame` or `tibble`. Optional if
  `x` is an `epi_df`; the defaults are `"geo_value"`, the `epi_df`'s
  `other_keys` metadata, and `"time_value"`, respectively; if you
  provide these manually, they must match the defaults. (This behavior
  is to enable consistent and sane results when you can't guarantee
  whether `x` is an `epi_df` or just a `tibble`/`data.frame`. You don't
  need to use it if you know that `x` is definitely an `epi_df`.) Not
  accepted when `x` is a `tsibble` or an `epi_archive`.

## Value

character vector
