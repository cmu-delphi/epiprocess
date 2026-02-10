# Convert to tsibble format

Converts an `epi_df` object into a tsibble, where the index is taken to
be `time_value`, and the key variables taken to be `geo_value` along
with any others in the `other_keys` field of the metadata, or else
explicitly set.

## Usage

``` r
# S3 method for class 'epi_df'
as_tsibble(x, key, ...)
```

## Arguments

- x:

  an `epi_df`

- key:

  Optional. Any additional keys (other than `geo_value`) to add to the
  `tsibble`.

- ...:

  additional arguments passed on to
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html)
