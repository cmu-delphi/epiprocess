# Set the `versions_end` attribute of an `epi_archive` object

An escape hatch for epix_as_of, which does not allow version \>
`$versions_end`.

## Usage

``` r
set_versions_end(x, versions_end)
```

## Arguments

- x:

  An `epi_archive` object

- versions_end:

  The new `versions_end` value

## Value

An `epi_archive` object with the updated `versions_end` attribute
