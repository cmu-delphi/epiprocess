# Standardize a `versions`/`.versions` argument into a vector of versions

Standardize a `versions`/`.versions` argument into a vector of versions

## Usage

``` r
versions_standardize(
  versions,
  archive,
  versions_arg = caller_arg(versions),
  call = caller_env()
)
```

## Arguments

- versions:

  Either (a) a vector containing the set of desired versions to
  include, (b) a description of the desired spacing, such as `"week"`,
  `"2 weeks"`, `"month"`, or another string accepted by
  [`seq`](https://rdrr.io/r/base/seq.html)'s /
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html)'s `by` parameter,
  or (c) `NULL`, to include all versions containing updates.

  In case (a), we accept vectors that can be automatically converted to
  match the [ptype](https://vctrs.r-lib.org/reference/vec_ptype.html) of
  versions in the archive; we try both character-to-Date and
  [`vctrs::vec_cast`](https://vctrs.r-lib.org/reference/vec_cast.html)
  conversions. In case (c), we look at the unique `version`s recorded in
  the archive's DT object.

- archive:

  the `epi_archive` to select versions from.

## Value

a vector with the same ptype as `archive$DT$version`
