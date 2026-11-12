# Express versions as numeric offsets from an origin version

Fast conversion of versions to bare numbers; reversed by
[`vorigin_offsets_to_versions()`](https://cmu-delphi.github.io/epiprocess/dev/reference/vorigin_offsets_to_versions.md).

## Usage

``` r
versions_to_vorigin_offsets(versions, vorigin)
```

## Arguments

- versions:

  vector of versions, `clobberable_versions_start`, or `versions_end`;
  may contain `NA`s.

- vorigin:

  length-1, non-`NA` vector with the same ptype as `versions`.

## Value

bare numeric vector, same length as `versions`.

## Details

This just works with the underlying numeric data of `versions`, which
should preserve the value of inequalities between versions, but other
than that, basically all bets are off: no necessary relationship with
torigin_offsets or differences between them, nor anything about original
version-version deltas (for latter,
[`tsibble::yearmonth`](https://tsibble.tidyverts.org/reference/year-month.html)
backing and POSIXct are challenges).
