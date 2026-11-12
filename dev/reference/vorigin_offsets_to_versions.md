# Inverse of [`versions_to_vorigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md)

Inverse of
[`versions_to_vorigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md)

## Usage

``` r
vorigin_offsets_to_versions(vorigin_offsets, vorigin)
```

## Arguments

- vorigin_offsets:

  bare numeric vector, as output by
  [`versions_to_vorigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md),
  or derived from such output.

- vorigin:

  the `vorigin` passed to
  [`versions_to_vorigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md)
  used when generating `vorigin_offsets`.

## Value

vec; same ptype as `vorigin`, same size as `vorigin_offsets`.
