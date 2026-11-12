# Inverse of [`time_values_to_torigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md)

Inverse of
[`time_values_to_torigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md)

## Usage

``` r
torigin_offsets_to_time_values(torigin_offsets, torigin, time_type)
```

## Arguments

- torigin_offsets:

  bare integerish numeric vector, as output by
  [`time_values_to_torigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md),
  or derived from such output.

- torigin, time_type:

  the `torigin` and `time_type` passed to
  [`time_values_to_torigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md)
  used when generating `torigin_offsets`.

## Value

vec; same ptype as `torigin`, same size as `torigin_offsets`.
