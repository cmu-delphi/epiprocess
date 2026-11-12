# Express time values as numbers of time steps from an origin time value

Fast, reversible conversion of `time_value`s to bare integerish;
reversed by
[`torigin_offsets_to_time_values()`](https://cmu-delphi.github.io/epiprocess/dev/reference/torigin_offsets_to_time_values.md).
Preserves time-time spacing in terms of nsteps.

## Usage

``` r
time_values_to_torigin_offsets(time_values, torigin, time_type)
```

## Arguments

- time_values:

  vector of time values of time type `time_type`.

- torigin:

  either (a) a single non-`NA` time value of time type `time_type`,
  or (b) a length-0 time value vector, in which case `time_values` must
  also be length 0.

- time_type:

  string.

## Value

bare integerish vector, with length matching that of `time_values`.

## Details

Predefined time origin based on time_value ptype doesn't work (unknown
representation wday for weekly), so this requires us to provide one. If
even we don't know due to 0 rows in data structure, we can pass a
length-0 `torigin` of the appropriate ptype, assuming that we'll
continue to work with 0-row data structures.
