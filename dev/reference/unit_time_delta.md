# Object that, added to time_values of time_type, advances by one time step/interval

Object that, added to time_values of time_type, advances by one time
step/interval

## Usage

``` r
unit_time_delta(time_type, format = c("friendly", "fast"))
```

## Arguments

- time_type:

  string; `epi_df`'s or `epi_archive`'s `time_type`

- format:

  "friendly" or "fast"; for some time_types, there are multiple ways to
  represent time_deltas. "friendly" tries to output a format that will
  be more informative when printed, and produce errors in more cases
  when used in unexpected ways. "fast" tries to output a time_delta that
  will be faster in downstream operations.

## Value

an object `u` such that `time_values + u` represents advancing by one
time step / moving to the subsequent time interval for any `time_values`
object of time type `time_type`, and such that `time_values + k * u` for
integerish vector `k` advances by `k` steps (with vectorization,
recycling). At time of writing, these objects also all support
multiplication by nonintegerish numeric vectors, `mean`, and `median`,
which are useful for summarizing vector time_deltas, but these
fractional time_deltas are not allowed in time_delta-specific
operations.
