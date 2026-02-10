# Convert a time delta to a integerish number of "unit" steps between time values

Convert a time delta to a integerish number of "unit" steps between time
values

## Usage

``` r
time_delta_to_n_steps(time_delta, time_type)
```

## Arguments

- time_delta:

  a vector that can be added to time values of time type `time_type` to
  arrive at other time values of that time type, or **\[experimental\]**
  such a vector with Inf/-Inf entries mixed in, if supported by the
  class of `time_delta`, even if `time_type` doesn't necessarily support
  Inf/-Inf entries. Basically a slide window arg but without sign and
  length restrictions.

- time_type:

  as in `validate_slide_window_arg`

## Value

[bare integerish](https://rlang.r-lib.org/reference/is_integerish.html)
vector (with possible infinite values) that produces the same result as
`time_delta` when multiplied by the natural
[`unit_time_delta`](https://cmu-delphi.github.io/epiprocess/dev/reference/unit_time_delta.md)
for that time type and added to time values of time type `time_type`. If
the given time type does not support infinite values, then it should
produce +Inf or -Inf for analogous entries of `time_delta`, and match
the addition result match the addition result for non-infinite entries.
