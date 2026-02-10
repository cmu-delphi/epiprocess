# Difference between two time value vectors in terms of number of time "steps"

Difference between two time value vectors in terms of number of time
"steps"

## Usage

``` r
time_minus_time_in_n_steps(x, y, time_type)
```

## Arguments

- x:

  a time_value (vector) of time type `time_type`

- y:

  a time_value (vector) of time type `time_type`

- time_type:

  as in
  [`validate_slide_window_arg()`](https://cmu-delphi.github.io/epiprocess/dev/reference/validate_slide_window_arg.md)

## Value

integerish vector such that `x + n_steps_to_time_delta_fast(result)`
should equal `y`.
