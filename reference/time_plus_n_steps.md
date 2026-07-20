# Advance/retreat time_values by specified number of time "steps"

Here, a "step" is based on the `time_type`, not just the class of `x`.

## Usage

``` r
time_plus_n_steps(x, y, time_type)

time_minus_n_steps(x, y, time_type)
```

## Arguments

- x:

  a time_value (vector) of time type `time_type`

- y:

  integerish (vector)

- time_type:

  as in
  [`validate_slide_window_arg()`](https://cmu-delphi.github.io/epiprocess/reference/validate_slide_window_arg.md)

## Value

a time_value (vector) of time type `time_type`
