# Closest time_delta that's approximately greater than or equal to given difftime

**\[experimental\]**

## Usage

``` r
difftime_approx_ceiling_time_delta(difftime, time_type)
```

## Arguments

- difftime:

  a difftime object

- time_type:

  as in
  [`validate_slide_window_arg`](https://cmu-delphi.github.io/epiprocess/reference/validate_slide_window_arg.md)

## Value

An object representing an integerish number (or vector of numbers) of
time steps between consecutive time_values of type `time_type`.
