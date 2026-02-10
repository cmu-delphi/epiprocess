# Convert from integerish/infinite/mix to time_delta

Convert from integerish/infinite/mix to time_delta

## Usage

``` r
n_steps_to_time_delta(n_steps, time_type, format = c("friendly", "fast"))
```

## Arguments

- n_steps:

  integerish vector that can mix in infinite values

- time_type:

  as in
  [`validate_slide_window_arg`](https://cmu-delphi.github.io/epiprocess/dev/reference/validate_slide_window_arg.md)

- format:

  optional; `"friendly"` to output a more descriptive/friendly class
  like `"difftime"` when possible; `"fast"` to output a class that's
  generally faster to work with when possible, like a vanilla
  `"numeric"`. Default is `"friendly"`.
