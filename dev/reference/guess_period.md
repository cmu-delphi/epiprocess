# Use max valid period as guess for `period` of `time_values`

**\[experimental\]**

## Usage

``` r
guess_period(
  time_values,
  time_values_arg = rlang::caller_arg(time_values),
  ...
)
```

## Arguments

- time_values:

  Vector containing time-interval-like or time-point-like data, with at
  least two distinct values.

- time_values_arg:

  Optional, string; name to give `time_values` in error messages.
  Defaults to quoting the expression the caller fed into the
  `time_values` argument.

- ...:

  Should be empty, there to satisfy the S3 generic.

## Value

length-1 vector; **\[experimental\]** class will either be the same
class as [`base::diff()`](https://rdrr.io/r/base/diff.html) on such time
values, an integer, or a double, such that all `time_values` can be
exactly obtained by adding `k * result` for an integer k, and such that
there is no smaller `result` that can achieve this.
