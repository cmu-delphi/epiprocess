# Make a complete date sequence between min(x\$time_value) and max (x\$time_value). Produce lists of dates before min(x\$time_value) and after max(x\$time_value) for padding initial and final windows to size `n`.

`before` and `after` args are assumed to have been validated by the
calling function (using `validate_slide_window_arg`).

## Usage

``` r
full_date_seq(x, before, after, time_type)
```
