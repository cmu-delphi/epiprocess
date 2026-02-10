# Format a length-1 time delta to a character to assist messaging

This is meant to address the following:

- `glue::glue("{as.difftime(1, units = 'days')}")` is "1"

- `glue::glue("{format(as.difftime(1, units = 'days'))}")` is "1 days"

- time deltas for yearmonths and integers don't have units attached at
  all

## Usage

``` r
format_time_delta(x, time_type)
```
