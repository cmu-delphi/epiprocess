# Like `summary` but working across all "time deltas", including difftimes

Also standardizes units of difftimes to the natural unit for the given
`time_type` (via conversion to and from a corresponding number of time
steps).

## Usage

``` r
time_delta_summary(time_delta, time_type)
```
