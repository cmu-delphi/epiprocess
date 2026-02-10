# Convert `time_delta` to an approximate difftime

**\[experimental\]**

## Usage

``` r
time_delta_to_approx_difftime(time_delta, time_type)
```

## Details

To assist in comparing `time_delta`s to default `difftime` thresholds
when we want to reduce friction.

It may be better to try to do something like make `time_delta`
validation more accommodating (e.g., of difftimes with units of "days"
when working on weekly scale), and remain rigid on yearmonths. Applying
deltas and comparing time_values might also be an approach but seems
more fraught as the least common denominator would be start/mid/end
datetimes of time intervals, but those are also ambiguous
(starting&representation wdays of weeks are unknown, timezone of dates
are unknown).

Another alternative approach, below, converts difftimes to time_deltas
instead. It requires knowledge of which way to round in order to get
time_deltas representing an integer number of time steps, but avoids
some potential inconsistencies of the time-delta-to-difftime approach
when we think about applying it to, e.g., months / spans of months with
varying numbers of days, and also makes it easier to avoid "magical
defaults".
