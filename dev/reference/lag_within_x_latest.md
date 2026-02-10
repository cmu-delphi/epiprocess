# pull the value from lags when values starts indefinitely being within prop of its latest value.

pull the value from lags when values starts indefinitely being within
prop of its latest value.

## Usage

``` r
lag_within_x_latest(lags, values, prop = 0.2)
```

## Arguments

- lags:

  vector of lags; should be sorted

- values:

  this should be a vector (e.g., a column) with length matching that of
  `lags`

- prop:

  optional length-1 double; proportion
