# Checks to see if a value in a vector is LOCF

LOCF meaning last observation carried forward (to later versions). Lags
the vector by 1, then compares with itself. If `is_key` is `TRUE`, only
values that are exactly the same between the lagged and original are
considered LOCF. If `is_key` is `FALSE` and `vec` is a vector of numbers
([`base::is.numeric`](https://rdrr.io/r/base/numeric.html)), then
approximate equality will be used, checking whether the absolute
difference between each pair of entries is `<= abs_tol`; if `vec` is
something else, then exact equality is used instead.

## Usage

``` r
is_locf(vec, abs_tol, is_key)
```

## Details

We include epikey-time columns in LOCF comparisons as part of an
optimization to avoid slower grouped operations while still ensuring
that the first observation for each time series will not be marked as
LOCF. We test these key columns for exact equality to prevent chopping
off consecutive time_values during flat periods when `abs_tol` is high.

We use exact equality for non-`is.numeric` double/integer columns such
as dates, datetimes, difftimes,
[`tsibble::yearmonth`](https://tsibble.tidyverts.org/reference/year-month.html)s,
etc., as these may be used as part of re-indexing or grouping
procedures, and we don't want to change the number of groups for those
operations when we remove LOCF data during compactification.
