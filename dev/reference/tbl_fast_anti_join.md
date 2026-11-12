# Variation on [`dplyr::anti_join`](https://dplyr.tidyverse.org/reference/filter-joins.html) for speed + tolerance setting

Variation on
[`dplyr::anti_join`](https://dplyr.tidyverse.org/reference/filter-joins.html)
for speed + tolerance setting

## Usage

``` r
tbl_fast_anti_join(x, y, ukey_names, val_names, abs_tol = 0)
```

## Arguments

- x:

  tibble; `x[ukey_names]` must not have any duplicate rows

- y:

  tibble; `y[ukey_names]` must not have any duplicate rows

- ukey_names:

  chr; names of columns that form a unique key, for `x` and for `y`

- val_names:

  chr; names of columns which should be treated as value/measurement
  columns, and compared with a tolerance

- abs_tol:

  scalar non-negative numeric; absolute tolerance with which to compare
  value columns; see
  [`vec_approx_equal`](https://cmu-delphi.github.io/epiprocess/dev/reference/vec_approx_equal.md)

## Value

rows from `x` that either (a) don't have a (0-tolerance) matching ukey
in `y`, or (b) have a matching ukey in `y`, but don't have approximately
equal value column values
