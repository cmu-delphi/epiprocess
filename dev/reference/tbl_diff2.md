# Calculate compact patch to move from one snapshot/update to another

Calculate compact patch to move from one snapshot/update to another

## Usage

``` r
tbl_diff2(
  earlier_snapshot,
  later_tbl,
  ukey_names,
  later_format = c("snapshot", "update"),
  compactify_abs_tol = 0
)
```

## Arguments

- earlier_snapshot:

  tibble or `NULL`; `NULL` represents that there was no data before
  `later_tbl`.

- later_tbl:

  tibble; must have the same column names as `earlier_snapshot` if it is
  a tibble.

- ukey_names:

  character; column names that together, form a unique key for
  `earlier_snapshot` and for `later_tbl`. This is unchecked; see
  [`check_ukey_unique`](https://cmu-delphi.github.io/epiprocess/dev/reference/check_ukey_unique.md)
  if you don't already have this guaranteed.

- later_format:

  "snapshot" or "update"; default is "snapshot". If "snapshot",
  `later_tbl` will be interpreted as a full snapshot of the data set
  including all ukeys, and any ukeys that are in `earlier_snapshot` but
  not in `later_tbl` are interpreted as deletions, which are currently
  (imprecisely) represented in the output patch as revisions of all
  non-`ukey_names` columns to NA values (using `{vctrs}`). If "update",
  then it's assumed that any deletions have already been represented
  this way in `later_tbl` and any ukeys not in `later_tbl` are simply
  unchanged; we are just ensuring that the update is fully compact for
  the given `compactify_abs_tol`.

- compactify_abs_tol:

  compactification tolerance; see `apply_compactify`

## Value

a tibble in compact "update" (diff) format
