# Copy a `DT` of an `epi_archive`, replacing its `time_value` and `version` columns

The result's other columns are deep copies of `dtbl`'s, so they don't
alias `dtbl`. To avoid extra copies, `time_values` and `versions` are
used directly as the new columns, not copied, so callers should pass
freshly computed vectors that they won't use elsewhere: the result may
modify them in place (e.g., the `setkeyv()` below may reorder them).

## Usage

``` r
dtbl_with_time_values_versions(dtbl, time_values, versions)
```

## Arguments

- dtbl:

  the `DT` of an `epi_archive`.

- time_values, versions:

  vectors with as many entries as `dtbl` has rows, to become the
  result's `time_value` and `version` columns.

## Value

a `data.table` with the same column order and key as `dtbl`.
