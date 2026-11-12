# Apply an update (e.g., from `tbl_diff2`) to a snapshot

Apply an update (e.g., from `tbl_diff2`) to a snapshot

## Usage

``` r
tbl_patch(snapshot, update, ukey_names)
```

## Arguments

- snapshot:

  tibble or `NULL`; entire data set as of some version, or `NULL` to
  treat `update` as the initial version of the data set.

- update:

  tibble; ukeys + initial values for added rows, ukeys + new values for
  changed rows. Deletions must be imprecisely represented as changing
  all values to NAs.

- ukey_names:

  character; names of columns that should form a unique key for
  `snapshot` and for `update`. Uniqueness is unchecked; if you don't
  have this guaranteed, see
  [`check_ukey_unique()`](https://cmu-delphi.github.io/epiprocess/dev/reference/check_ukey_unique.md).

## Value

tibble; snapshot of the data set with the update applied.
