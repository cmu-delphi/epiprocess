# Internal helper; lgl; which updates are LOCF and should thus be dropped when compactifying

LOCF meaning last observation carried forward (to later versions).

## Usage

``` r
update_is_locf(arranged_updates_df, ukey_names, abs_tol, init_nas_are_locf)
```

## Arguments

- arranged_updates_df:

  an arranged update data frame like an `epi_archive` `DT` (not
  validated)

- ukey_names:

  (not validated:) chr; the archive/equivalent
  [`key_colnames`](https://cmu-delphi.github.io/epiprocess/dev/reference/key_colnames.md);
  must include `"version"`. Does not necessarily need to have
  `geo_value` and `time_value`.

- abs_tol:

  (not validated:) as in
  [`apply_compactify`](https://cmu-delphi.github.io/epiprocess/dev/reference/apply_compactify.md)

- init_nas_are_locf:

  (not validated:) as in
  [`apply_compactify`](https://cmu-delphi.github.io/epiprocess/dev/reference/apply_compactify.md)

## Value

lgl

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
