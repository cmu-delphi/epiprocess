# Internal helper; lgl; which updates are LOCF

(Not validated:) Must be called on an `epi_archive`'s `DT` or a data
frame formatted like one.

## Usage

``` r
update_is_locf(arranged_updates_df, ukey_names, abs_tol, init_nas_are_locf)
```

## Arguments

- arranged_updates_df:

  an arranged update data frame like an `epi_archive` `DT`

- ukey_names:

  (not validated:) chr; the archive/equivalent
  [`key_colnames`](https://cmu-delphi.github.io/epiprocess/dev/reference/key_colnames.md);
  must include `"version"`.

- abs_tol:

  (not validated:) as in
  [`apply_compactify`](https://cmu-delphi.github.io/epiprocess/dev/reference/apply_compactify.md)

- init_nas_are_locf:

  (not validated:) as in
  [`apply_compactify`](https://cmu-delphi.github.io/epiprocess/dev/reference/apply_compactify.md)

## Value

lgl
