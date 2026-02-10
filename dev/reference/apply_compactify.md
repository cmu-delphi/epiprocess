# Given a tibble as would be found in an epi_archive, remove duplicate entries.

Works by shifting all rows except the version, then comparing values to
see if they've changed. We need to arrange in descending order, but note
that we don't need to group, since at least one column other than
version has changed, and so is kept.

## Usage

``` r
apply_compactify(updates_df, ukey_names, abs_tol = 0)
```

## Arguments

- updates_df:

  DT of an `epi_archive` or something analogous (though potentially
  unsorted) of another class

- ukey_names:

  chr; the column names forming a unique key for the `updates_df`;
  "version" must come last. For an `epi_archive`'s `DT`, this would be
  `key(DT)`.

- abs_tol:

  numeric, \>=0; absolute tolerance to use on numeric measurement
  columns when determining whether something can be compactified away;
  see
  [`is_locf`](https://cmu-delphi.github.io/epiprocess/dev/reference/is_locf.md)
