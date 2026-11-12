# Given a tibble as would be found in an epi_archive, remove duplicate entries.

Works by shifting all rows except the version, then comparing values to
see if they've changed. We need to arrange in descending order, but note
that we don't need to group, since at least one column other than
version has changed, and so is kept.

## Usage

``` r
apply_compactify(
  updates_df,
  ukey_names,
  abs_tol = 0,
  init_nas_are_locf = FALSE
)
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
  [`vec_approx_equal`](https://cmu-delphi.github.io/epiprocess/dev/reference/vec_approx_equal.md)

- init_nas_are_locf:

  bool; do we treat [entirely-missing
  values](https://vctrs.r-lib.org/reference/missing.html) in the initial
  measurement for each epikey-time as LOCF? Ordinarily `FALSE`, but
  `TRUE` if we're trying to "invert" an
  [`epix_merge`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_merge.md),
  i.e., we've just narrowed down the value column set and are trying to
  remove extra `NA`s (and other rows) created by using
  [`epix_merge`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_merge.md)
  / the `epi_archive` format. Currently, this is ordinarily `FALSE` in
  order to preserve explicit measurements of `NA` provided by the user;
  this also matches "vanilla expectations", as outer joins / the data
  frame format also promote some implicit NAs into explicit ones,
  conflating their origins. We're forced into a judgment call here by
  the current `epi_archive` format.

## Examples

``` r

# Typical usage: compactifying epi_archive-like data
tibble::tibble(geo_value = 1, time_value = 1, version = 1:4, value = c(1, 1, 3, 4)) %>%
  epiprocess:::apply_compactify(ukey_names = c("geo_value", "time_value", "version"))
#> # A tibble: 3 × 4
#>   geo_value time_value version value
#>       <dbl>      <dbl>   <int> <dbl>
#> 1         1          1       1     1
#> 2         1          1       3     3
#> 3         1          1       4     4

# Other usage: compactifying data that may not have geo or time
data.table::data.table(version = 2:6, version_info = c(1, 1, 3, 4, 0), key = "version") %>%
  epiprocess:::apply_compactify(ukey_names = "version")
#> Key: <version>
#>    version version_info
#>      <int>        <num>
#> 1:       2            1
#> 2:       4            3
#> 3:       5            4
#> 4:       6            0
```
