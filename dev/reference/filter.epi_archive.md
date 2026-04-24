# [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) for `epi_archive`s

[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) for
`epi_archive`s

## Usage

``` r
# S3 method for class 'epi_archive'
filter(.data, ..., .by = NULL, .format_aware = FALSE)
```

## Arguments

- .data:

  an `epi_archive`

- ...:

  as in
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html);
  using the `version` column is not allowed unless you use
  `.format_aware = TRUE`; see details.

- .by:

  as in
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)

- .format_aware:

  optional, `TRUE` or `FALSE`; default `FALSE`. See details.

## Details

By default, using the `version` column or measurement columns is
disabled as it's easy to get unexpected results. See if either
[`epix_as_of`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_as_of.md)
or
[`epix_slide`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_slide.md)
works for any version selection you have in mind: for version selection,
see the `version` or `.versions` args, respectively; for measurement
column-based filtering, try `filter`ing after `epix_as_of` or inside the
`.f` in
[`epix_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_slide.md).
If they don't cover your use case, then you can set
`.format_aware = TRUE` to enable usage of these columns, but be careful
to:

- Factor in that `.data$DT` may have been converted into a compact
  format based on diffing consecutive versions, and the last version of
  each observation in `.data$DT` will always be carried forward to
  future `version`s`; see details of [`as_epi_archive\`\].

- Set `clobberable_versions_start` and `versions_end` of the result
  appropriately after the `filter` call. They will be initialized with
  the same values as in `.data`.

[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
also has an optional argument `.preserve`, which should not have an
impact on (ungrouped) `epi_archive`s, and `grouped_epi_archive`s do not
currently support
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).

## Examples

``` r
# Filter to one location and a particular time range:
archive_cases_dv_subset %>%
  filter(geo_value == "fl", time_value >= as.Date("2020-10-01"))
#> An `epi_archive` object, with:
#> ℹ Time range:    2020-10-01 -- 2021-11-30 (times are days)
#> ℹ Version range: 2020-10-02 -- 2021-12-01
#> ℹ A preview of the table (23619 rows x 5 columns):
#> Key: <geo_value, time_value, version>
#>        geo_value time_value    version percent_cli case_rate_7d_av
#>           <char>     <Date>     <Date>       <num>           <num>
#>     1:        fl 2020-10-01 2020-10-02          NA       10.711424
#>     2:        fl 2020-10-01 2020-10-04    5.751017       10.711424
#>     3:        fl 2020-10-01 2020-10-05    5.721662       10.711424
#>     4:        fl 2020-10-01 2020-10-06    5.465139       10.711424
#>     5:        fl 2020-10-01 2020-10-07    5.278582       10.711424
#>    ---                                                            
#> 23615:        fl 2021-11-26 2021-11-29    1.077506        0.000000
#> 23616:        fl 2021-11-27 2021-11-28          NA        0.000000
#> 23617:        fl 2021-11-28 2021-11-29          NA        0.000000
#> 23618:        fl 2021-11-29 2021-11-30          NA        0.000000
#> 23619:        fl 2021-11-30 2021-12-01          NA        5.844879

# Convert to weekly by taking the Saturday data for each week, so that
# `case_rate_7d_av` represents a Sun--Sat average:
archive_cases_dv_subset %>%
  filter(as.POSIXlt(time_value)$wday == 6L)
#> An `epi_archive` object, with:
#> ℹ Time range:    2020-06-06 -- 2021-11-27 (times are weeks)
#> ℹ Version range: 2020-06-07 -- 2021-12-01, but no row updates recorded after
#>   2021-11-29
#> ℹ A preview of the table (18416 rows x 5 columns):
#> Key: <geo_value, time_value, version>
#>        geo_value time_value    version percent_cli case_rate_7d_av
#>           <char>     <Date>     <Date>       <num>           <num>
#>     1:        ca 2020-06-06 2020-06-07          NA        6.760295
#>     2:        ca 2020-06-06 2020-06-10    1.342609        6.760295
#>     3:        ca 2020-06-06 2020-06-11    1.581691        6.760295
#>     4:        ca 2020-06-06 2020-06-12    1.856477        6.760295
#>     5:        ca 2020-06-06 2020-06-13    1.901306        6.760295
#>    ---                                                            
#> 18412:        tx 2021-11-20 2021-11-25    2.114713       11.179158
#> 18413:        tx 2021-11-20 2021-11-26    2.089479       11.179158
#> 18414:        tx 2021-11-20 2021-11-27    2.081636       11.179158
#> 18415:        tx 2021-11-20 2021-11-29    2.031905       11.179158
#> 18416:        tx 2021-11-27 2021-11-28          NA        7.174299

# Filtering involving the `version` column or measurement columns requires
# extra care. See epix_as_of and epix_slide instead for some common
# operations. One semi-common operation that ends up being fairly simple is
# treating observations as finalized after some amount of time, and ignoring
# any revisions that were made after that point:
archive_cases_dv_subset %>%
  filter(
    version <= time_value + as.difftime(60, units = "days"),
    .format_aware = TRUE
  )
#> An `epi_archive` object, with:
#> ℹ Time range:    2020-06-01 -- 2021-11-30 (times are days)
#> ℹ Version range: 2020-06-02 -- 2021-12-01
#> ℹ A preview of the table (104394 rows x 5 columns):
#> Key: <geo_value, time_value, version>
#>         geo_value time_value    version percent_cli case_rate_7d_av
#>            <char>     <Date>     <Date>       <num>           <num>
#>      1:        ca 2020-06-01 2020-06-02          NA        6.628329
#>      2:        ca 2020-06-01 2020-06-06    2.140116        6.628329
#>      3:        ca 2020-06-01 2020-06-07    2.140116        6.628329
#>      4:        ca 2020-06-01 2020-06-08    2.140379        6.628329
#>      5:        ca 2020-06-01 2020-06-09    2.114430        6.628329
#>     ---                                                            
#> 104390:        tx 2021-11-26 2021-11-29    1.858596        7.957657
#> 104391:        tx 2021-11-27 2021-11-28          NA        7.174299
#> 104392:        tx 2021-11-28 2021-11-29          NA        6.834681
#> 104393:        tx 2021-11-29 2021-11-30          NA        8.841247
#> 104394:        tx 2021-11-30 2021-12-01          NA        9.566218
```
