# Fill `epi_archive` unobserved history

This function fills in missing version history in an `epi_archive`
object up to a specified version, updating the `versions_end` field as
necessary. Note that the filling is done in a compactified way, see
details.

## Usage

``` r
epix_fill_through_version(x, fill_versions_end, how = c("na", "locf"))
```

## Arguments

- x:

  An `epi_archive`

- fill_versions_end:

  a scalar of the same class&type as `x$version`: the version through
  which to fill in missing version history; the `epi_archive`'s
  `versions_end` attribute will be set to this, unless it already had a
  later `$versions_end`.

- how:

  Optional; `"na"` or `"locf"`: `"na"` fills missing version history
  with `NA`s, `"locf"` fills missing version history with the last
  version of each observation carried forward (LOCF). Default is `"na"`.

## Value

An `epi_archive`

An `epi_archive`

## Details

Note that we generally store `epi_archive`'s in a compacted form,
meaning that, implciitly, if a version does not exist, but the
`version_end` attribute is greater, then it is understood that all the
versions in between had the same value as the last observed version.
This affects the behavior of this function in the following ways:

- if `how = "na"`, then the function will fill in at most one missing
  version with `NA` and the rest will be implicit.

- if `how = "locf"`, then the function will not fill any values.

## Examples

``` r
test_date <- as.Date("2020-01-01")
ea_orig <- as_epi_archive(data.table::data.table(
  geo_value = "ak",
  time_value = test_date + c(rep(0L, 5L), 1L),
  version = test_date + c(1:5, 2L),
  value = 1:6
))
epix_fill_through_version(ea_orig, test_date + 8, "na")
#> An `epi_archive` object, with:
#> ℹ Time range:    2020-01-01 -- 2020-01-02 (times are days)
#> ℹ Version range: 2020-01-02 -- 2020-01-09, but no row updates recorded after
#>   2020-01-07
#> ℹ A preview of the table (8 rows x 4 columns):
#> Key: <geo_value, time_value, version>
#>    geo_value time_value    version value
#>       <char>     <Date>     <Date> <int>
#> 1:        ak 2020-01-01 2020-01-02     1
#> 2:        ak 2020-01-01 2020-01-03     2
#> 3:        ak 2020-01-01 2020-01-04     3
#> 4:        ak 2020-01-01 2020-01-05     4
#> 5:        ak 2020-01-01 2020-01-06     5
#> 6:        ak 2020-01-01 2020-01-07    NA
#> 7:        ak 2020-01-02 2020-01-03     6
#> 8:        ak 2020-01-02 2020-01-07    NA
epix_fill_through_version(ea_orig, test_date + 8, "locf")
#> An `epi_archive` object, with:
#> ℹ Time range:    2020-01-01 -- 2020-01-02 (times are days)
#> ℹ Version range: 2020-01-02 -- 2020-01-09, but no row updates recorded after
#>   2020-01-06
#> ℹ A preview of the table (6 rows x 4 columns):
#> Key: <geo_value, time_value, version>
#>    geo_value time_value    version value
#>       <char>     <Date>     <Date> <int>
#> 1:        ak 2020-01-01 2020-01-02     1
#> 2:        ak 2020-01-01 2020-01-03     2
#> 3:        ak 2020-01-01 2020-01-04     3
#> 4:        ak 2020-01-01 2020-01-05     4
#> 5:        ak 2020-01-01 2020-01-06     5
#> 6:        ak 2020-01-02 2020-01-03     6
```
