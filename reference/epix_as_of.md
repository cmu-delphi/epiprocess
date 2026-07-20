# Generate a snapshot from an `epi_archive` object

Generates a snapshot in `epi_df` format from an `epi_archive` object, as
of a given version. See the [archive
vignette](https://cmu-delphi.github.io/epiprocess/articles/archive.html)
for examples.

## Usage

``` r
epix_as_of(
  x,
  version,
  min_time_value = -Inf,
  all_versions = FALSE,
  max_version = deprecated()
)
```

## Arguments

- x:

  An `epi_archive` object

- version:

  Time value specifying the max version to permit in the snapshot. That
  is, the snapshot will comprise the unique rows of the current archive
  data that represent the most up-to-date signal values, as of the
  specified `version` (and whose time values are at least
  `min_time_value`.)

- min_time_value:

  Time value specifying the min time value to permit in the snapshot.
  Default is `-Inf`, which effectively means that there is no minimum
  considered.

- all_versions:

  If `all_versions = TRUE`, then the output will be in `epi_archive`
  format, and contain rows in the specified `time_value` range having
  `version <= version`. The resulting object will cover a potentially
  narrower `version` and `time_value` range than `x`, depending on
  user-provided arguments. Otherwise, there will be one row in the
  output for the `version` of each `time_value`. Default is `FALSE`.

- max_version:

  **\[deprecated\]** please use `version` argument instead.

## Value

An `epi_df` object.

## Examples

``` r
epix_as_of(
  archive_cases_dv_subset,
  version = max(archive_cases_dv_subset$DT$version)
)
#> An `epi_df` object, 2,192 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2021-12-01
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 1–5 days
#> 
#> # A tibble: 2,192 × 4
#>    geo_value time_value percent_cli case_rate_7d_av
#>  * <chr>     <date>           <dbl>           <dbl>
#>  1 ca        2020-06-01        2.75            6.84
#>  2 ca        2020-06-02        2.57            6.82
#>  3 ca        2020-06-03        2.48            6.66
#>  4 ca        2020-06-04        2.41            6.98
#>  5 ca        2020-06-05        2.57            6.97
#>  6 ca        2020-06-06        2.63            6.66
#>  7 ca        2020-06-07        2.73            6.74
#>  8 ca        2020-06-08        3.04            6.67
#>  9 ca        2020-06-09        2.97            6.81
#> 10 ca        2020-06-10        2.99            7.13
#> # ℹ 2,182 more rows

range(archive_cases_dv_subset$DT$version) # 2020-06-02 -- 2021-12-01
#> [1] "2020-06-02" "2021-12-01"

epix_as_of(archive_cases_dv_subset, as.Date("2020-06-12"))
#> An `epi_df` object, 44 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2020-06-12
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 1–4 days
#> 
#> # A tibble: 44 × 4
#>    geo_value time_value percent_cli case_rate_7d_av
#>  * <chr>     <date>           <dbl>           <dbl>
#>  1 ca        2020-06-01        2.23            6.63
#>  2 ca        2020-06-02        2.06            6.45
#>  3 ca        2020-06-03        1.90            6.62
#>  4 ca        2020-06-04        1.79            6.64
#>  5 ca        2020-06-05        1.83            6.91
#>  6 ca        2020-06-06        1.86            6.76
#>  7 ca        2020-06-07        1.78            6.75
#>  8 ca        2020-06-08        1.90            6.90
#>  9 ca        2020-06-09       NA               7.02
#> 10 ca        2020-06-10       NA               7.36
#> # ℹ 34 more rows

# --- Advanced: ---

# When requesting recent versions of a data set, there can be some
# reproducibility issues. For example, requesting data as of the current date
# may return different values based on whether today's data is available yet
# or not. Other factors include the time it takes between data becoming
# available and when you download the data, and whether the data provider
# will overwrite ("clobber") version data rather than just publishing new
# versions. You can include information about these factors by setting the
# `clobberable_versions_start` and `versions_end` of an `epi_archive`, in
# which case you will get warnings about potential reproducibility issues:

archive_cases_dv_subset2 <- as_epi_archive(
  archive_cases_dv_subset$DT,
  # Suppose last version with an update could potentially be rewritten
  # (a.k.a. "hotfixed", "clobbered", etc.):
  clobberable_versions_start = max(archive_cases_dv_subset$DT$version),
  # Suppose today is the following day, and there are no updates out yet:
  versions_end = max(archive_cases_dv_subset$DT$version) + 1L
)

epix_as_of(archive_cases_dv_subset2, max(archive_cases_dv_subset$DT$version))
#> Warning: Getting data as of some recent version which could still be overwritten (under
#> routine circumstances) without assigning a new version number (a.k.a.
#> "clobbered").  Thus, the snapshot that we produce here should not be expected
#> to be reproducible later. See `?epi_archive` for more info and `?epix_as_of` on
#> how to muffle.
#> An `epi_df` object, 2,192 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2021-12-01
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 1–5 days
#> 
#> # A tibble: 2,192 × 4
#>    geo_value time_value percent_cli case_rate_7d_av
#>  * <chr>     <date>           <dbl>           <dbl>
#>  1 ca        2020-06-01        2.75            6.84
#>  2 ca        2020-06-02        2.57            6.82
#>  3 ca        2020-06-03        2.48            6.66
#>  4 ca        2020-06-04        2.41            6.98
#>  5 ca        2020-06-05        2.57            6.97
#>  6 ca        2020-06-06        2.63            6.66
#>  7 ca        2020-06-07        2.73            6.74
#>  8 ca        2020-06-08        3.04            6.67
#>  9 ca        2020-06-09        2.97            6.81
#> 10 ca        2020-06-10        2.99            7.13
#> # ℹ 2,182 more rows
```
