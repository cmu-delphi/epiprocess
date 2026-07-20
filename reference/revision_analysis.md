# A function to describe revision behavior for an archive.

`revision_summary` removes all missing values (if requested), and then
computes some basic statistics about the revision behavior of an
archive, returning a tibble summarizing the revisions per
time_value+epi_key features (limited to those that have data available
past the min waiting period to compare against, and are not detected as
added in a bulk report). The columns returned are:

1.  `n_revisions`: the total number of revisions for that entry

2.  `min_lag`: the minimum time to any value (if `drop_nas=FALSE`, this
    includes `NA`'s)

3.  `max_lag`: the amount of time until the final (new) version (same
    caveat for `drop_nas=FALSE`, though it is far less likely to matter)

4.  `min_value`: the minimum value across revisions

5.  `max_value`: the maximum value across revisions

6.  `median_value`: the median value across revisions

7.  `spread`: the difference between the smallest and largest values
    (this always excludes `NA` values)

8.  `rel_spread`: `spread` divided by the largest value (so it will
    always be less than 1). Note that this need not be the final value.
    It will be `NA` whenever `spread` is 0.

9.  `lag_near_latest`: the time taken for the revisions to settle to
    within `within_latest` (default 20%) of the final value and stay
    there. For example, consider the series (0, 20, 99, 150, 102, 100);
    then `lag_near_latest` is 5, since even though 99 is within 20%, it
    is outside the window afterwards at 150.

## Usage

``` r
revision_analysis(
  epi_arch,
  ...,
  min_waiting_period = as.difftime(60, units = "days"),
  within_latest = 0.2,
  bulk_reporting_level = 0.8,
  bulk_reporting_multiplier = 1.2,
  compactify = TRUE,
  compactify_abs_tol = 0,
  compactify_drop_initial_nas = TRUE,
  drop_nas = FALSE,
  return_only_tibble = FALSE
)

# S3 method for class 'revision_analysis'
print(
  x,
  quick_revision = as.difftime(3, units = "days"),
  few_revisions = 3,
  abs_spread_threshold = NULL,
  rel_spread_threshold = 0.1,
  ...
)

revision_summary(
  epi_arch,
  ...,
  min_waiting_period = as.difftime(60, units = "days"),
  within_latest = 0.2,
  bulk_reporting_level = 0.8,
  bulk_reporting_multiplier = 1.2,
  compactify = TRUE,
  compactify_abs_tol = 0,
  compactify_drop_initial_nas = TRUE,
  drop_nas = FALSE,
  return_only_tibble = FALSE
)
```

## Arguments

- epi_arch:

  an epi_archive to be analyzed

- ...:

  \<[`tidyselect`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>,
  used to choose the column to summarize. If empty and there is only one
  value/measurement column (i.e., not in
  [`key_colnames`](https://cmu-delphi.github.io/epiprocess/reference/key_colnames.md))
  in the archive, it will automatically select it. If supplied, `...`
  must select exactly one column.

- min_waiting_period:

  `difftime`, integer or `NULL`. Sets a cutoff: any time_values that
  have not had at least `min_waiting_period` to stabilize as of the
  `versions_end` are removed. `min_waiting_period` should characterize
  the typical time during which most significant revisions occur. The
  default of 60 days corresponds to a typical near-final value for case
  counts as reported in the context of insurance. To avoid this
  filtering, either set to `NULL` or 0. A `difftime` will be rounded up
  to the appropriate `time_type` if necessary (that is 5 days will be
  rounded to 1 week if the data is weekly).

- within_latest:

  double between 0 and 1. Determines the threshold used for the `lag_to`

- bulk_reporting_level, bulk_reporting_multiplier:

  numeric; the former between 0 and 1, typically close to but less than
  one, and the latter `>= 1`; defaults of 0.8 and 1.2, respectively.
  Determines how to detect bulk reporting. Consider the distribution of
  "max initial lags" across geodemographic group x version pairs that
  add initial observations for new time values; a bulk reporting lag
  threshold is determined by taking the `bulk_reporting_level`-th
  quantile of this distribution, multiplying by
  `bulk_reporting_multiplier`, and rounding to an integer number of time
  intervals. To avoid flagging anything as bulk reporting, set
  `bulk_reporting_level = 1`.

- compactify:

  bool. If `TRUE`, we will compactify after the signal requested in
  `...` has been selected on its own and the `drop_nas` step. This
  helps, for example, to give similar results when called on
  [merged](https://cmu-delphi.github.io/epiprocess/reference/epix_merge.md)
  and single-signal archives, since merged archives record an update
  when any of the other signals change, not just the requested signal.
  The default is `TRUE`.

- compactify_abs_tol:

  length-1 double, used if `compactify` is `TRUE`, it determines the
  threshold for when two doubles are considered identical.

- compactify_drop_initial_nas:

  bool; should we drop initial estimates of NA during the
  compactification step? Default is TRUE, because these NAs are likely
  present due to
  [`epix_merge()`](https://cmu-delphi.github.io/epiprocess/reference/epix_merge.md)ing
  with a more timely indicator, and the upstream source probably didn't
  actually report explicit NAs as provisional estimates.

- drop_nas:

  bool, do we drop all `NA` values (not just initial estimates of `NA`)
  from the archive data structure prior to (optional) compactification?
  This both strips (i) initial estimates of NA and (ii) explicit
  revisions from a non-NA estimate to NA. Equivalent to
  `compactify_drop_initial_nas` if `compactify` is `TRUE`. Default is
  FALSE, favoring
  `compactify = TRUE, compactify_drop_initial_nas = TRUE`.

- return_only_tibble:

  boolean to return only the simple `tibble` of computational results
  rather than the complete S3 object.

- x:

  a `revision_analysis` object

- quick_revision:

  Difftime or integer (integer is treated as days). The amount of time
  between the final revision and the actual time_value to consider the
  revision quickly resolved. Default of 3 days. This will be rounded up
  to the appropriate `time_type` if necessary (that is 5 days will be
  rounded to 1 week if the data is weekly).

- few_revisions:

  Integer. The upper bound on the number of revisions to consider "few".
  Default is 3.

- abs_spread_threshold:

  Scalar numeric. The maximum spread used to characterize revisions
  which don't actually change very much. Default is 5% of the maximum
  value in the dataset, but this is the most unit dependent of values,
  and likely needs to be chosen appropriate for the scale of the
  dataset.

- rel_spread_threshold:

  Scalar between 0 and 1. The relative spread fraction used to
  characterize revisions which don't actually change very much. Default
  is .1, or 10% of the final value

## Value

An S3 object with class `revision_analysis`. This function is typically
called for the purposes of inspecting the printed output. The results of
the computations are available in
`revision_analysis(...)$revision_behavior`. If you only want to access
the internal computations, use `return_only_tibble = TRUE`.

## Details

Applies to `epi_archive`s with `time_type`s of `"day"`, `"week"`, and
`"yearmonth"`. It can also work with a `time_type` of `"integer"` if the
possible `time_values` are all consecutive integers; you will need to
manually specify the `min_waiting_period` and `quick_revision`, though.
Using a `time_type` of `"integer"` with week numbers like 202501 will
produce incorrect results for some calculations, since week numbering
contains jumps at year boundaries.

## Examples

``` r

# Print revision summary:
revision_analysis(archive_cases_dv_subset, percent_cli)
#> 
#> ── Revision analysis for archive spanning time values 2020-06-01 to 2021-11-30. ──
#> 
#> ── Across epi_key + versions that add new time values: 
#> Freshest new time value's lag/latency:
#>      min median     mean    max
#>   3 days 3 days 3.1 days 4 days
#> Farthest-back new time value's lag/latency:
#>      min median     mean     max
#>   3 days 3 days 3.3 days 12 days
#> 
#> ── Across epi_key + time_value + versions: 
#> Fraction of all versions that are `NA`:
#> • 0 out of 112,360 (0%)
#> 
#> ── Bulk reporting adding initial observations for older epikey + time values: 
#> Initial lags above 5 days were counted as bulk reporting.
#> Fraction of epi_key + time_values initially added by bulk reporting:
#> • 116 out of 1,956 (5.93%)
#> Versions containing bulk reporting: 10
#> • (2020-08-03, 2020-08-16, 2020-08-23, 2020-09-25, 2021-02-22, 2021-04-15,
#> 2021-07-24, 2021-08-24, 2021-09-23, and 2021-09-30)
#> Versions adding epikey + time values but no bulk reporting: 400
#> Revision-only versions: 45
#> 
#> ── Remaining information is for non-bulk-reported epikey + time values
#>    with semi-stable versions past the waiting period available. 
#> 
#> ── Fraction of epi_key + time_values with 
#> No revisions:
#> • 0 out of 1,840 (0%)
#> Quick revisions (last revision within 3 days of the `time_value`):
#> • 0 out of 1,840 (0%)
#> Few revisions (At most 3 revisions for that `time_value`):
#> • 0 out of 1,840 (0%)
#> 
#> ── Fraction of revised epi_key + time_values which have: 
#> Less than 0.1 spread in relative value:
#> • 66 out of 1,840 (3.59%)
#> Spread of more than 2.221 in actual value (when revised):
#> • 658 out of 1,840 (35.76%)
#> 
#> ── Days until within 20% of the latest value: 
#>      min median     mean     max
#>   3 days 5 days 8.9 days 67 days
#> 
#> ── Days until at the latest lag: 
#>       min  median      mean     max
#>   58 days 73 days 70.7 days 74 days

# Print some underlying data:
revision_example <- revision_analysis(archive_cases_dv_subset, percent_cli)
revision_example$revision_behavior %>% arrange(desc(spread))
#> # A tibble: 1,840 × 11
#>    time_value geo_value n_revisions min_lag max_lag lag_near_latest spread
#>    <date>     <chr>           <int> <drtn>  <drtn>  <drtn>           <dbl>
#>  1 2020-12-26 ca                 62 3 days  73 days  6 days          14.1 
#>  2 2020-12-25 ca                 62 3 days  73 days  7 days          13.2 
#>  3 2020-11-27 fl                 66 3 days  73 days  4 days          12.0 
#>  4 2021-09-27 fl                 43 3 days  63 days 59 days           9.79
#>  5 2020-12-25 fl                 62 3 days  73 days  4 days           9.75
#>  6 2021-09-26 fl                 43 4 days  64 days 60 days           9.48
#>  7 2021-09-27 ca                 43 3 days  63 days  8 days           9.31
#>  8 2021-09-25 fl                 43 5 days  65 days 61 days           8.83
#>  9 2020-11-05 ny                 66 3 days  73 days 11 days           8.64
#> 10 2020-11-27 tx                 66 3 days  73 days 10 days           8.56
#> # ℹ 1,830 more rows
#> # ℹ 4 more variables: rel_spread <dbl>, min_value <dbl>, max_value <dbl>,
#> #   median_value <dbl>
```
