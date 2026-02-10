# `as_epi_archive` converts a data frame, data table, or tibble into an `epi_archive` object.

The second main data structure for storing time series in `epiprocess`.
It is similar to `epi_df` in that it fundamentally a table with a few
required columns that stores epidemiological time series data. An
`epi_archive` requires a `geo_value`, `time_value`, and `version` column
(and possibly other key columns) along with measurement values. In
brief, an `epi_archive` is a history of the time series data, where the
`version` column tracks the time at which the data was available. This
allows for version-aware forecasting.

`new_epi_archive` is the low-level constructor for `epi_archive` objects
that only performs some fast, basic checks on the inputs.
`validate_epi_archive` can perform more costly validation checks on its
output. But most users should use `as_epi_archive`, which performs all
necessary checks and has some additional features.

Does not validate `geo_type` or `time_type` against `geo_value` and
`time_value` columns. These are assumed to have been set to compatibly.

## Usage

``` r
as_epi_archive(
  x,
  geo_type = deprecated(),
  time_type = deprecated(),
  other_keys = character(),
  compactify = TRUE,
  compactify_abs_tol = 0,
  clobberable_versions_start = NA,
  .versions_end = max_version_with_row_in(x),
  ...,
  versions_end = .versions_end
)

is_epi_archive(x)

new_epi_archive(
  x,
  geo_type,
  time_type,
  other_keys,
  clobberable_versions_start,
  versions_end
)

validate_epi_archive(x)
```

## Arguments

- x:

  A data.frame, data.table, or tibble, with columns `geo_value`,
  `time_value`, `version`, and then any additional number of columns.

- geo_type:

  DEPRECATED Has no effect. Geo value type is inferred from the location
  column and set to "custom" if not recognized.

- time_type:

  DEPRECATED Has no effect. Time value type inferred from the time
  column and set to "custom" if not recognized. Unpredictable behavior
  may result if the time type is not recognized.

- other_keys:

  Character vector specifying the names of variables in `x` that should
  be considered key variables (in the language of `data.table`) apart
  from "geo_value", "time_value", and "version". Typical examples are
  "age" or more granular geographies.

- compactify:

  Optional; `TRUE`, `FALSE`, or `"message"`. `TRUE` will remove some
  redundant rows, `FALSE` will not. `"message"` is like `TRUE` but will
  emit a message if anything was changed. Default is `TRUE`. See more
  information below under "Compactification:".

- compactify_abs_tol:

  Optional; double. A tolerance level used to detect approximate
  equality for compactification. The default is 0, which corresponds to
  exact equality. Consider using this if your value columns undergo tiny
  nonmeaningful revisions and the archive object with the default
  setting is too large.

- clobberable_versions_start:

  Optional; `length`-1; either a value of the same `class` as
  `x$version`, or an `NA` of any `class`: specifically, either (a) the
  earliest version that could be subject to "clobbering" (being
  overwritten with different update data, but using the *same* version
  tag as the old update data), or (b) `NA`, to indicate that no versions
  are clobberable. There are a variety of reasons why versions could be
  clobberable under routine circumstances, such as (a) today's version
  of one/all of the columns being published after initially being filled
  with `NA` or LOCF, (b) a buggy version of today's data being published
  but then fixed and republished later in the day, or (c) data pipeline
  delays (e.g., publisher uploading, periodic scraping, database
  syncing, periodic fetching, etc.) that make events (a) or (b)
  reflected later in the day (or even on a different day) than expected;
  potential causes vary between different data pipelines. The default
  value is `NA`, which doesn't consider any versions to be clobberable.
  Another setting that may be appropriate for some pipelines is
  `max_version_with_row_in(x)`.

- .versions_end:

  location based versions_end, used to avoid prefix `version = issue`
  from being assigned to `versions_end` instead of being used to rename
  columns.

- ...:

  used for specifying column names, as in
  [`dplyr::rename`](https://dplyr.tidyverse.org/reference/rename.html).
  For example `version = release_date`

- versions_end:

  Optional; length-1, same `class` as `x$version`: what is the last
  version we have observed? The default is `max_version_with_row_in(x)`,
  but values greater than this could also be valid, and would indicate
  that we observed additional versions of the data beyond
  `max(x$version)`, but they all contained empty updates. (The default
  value of `clobberable_versions_start` does not fully trust these empty
  updates, and assumes that any version `>= max(x$version)` could be
  clobbered.) If `nrow(x) == 0`, then this argument is mandatory.

## Value

- Of `as_epi_archive`: an `epi_archive` object

&nbsp;

- Of `is_epi_archive`: `TRUE` if the object inherits from `epi_archive`,
  otherwise `FALSE`.

&nbsp;

- Of `new_epi_archive`: an (unvalidated) `epi_archive`

&nbsp;

- Of `validate_epi_archive`: an `epi_archive`,
  [invisibly](https://rdrr.io/r/base/invisible.html) (or raises an error
  if `x` was invalid)

## Details

An `epi_archive` contains a `data.table` object `DT` (from the
`{data.table}` package), with (at least) the following columns:

- `geo_value`: the geographic value associated with each row of
  measurements,

- `time_value`: the time value associated with each row of measurements,

- `version`: the time value specifying the version for each row of
  measurements. For example, if in a given row the `version` is January
  15, 2022 and `time_value` is January 14, 2022, then this row contains
  the measurements of the data for January 14, 2022 that were available
  one day later.

The variables `geo_value`, `time_value`, `version` serve as key
variables for the data table (in addition to any other keys specified in
the metadata). There can only be a single row per unique combination of
key variables. The keys for an `epi_archive` can be viewed with
`key(epi_archive$DT)`.

### Compactification

By default, an `epi_archive` will compactify the data table to remove
redundant rows. This is done by not storing rows that have the same
value, except for the `version` column (this is essentially a last
observation carried forward, but along the version index). This is done
to save space and improve performance. If you do not want to compactify
the data, you can set `compactify = FALSE` in `as_epi_archive()`.

Note that in some data scenarios, LOCF may not be appropriate. For
instance, if you expected data to be updated on a given day, but your
data source did not update, then it could be reasonable to code the data
as `NA` for that day, instead of assuming LOCF.

`NA`s *can* be introduced by `epi_archive` methods for other reasons,
e.g., in
[`epix_fill_through_version`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_fill_through_version.md)
and
[`epix_merge`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_merge.md),
if requested, to represent potential update data that we do not yet have
access to; or in
[`epix_merge`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_merge.md)
to represent the "value" of an observation before the version in which
it was first released, or if no version of that observation appears in
the archive data at all.

### Metadata

The following pieces of metadata are included as fields in an
`epi_archive` object:

- `geo_type`: the type for the geo values.

- `time_type`: the type for the time values.

- `other_keys`: any additional keys as a character vector. Typical
  examples are "age" or sub-geographies.

While this metadata is not protected, it is generally recommended to
treat it as read-only, and to use the `epi_archive` methods to interact
with the data archive. Unexpected behavior may result from modifying the
metadata directly.

## See also

[`epix_as_of`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_as_of.md)
[`epix_merge`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_merge.md)
[`epix_slide`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_slide.md)

## Examples

``` r
# Simple ex. with necessary keys
tib <- tibble::tibble(
  geo_value = rep(c("ca", "hi"), each = 5),
  time_value = rep(seq(as.Date("2020-01-01"),
    by = 1, length.out = 5
  ), times = 2),
  version = rep(seq(as.Date("2020-01-02"),
    by = 1, length.out = 5
  ), times = 2),
  value = rnorm(10, mean = 2, sd = 1)
)

toy_epi_archive <- tib %>% as_epi_archive()
toy_epi_archive
#> → An `epi_archive` object, with metadata:
#> ℹ Min/max time values: 2020-01-01 / 2020-01-05
#> ℹ First/last version with update: 2020-01-02 / 2020-01-06
#> ℹ Versions end: 2020-01-06
#> ℹ A preview of the table (10 rows x 4 columns):
#> Key: <geo_value, time_value, version>
#>     geo_value time_value    version     value
#>        <char>     <Date>     <Date>     <num>
#>  1:        ca 2020-01-01 2020-01-02 3.1484116
#>  2:        ca 2020-01-02 2020-01-03 0.1781823
#>  3:        ca 2020-01-03 2020-01-04 1.7526747
#>  4:        ca 2020-01-04 2020-01-05 1.7558004
#>  5:        ca 2020-01-05 2020-01-06 1.7172946
#>  6:        hi 2020-01-01 2020-01-02 1.4463006
#>  7:        hi 2020-01-02 2020-01-03 2.6289820
#>  8:        hi 2020-01-03 2020-01-04 4.0650249
#>  9:        hi 2020-01-04 2020-01-05 0.3690106
#> 10:        hi 2020-01-05 2020-01-06 2.5124269

# Ex. with an additional key for county
df <- data.frame(
  geo_value = c(replicate(2, "ca"), replicate(2, "fl")),
  county = c(1, 3, 2, 5),
  time_value = c(
    "2020-06-01",
    "2020-06-02",
    "2020-06-01",
    "2020-06-02"
  ),
  version = c(
    "2020-06-02",
    "2020-06-03",
    "2020-06-02",
    "2020-06-03"
  ),
  cases = c(1, 2, 3, 4),
  cases_rate = c(0.01, 0.02, 0.01, 0.05)
)

x <- df %>% as_epi_archive(other_keys = "county")
#> Warning: Unsupported time type in column `x$time_value`, with class `character`.
#> Time-related functionality may have unexpected behavior.
```
