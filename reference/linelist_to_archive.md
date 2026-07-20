# Convert a line list to an `epi_archive` object

Converts a line list (a data frame where each row represents a case or
event) into an
[`epi_archive`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
object. This function requires "recorded" and "deleted" timestamps, and
generates a time series of counts (e.g. daily hospitalizations) as they
would have appeared at different points in time.

## Usage

``` r
linelist_to_archive(
  x,
  ...,
  geo_value = NULL,
  other_keys = NULL,
  time_value = NULL,
  version_recorded = NULL,
  version_deleted = NULL,
  is_deletion = NULL,
  value = NULL,
  id = NULL,
  clobberable_versions_start = NA,
  versions_end = NULL
)
```

## Arguments

- x:

  A data frame (line list).

- ...:

  Should be empty.

- geo_value, other_keys, time_value, version_recorded, version_deleted:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns in `x` representing:

  - `geo_value`: the geographic location of the event.

  - `other_keys`: (optional) additional key columns (e.g. age group).

  - `time_value`: the time of the event.

  - `version_recorded`: the time at which the event became
    known/recorded.

  - `version_deleted`: (optional) the time at which the event was
    removed/deleted. If `NULL` (default), it is assumed no events are
    deleted. Mutually exclusive with `is_deletion`.

- is_deletion:

  (optional)
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column in `x` indicating if the row is a deletion (`TRUE`/1) or an
  entry (`FALSE`/0). Used for "chart-style" linelists where each row is
  an update event and `version_recorded` represents when the update
  occurred. Mutually exclusive with `version_deleted`.

- value:

  Either `NULL` (default) or a string specifying the name of the output
  count column. If `NULL` and `other_keys` is empty, defaults to
  "count".

- id:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional column identifying unique events/cases.

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

- versions_end:

  optional; as in
  [`as_epi_archive`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md),
  or `NULL`. If the latest version(s) had no new events recorded or
  deleted, you can note this using `versions_end`. `NULL`, the default,
  will assume there are no such versions.

## Value

An
[`epi_archive`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
object.

## Examples

``` r
library(dplyr)

linelist <- tibble(
  event_id = 1:3,
  geo_value = c("ca", "ca", "ca"),
  time_value = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02")),
  report_date = as.Date(c("2022-01-02", "2022-01-02", "2022-01-03")),
  delete_date = as.Date(c("2022-01-04", NA, NA))
)

archive <- linelist_to_archive(
  linelist,
  geo_value = geo_value,
  time_value = time_value,
  version_recorded = report_date,
  version_deleted = delete_date
)
#> ℹ Defaulting to col `event_id` as `id`.
```
