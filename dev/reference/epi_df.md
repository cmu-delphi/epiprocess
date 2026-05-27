# Test for `epi_df` format

One of the two main data structures for storing time series in
`epiprocess`. It is simply tibble with at least two columns, `geo_value`
and `time_value`, that provide the keys for the time series. It can have
any other columns, which can be seen as measured variables at each key.
In brief, an `epi_df` represents a snapshot of an epidemiological data
set at a point in time.

## Usage

``` r
as_epi_df(x, ...)

# S3 method for class 'epi_df'
as_epi_df(x, ...)

# S3 method for class 'tbl_df'
as_epi_df(
  x,
  geo_type = deprecated(),
  time_type = deprecated(),
  as_of,
  other_keys = character(),
  signal_format = c("auto", "wide", "long"),
  signal_var = NULL,
  ...
)

# S3 method for class 'grouped_df'
as_epi_df(x, ...)

# S3 method for class 'data.frame'
as_epi_df(
  x,
  as_of,
  other_keys = character(),
  signal_format = c("auto", "wide", "long"),
  signal_var = NULL,
  ...
)

# S3 method for class 'tbl_ts'
as_epi_df(
  x,
  as_of,
  other_keys = character(),
  signal_format = c("auto", "wide", "long"),
  signal_var = NULL,
  ...
)

is_epi_df(x)

new_epi_df(
  x = tibble::tibble(geo_value = character(), time_value = as.Date(integer())),
  geo_type,
  time_type,
  as_of,
  other_keys = character(),
  ...
)
```

## Arguments

- x:

  An object.

- ...:

  Additional arguments passed to methods.

- geo_type:

  **\[deprecated\]** in `as_epi_df()`, has no effect; the geo value type
  is inferred from the location column and set to "custom" if not
  recognized. In `new_epi_df()`, should be set to the same value that
  would be inferred.

- time_type:

  **\[deprecated\]** in `as_epi_df()`, has no effect: the time value
  type inferred from the time column and set to "custom" if not
  recognized. Unpredictable behavior may result if the time type is not
  recognized. In `new_epi_df()`, should be set to the same value that
  would be inferred.

- as_of:

  Time value representing the time at which the given data were
  available. For example, if `as_of` is January 31, 2022, then the
  `epi_df` object that is created would represent the most up-to-date
  version of the data available as of January 31, 2022. If the `as_of`
  argument is missing, then the current day-time will be used.

- other_keys:

  If your tibble has additional keys, be sure to specify them as a
  character vector here (typical examples are "age" or sub-geographies).

- signal_format:

  Format of the signal data. If `"auto"` (default), the function will
  try to detect if the data is in long format and pivot to wide if
  necessary. This happens only if a unique signal identifier column (see
  `signal_var`) and a `value` column are both present, and the signal
  column contains more than one unique value. `"long"` format treats the
  signal as a metadata key by adding it to `other_keys`. `"wide"` format
  tries to pivot to wide.

- signal_var:

  The name of the column that contains the signal identifiers. If
  `NULL`, the function will try to guess this column (see
  [`signal_column_names()`](https://cmu-delphi.github.io/epiprocess/dev/reference/signal_column_names.md)
  for a list of column names that will be checked).

## Value

- Of `as_epi_df()`: an (ungrouped) `epi_df`

&nbsp;

- Of `is_epi_df`: `TRUE` if the object inherits from `epi_df`, otherwise
  `FALSE`.

&nbsp;

- Of `new_epi_df()`: an `epi_df`

## Details

An `epi_df` is a kind of tibble with (at least) the following columns:

- `geo_value`: A character vector representing the geographical unit of
  observation. This could be a country code, a state name, a county
  code, etc.

- `time_value`: A date or integer vector representing the time of
  observation.

Other columns can be considered as measured variables, which we also
refer to as indicators or signals. An `epi_df` object also has metadata
with (at least) the following fields:

- `geo_type`: the type for the geo values.

- `time_type`: the type for the time values.

- `as_of`: the time value at which the given data were available.

Most users should use `as_epi_df`. The input tibble `x` to the
constructor must contain the columns `geo_value` and `time_value`. All
other columns will be preserved as is, and treated as measured
variables. If `as_of` is missing, then the function will try to guess it
from an `as_of`, `issue`, or `version` column of `x` (if any of these
are present), or from as an `as_of` field in its metadata (stored in its
attributes); if this fails, then the current day-time will be used. The
`new_epi_df` constructor assumes its arguments have already been
validated, so it should mainly be used by advanced users.

Metadata for an `epi_df` object `x` can be accessed (and altered) via
`attributes(x)$metadata`. The first field in the above list, `geo_type`,
can usually be inferred from the `geo_value` columns. They are not
currently used by any downstream functions in the `epiprocess` package,
and serve only as useful bits of information to convey about the data
set at hand. More information on their coding is given below.

The last field in the above list, `as_of`, is one of the most unique
aspects of an `epi_df` object. In brief, we can think of an `epi_df`
object as a single snapshot of a data set that contains the most
up-to-date values of the signals variables, as of the time specified in
the `as_of` field.

If an `epi_df` ever loses its `geo_value` or `time_value` columns, it
will decay into a regular tibble.

A companion object is the `epi_archive` object, which contains the full
version history of a given data set. Revisions are common in many types
of epidemiological data streams, and paying attention to data revisions
can be important for all sorts of downstream data analysis and modeling
tasks. See the documentation for
[`epi_archive`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_archive.md)
for more details on how data versioning works in the `epiprocess`
package (including how to generate `epi_df` objects, as data snapshots,
from an `epi_archive` object).

### Geo Types

The following geo types are recognized in an `epi_df`.

- `"county"`: each observation corresponds to a U.S. county; coded by
  5-digit FIPS code.

- `"hrr"`: each observation corresponds to a U.S. hospital referral
  region (designed to represent regional healthcare markets); there are
  306 HRRs in the U.S; coded by number (nonconsecutive, between 1 and
  457).

- `"state"`: each observation corresponds to a U.S. state; coded by
  2-digit postal abbreviation (lowercase); note that Puerto Rico is "pr"
  and Washington D.C. is "dc".

- `"hhs"`: each observation corresponds to a U.S. HHS region; coded by
  number (consecutive, between 1 and 10).

- `"nation"`: each observation corresponds to a country; coded by ISO
  31661- alpha-2 country codes (lowercase).

An unrecognizable geo type is labeled "custom".

### Time Types

The following time types are recognized in an `epi_df`.

- `"day"`: each observation corresponds to a day; coded as a `Date`
  object, as in `as.Date("2022-01-31")`.

- `"week"`: each observation corresponds to a week; the alignment can be
  arbitrary (as to whether a week starts on a Monday, Tuesday); coded as
  a `Date` object, representing the start date of week.

- `"yearmonth"`: each observation corresponds to a month; coded as a
  [`tsibble::yearmonth`](https://tsibble.tidyverts.org/reference/year-month.html)
  object.

- `"integer"`: a generic integer index (e.g. years or something else).

An unrecognizable time type is labeled "custom".

## Functions

- `as_epi_df()`: The preferred way of constructing `epi_df`s

- `new_epi_df()`: Lower-level constructor for `epi_df` object

## Examples

``` r
# Convert a `tsibble` that has county code as an extra key
# Notice that county code should be a character string to preserve any leading zeroes
ex1_input <- tibble::tibble(
  geo_value = c(
    "06059", "06061", "06067",
    "12111", "12113", "12117",
    "42101", "42103", "42105"
  ),
  state_name = rep(c("ca", "fl", "pa"), each = 3),
  time_value = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
    by = "day"
  ), length.out = length(geo_value)),
  value = 1:length(geo_value) + 0.01 * rnorm(length(geo_value))
) %>%
  tsibble::as_tsibble(index = time_value, key = c(geo_value, state_name))

# The `other_keys` metadata (`"state_name"` in this case) is automatically
# inferred from the `tsibble`'s `key`:
ex1 <- as_epi_df(x = ex1_input, as_of = "2020-06-03")
attr(ex1, "metadata")[["other_keys"]]
#> [1] "state_name"

# Dealing with misspecified column names:
# Geographical and temporal information must be provided in columns named
# `geo_value` and `time_value`; if we start from a data frame with a
# different format, it must be converted to use `geo_value` and `time_value`
# before calling `as_epi_df`.
ex2_input <- tibble::tibble(
  state = rep(c("ca", "fl", "pa"), each = 3), # misnamed
  pol = rep(c("blue", "swing", "swing"), each = 3), # extra key
  reported_date = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
    by = "day"
  ), length.out = length(state)), # misnamed
  value = 1:length(state) + 0.01 * rnorm(length(state))
)
print(ex2_input)
#> # A tibble: 9 × 4
#>   state pol   reported_date value
#>   <chr> <chr> <date>        <dbl>
#> 1 ca    blue  2020-06-01    0.987
#> 2 ca    blue  2020-06-02    2.01 
#> 3 ca    blue  2020-06-03    3.02 
#> 4 fl    swing 2020-06-01    4.00 
#> 5 fl    swing 2020-06-02    4.99 
#> 6 fl    swing 2020-06-03    6.00 
#> 7 pa    swing 2020-06-01    6.99 
#> 8 pa    swing 2020-06-02    7.98 
#> 9 pa    swing 2020-06-03    9.01 

ex2 <- ex2_input %>%
  dplyr::rename(geo_value = state, time_value = reported_date) %>%
  as_epi_df(
    as_of = "2020-06-03",
    other_keys = "pol"
  )
attr(ex2, "metadata")
#> $geo_type
#> [1] "state"
#> 
#> $time_type
#> [1] "day"
#> 
#> $as_of
#> [1] "2020-06-03"
#> 
#> $other_keys
#> [1] "pol"
#> 

# Adding additional keys to an `epi_df` object
ex3_input <- covid_incidence_county_subset %>%
  dplyr::filter(time_value > "2021-12-01", state_name == "Massachusetts") %>%
  dplyr::slice_tail(n = 6)

ex3 <- ex3_input %>%
  tsibble::as_tsibble() %>% # needed to add the additional metadata
  # add 2 extra keys
  dplyr::mutate(
    state = rep("MA", 6),
    pol = rep(c("blue", "swing", "swing"), each = 2)
  ) %>%
  as_epi_df(other_keys = c("state", "pol"))

attr(ex3, "metadata")
#> $geo_type
#> [1] "county"
#> 
#> $time_type
#> [1] "day"
#> 
#> $as_of
#> [1] "2026-05-27 00:51:46 UTC"
#> 
#> $other_keys
#> [1] "state" "pol"  
#> 

# Decays to a tibble
covid_incidence_county_subset %>%
  dplyr::select(-geo_value)
#> # A tibble: 16,212 × 4
#>    time_value cases county_name       state_name   
#>  * <date>     <dbl> <chr>             <chr>        
#>  1 2020-06-01     4 Barnstable County Massachusetts
#>  2 2020-06-01     0 Berkshire County  Massachusetts
#>  3 2020-06-01    78 Bristol County    Massachusetts
#>  4 2020-06-01     0 Dukes County      Massachusetts
#>  5 2020-06-01    92 Essex County      Massachusetts
#>  6 2020-06-01     0 Franklin County   Massachusetts
#>  7 2020-06-01    35 Hampden County    Massachusetts
#>  8 2020-06-01     4 Hampshire County  Massachusetts
#>  9 2020-06-01    98 Middlesex County  Massachusetts
#> 10 2020-06-01     0 Nantucket County  Massachusetts
#> # ℹ 16,202 more rows
```
