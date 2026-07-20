# `group_by` and related methods for `epi_archive`, `grouped_epi_archive`

`group_by` and related methods for `epi_archive`, `grouped_epi_archive`

## Usage

``` r
# S3 method for class 'epi_archive'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))

# S3 method for class 'grouped_epi_archive'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))

# S3 method for class 'grouped_epi_archive'
group_by_drop_default(.tbl)

# S3 method for class 'grouped_epi_archive'
group_vars(x)

# S3 method for class 'grouped_epi_archive'
groups(x)

# S3 method for class 'grouped_epi_archive'
ungroup(x, ...)

is_grouped_epi_archive(x)
```

## Arguments

- .data:

  An `epi_archive` or `grouped_epi_archive`

- ...:

  Similar to
  [`dplyr::group_by`](https://dplyr.tidyverse.org/reference/group_by.html)
  (see "Details:" for edge cases);

  - For `group_by`: unquoted variable name(s) or other ["data
    masking"](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)
    expression(s). It's possible to use
    [`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)-like
    syntax here to calculate new columns on which to perform grouping,
    but note that, if you are regrouping an already-grouped `.data`
    object, the calculations will be carried out ignoring such grouping
    (same as [in
    dplyr](https://dplyr.tidyverse.org/reference/group_by.html)).

  - For `ungroup`: either

    - empty, in order to remove the grouping and output an
      `epi_archive`; or

    - variable name(s) or other
      ["tidy-select"](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
      expression(s), in order to remove the matching variables from the
      list of grouping variables, and output another
      `grouped_epi_archive`.

- .add:

  Boolean. If `FALSE`, the default, the output will be grouped by the
  variable selection from `...` only; if `TRUE`, the output will be
  grouped by the current grouping variables plus the variable selection
  from `...`.

- .drop:

  As described in
  [`dplyr::group_by`](https://dplyr.tidyverse.org/reference/group_by.html);
  determines treatment of factor columns.

- .tbl:

  A `grouped_epi_archive` object.

- x:

  For `groups`, `group_vars`, or `ungroup`: a `grouped_epi_archive`; for
  `is_grouped_epi_archive`: any object

## Details

To match `dplyr`, `group_by` allows "data masking" (also referred to as
"tidy evaluation") expressions `...`, not just column names, in a way
similar to `mutate`. Note that replacing or removing key columns with
these expressions is disabled.

`archive %>% group_by()` and other expressions that group or regroup by
zero columns (indicating that all rows should be treated as part of one
large group) will output a `grouped_epi_archive`, in order to enable the
use of `grouped_epi_archive` methods on the result. This is in slight
contrast to the same operations on tibbles and grouped tibbles, which
will *not* output a `grouped_df` in these circumstances.

Using `group_by` with `.add=FALSE` to override the existing grouping is
disabled; instead, `ungroup` first then `group_by`.

`group_by_drop_default` on (ungrouped) `epi_archive`s is expected to
dispatch to `group_by_drop_default.default` (but there is a dedicated
method for `grouped_epi_archive`s).

## Examples

``` r

grouped_archive <- archive_cases_dv_subset %>% group_by(geo_value)

# `print` for metadata and method listing:
grouped_archive %>% print()
#> A `grouped_epi_archive` object:
#> * Groups: geo_value
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Time range:    2020-06-01 -- 2021-11-30 (times are days)
#> ℹ Version range: 2020-06-02 -- 2021-12-01
#> ℹ A preview of the table (129638 rows x 5 columns):
#> Key: <geo_value, time_value, version>
#>         geo_value time_value    version percent_cli case_rate_7d_av
#>            <char>     <Date>     <Date>       <num>           <num>
#>      1:        ca 2020-06-01 2020-06-02          NA        6.628329
#>      2:        ca 2020-06-01 2020-06-06    2.140116        6.628329
#>      3:        ca 2020-06-01 2020-06-07    2.140116        6.628329
#>      4:        ca 2020-06-01 2020-06-08    2.140379        6.628329
#>      5:        ca 2020-06-01 2020-06-09    2.114430        6.628329
#>     ---                                                            
#> 129634:        tx 2021-11-26 2021-11-29    1.858596        7.957657
#> 129635:        tx 2021-11-27 2021-11-28          NA        7.174299
#> 129636:        tx 2021-11-28 2021-11-29          NA        6.834681
#> 129637:        tx 2021-11-29 2021-11-30          NA        8.841247
#> 129638:        tx 2021-11-30 2021-12-01          NA        9.566218

# The primary use for grouping is to perform a grouped `epix_slide`:

archive_cases_dv_subset %>%
  group_by(geo_value) %>%
  epix_slide(
    .f = ~ mean(.x$case_rate_7d_av),
    .before = 2,
    .versions = as.Date("2020-06-11") + 0:2,
    .new_col_name = "case_rate_3d_av"
  ) %>%
  ungroup()
#> # A tibble: 12 × 3
#>    geo_value version    case_rate_3d_av
#>    <chr>     <date>               <dbl>
#>  1 ca        2020-06-11            7.19
#>  2 fl        2020-06-11            5.71
#>  3 ny        2020-06-11            4.59
#>  4 tx        2020-06-11            5.62
#>  5 ca        2020-06-12            7.52
#>  6 fl        2020-06-12            5.82
#>  7 ny        2020-06-12            4.34
#>  8 tx        2020-06-12            5.91
#>  9 ca        2020-06-13            7.62
#> 10 fl        2020-06-13            6.11
#> 11 ny        2020-06-13            4.14
#> 12 tx        2020-06-13            6.03

# -----------------------------------------------------------------

# Advanced: some other features of dplyr grouping are implemented:

library(dplyr)
toy_archive <-
  tribble(
    ~geo_value, ~age_group, ~time_value, ~version, ~value,
    "us", "adult", "2000-01-01", "2000-01-02", 121,
    "us", "pediatric", "2000-01-02", "2000-01-03", 5, # (addition)
    "us", "adult", "2000-01-01", "2000-01-03", 125, # (revision)
    "us", "adult", "2000-01-02", "2000-01-03", 130 # (addition)
  ) %>%
  mutate(
    age_group = ordered(age_group, c("pediatric", "adult")),
    time_value = as.Date(time_value),
    version = as.Date(version)
  ) %>%
  as_epi_archive(other_keys = "age_group")

# The following are equivalent:
toy_archive %>% group_by(geo_value, age_group)
#> A `grouped_epi_archive` object:
#> * Groups: geo_value, age_group
#> * Drops groups formed by factor levels that don't appear in the data
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Other DT keys: age_group
#> ℹ Time range:    2000-01-01 -- 2000-01-02 (times are days)
#> ℹ Version range: 2000-01-02 -- 2000-01-03
#> ℹ A preview of the table (4 rows x 5 columns):
#> Key: <geo_value, time_value, age_group, version>
#>    geo_value age_group time_value    version value
#>       <char>     <ord>     <Date>     <Date> <num>
#> 1:        us     adult 2000-01-01 2000-01-02   121
#> 2:        us     adult 2000-01-01 2000-01-03   125
#> 3:        us pediatric 2000-01-02 2000-01-03     5
#> 4:        us     adult 2000-01-02 2000-01-03   130
toy_archive %>%
  group_by(geo_value) %>%
  group_by(age_group, .add = TRUE)
#> A `grouped_epi_archive` object:
#> * Groups: geo_value, age_group
#> * Drops groups formed by factor levels that don't appear in the data
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Other DT keys: age_group
#> ℹ Time range:    2000-01-01 -- 2000-01-02 (times are days)
#> ℹ Version range: 2000-01-02 -- 2000-01-03
#> ℹ A preview of the table (4 rows x 5 columns):
#> Key: <geo_value, time_value, age_group, version>
#>    geo_value age_group time_value    version value
#>       <char>     <ord>     <Date>     <Date> <num>
#> 1:        us     adult 2000-01-01 2000-01-02   121
#> 2:        us     adult 2000-01-01 2000-01-03   125
#> 3:        us pediatric 2000-01-02 2000-01-03     5
#> 4:        us     adult 2000-01-02 2000-01-03   130
grouping_cols <- c("geo_value", "age_group")
toy_archive %>% group_by(across(all_of(grouping_cols)))
#> A `grouped_epi_archive` object:
#> * Groups: geo_value, age_group
#> * Drops groups formed by factor levels that don't appear in the data
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Other DT keys: age_group
#> ℹ Time range:    2000-01-01 -- 2000-01-02 (times are days)
#> ℹ Version range: 2000-01-02 -- 2000-01-03
#> ℹ A preview of the table (4 rows x 5 columns):
#> Key: <geo_value, time_value, age_group, version>
#>    geo_value age_group time_value    version value
#>       <char>     <ord>     <Date>     <Date> <num>
#> 1:        us     adult 2000-01-01 2000-01-02   121
#> 2:        us     adult 2000-01-01 2000-01-03   125
#> 3:        us pediatric 2000-01-02 2000-01-03     5
#> 4:        us     adult 2000-01-02 2000-01-03   130

# And these are equivalent:
toy_archive %>% group_by(geo_value)
#> A `grouped_epi_archive` object:
#> * Groups: geo_value
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Other DT keys: age_group
#> ℹ Time range:    2000-01-01 -- 2000-01-02 (times are days)
#> ℹ Version range: 2000-01-02 -- 2000-01-03
#> ℹ A preview of the table (4 rows x 5 columns):
#> Key: <geo_value, time_value, age_group, version>
#>    geo_value age_group time_value    version value
#>       <char>     <ord>     <Date>     <Date> <num>
#> 1:        us     adult 2000-01-01 2000-01-02   121
#> 2:        us     adult 2000-01-01 2000-01-03   125
#> 3:        us pediatric 2000-01-02 2000-01-03     5
#> 4:        us     adult 2000-01-02 2000-01-03   130
toy_archive %>%
  group_by(geo_value, age_group) %>%
  ungroup(age_group)
#> A `grouped_epi_archive` object:
#> * Groups: geo_value
#> It wraps an ungrouped `epi_archive`, with metadata:
#> ℹ Other DT keys: age_group
#> ℹ Time range:    2000-01-01 -- 2000-01-02 (times are days)
#> ℹ Version range: 2000-01-02 -- 2000-01-03
#> ℹ A preview of the table (4 rows x 5 columns):
#> Key: <geo_value, time_value, age_group, version>
#>    geo_value age_group time_value    version value
#>       <char>     <ord>     <Date>     <Date> <num>
#> 1:        us     adult 2000-01-01 2000-01-02   121
#> 2:        us     adult 2000-01-01 2000-01-03   125
#> 3:        us pediatric 2000-01-02 2000-01-03     5
#> 4:        us     adult 2000-01-02 2000-01-03   130

# To get the grouping variable names as a character vector:
toy_archive %>%
  group_by(geo_value) %>%
  group_vars()
#> [1] "geo_value"

# To get the grouping variable names as a `list` of `name`s (a.k.a. symbols):
toy_archive %>%
  group_by(geo_value) %>%
  groups()
#> [[1]]
#> geo_value
#> 

toy_archive %>%
  group_by(geo_value, age_group, .drop = FALSE) %>%
  epix_slide(.f = ~ sum(.x$value), .before = 20) %>%
  ungroup()
#> # A tibble: 4 × 4
#>   geo_value age_group version    slide_value
#>   <chr>     <ord>     <date>           <dbl>
#> 1 us        pediatric 2000-01-02           0
#> 2 us        adult     2000-01-02         121
#> 3 us        pediatric 2000-01-03           5
#> 4 us        adult     2000-01-03         255
```
