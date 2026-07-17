# Take each requested (group and) version in an archive, run a computation (e.g., forecast)

... and collect the results. This is useful for more accurately
simulating how a forecaster, nowcaster, or other algorithm would have
behaved in real time, factoring in reporting latency and data revisions;
see
[`vignette("backtesting", package="epipredict")`](https://cmu-delphi.github.io/epipredict/articles/backtesting.html)
for a walkthrough.

## Usage

``` r
epix_slide(
  .x,
  .f,
  ...,
  .before = Inf,
  .versions = NULL,
  .new_col_name = NULL,
  .all_versions = FALSE
)
```

## Arguments

- .x:

  An
  [`epi_archive`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_archive.md)
  or
  [`grouped_epi_archive`](https://cmu-delphi.github.io/epiprocess/dev/reference/group_by.epi_archive.md)
  object. If ungrouped, all data in `x` will be treated as part of a
  single data group.

- .f:

  Function, formula, or missing; together with `...` specifies the
  computation. The computation will be run on each requested
  group-version combination, with a time window filter applied if
  `.before` is supplied.

  If `.f` is a function must have the form `function(x, g, v)` or
  `function(x, g, v, <additional configuration args>)`, where

      - `x` is an `epi_df` with the same column names as the archive's `DT`,
        minus the `version` column. (Or, if `.all_versions = TRUE`, an
        `epi_archive` with the requested partial version history.)

      - `g` is a one-row tibble containing the values of the grouping variables
        for the associated group.

      - `v` (length-1) is the associated `version` (one of the requested
        `.versions`)

      - `<additional configuration args>` are optional; you can add such
        arguments to your function and set them by passing them through the
        `...` argument to `epix_slide()`.

  If a formula, `.f` can operate directly on columns accessed via
  `.x$var` or `.$var`, as in `~ mean (.x$var)` to compute a mean of a
  column `var` for each group-`ref_time_value` combination. The group
  key can be accessed via `.y` or `.group_key`, and the reference time
  value can be accessed via `.z`, `.version`, or `.ref_time_value`. If
  `.f` is missing, then `...` will specify the computation.

- ...:

  Additional arguments to pass to the function or formula specified via
  `f`. Alternatively, if `.f` is missing, then the `...` is interpreted
  as a
  ["data-masking"](https://rlang.r-lib.org/reference/args_data_masking.html)
  expression or expressions for tidy evaluation; in addition to
  referring columns directly by name, the expressions have access to
  `.data` and `.env` pronouns as in `dplyr` verbs, and can also refer to
  `.x` (not the same as the input epi_archive), `.group_key` and
  `.version`/`.ref_time_value`. See details for more.

- .before:

  Optional; applies a `time_value` filter before running each
  computation. The default is not to apply a `time_value` filter. If
  provided, it should be a single integer or difftime that is compatible
  with the time_type of the time_value column. If an integer, then the
  minimum possible `time_value` included will be that many time steps
  (according to the `time_type`) before each requested `.version`. This
  window endpoint is inclusive. For example, if `.before = 14`, the
  `time_type` in the archive is "day", and the requested `.version` is
  January 15, then the smallest possible `time_value` possible in the
  snapshot will be January 1. Note that this does not mean that there
  will be 14 or 15 distinct `time_value`s actually appearing in the
  data; for most reporting streams, reporting as of January 15 won't
  include `time_value`s all the way through January 14, due to reporting
  latency. Unlike
  [`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md),
  `epix_slide()` won't fill in any missing `time_values` in this window.

- .versions:

  Requested versions on which to run the computation. Each requested
  `.version` also serves as the anchor point from which the `time_value`
  window specified by `.before` is drawn. If `.versions` is missing, it
  will be set to a regularly-spaced sequence of values set to cover the
  range of `version`s in the `DT` plus the `versions_end`; the spacing
  of values will be guessed (using the GCD of the skips between values).

- .new_col_name:

  Either `NULL` or a string indicating the name of the new column that
  will contain the derived values. The default, `NULL`, will use the
  name "slide_value" unless your slide computations output data frames,
  in which case they will be unpacked into the constituent columns and
  the data frame's column names will be used instead. If the resulting
  column name(s) overlap with the column names used for labeling the
  computations, which are `group_vars(x)` and `"version"`, then the
  values for these columns must be identical to the labels we assign.

- .all_versions:

  (Not the same as `.all_rows` parameter of `epi_slide`.) If
  `.all_versions = TRUE`, then the slide computation will be passed the
  version history (all versions `<= .version` where `.version` is one of
  the requested `.version`s), in `epi_archive` format. Otherwise, the
  slide computation will be passed only the most recent `version` for
  every unique `time_value`, in `epi_df` format. Default is `FALSE`.

## Value

A tibble whose columns are: the grouping variables (if any),
`time_value`, containing the reference time values for the slide
computation, and a column named according to the `.new_col_name`
argument, containing the slide values. It will be grouped by the
grouping variables.

## Details

This is similar to looping over versions and calling
[`epix_as_of`](https://cmu-delphi.github.io/epiprocess/dev/reference/epix_as_of.md),
but has some conveniences such as working naturally with
[`grouped_epi_archive`](https://cmu-delphi.github.io/epiprocess/dev/reference/group_by.epi_archive.md)s,
optional time windowing, and syntactic sugar to make things shorter to
write.

A few key distinctions between the current function and
[`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md):

1.  In `.f` functions for `epix_slide`, one should not assume that the
    input data to contain any rows with `time_value` matching the
    computation's `.version`, due to reporting latency; for typical
    epidemiological surveillance data, observations pertaining to a
    particular time period (`time_value`) are first reported `as_of`
    some instant after that time period has ended. No time window
    completion is performed as in
    [`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md).

2.  The input class and columns are similar but different: `epix_slide`
    (with the default `.all_versions=FALSE`) keeps all columns and the
    `epi_df`-ness of the first argument to each computation; `epi_slide`
    only provides the grouping variables in the second input, and will
    convert the first input into a regular tibble if the grouping
    variables include the essential `geo_value` column. (With
    `.all_versions=TRUE`, `epix_slide` will provide an `epi_archive`
    rather than an `epi-df` to each computation.)

3.  The output class and columns are similar but different:
    `epix_slide()` returns a tibble containing only the grouping
    variables, `time_value`, and the new column(s) from the slide
    computations, whereas
    [`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
    returns an `epi_df` with all original variables plus the new columns
    from the slide computations. (Both will mirror the grouping or
    ungroupedness of their input, with one exception: `epi_archive`s can
    have trivial (zero-variable) groupings, but these will be dropped in
    `epix_slide` results as they are not supported by tibbles.)

4.  There are no size stability checks or element/row recycling to
    maintain size stability in `epix_slide`, unlike in `epi_slide`.
    (`epix_slide` is roughly analogous to
    [`dplyr::group_modify`](https://dplyr.tidyverse.org/reference/group_map.html),
    while `epi_slide` is roughly analogous to
    [`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html).)

5.  `.all_rows` is not supported in `epix_slide`; since the slide
    computations are allowed more flexibility in their outputs than in
    `epi_slide`, we can't guess a good representation for missing
    computations for excluded group-`.ref_time_value` pairs.

6.  The `.versions` default for `epix_slide` is based on making an
    evenly-spaced sequence out of the `version`s in the `DT` plus the
    `versions_end`, rather than all unique `time_value`s.

7.  `epix_slide()` computations can refer to the current element of
    `.versions` as either `.version` or `.ref_time_value`, while
    [`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
    computations refer to the current element of `.ref_time_values` with
    `.ref_time_value`.

Apart from the above distinctions, the interfaces between `epix_slide()`
and
[`epi_slide()`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide.md)
are the same.

## Examples

``` r
library(dplyr)

# Request only a small set of versions, for example's sake:
requested_versions <-
  seq(as.Date("2020-09-02"), as.Date("2020-09-15"), by = "1 day")

# Investigate reporting lag of `percent_cli` signal (though normally we'd
# probably work off of the dedicated `revision_summary()` function instead):
archive_cases_dv_subset %>%
  epix_slide(
    geowide_percent_cli_max_time = max(time_value[!is.na(percent_cli)]),
    geowide_percent_cli_rpt_lag = .version - geowide_percent_cli_max_time,
    .versions = requested_versions
  )
#> # A tibble: 14 × 3
#>    version    geowide_percent_cli_max_time geowide_percent_cli_rpt_lag
#>  * <date>     <date>                       <drtn>                     
#>  1 2020-09-02 2020-08-30                   3 days                     
#>  2 2020-09-03 2020-08-31                   3 days                     
#>  3 2020-09-04 2020-09-01                   3 days                     
#>  4 2020-09-05 2020-09-02                   3 days                     
#>  5 2020-09-06 2020-09-03                   3 days                     
#>  6 2020-09-07 2020-09-04                   3 days                     
#>  7 2020-09-08 2020-09-05                   3 days                     
#>  8 2020-09-09 2020-09-06                   3 days                     
#>  9 2020-09-10 2020-09-07                   3 days                     
#> 10 2020-09-11 2020-09-08                   3 days                     
#> 11 2020-09-12 2020-09-09                   3 days                     
#> 12 2020-09-13 2020-09-10                   3 days                     
#> 13 2020-09-14 2020-09-11                   3 days                     
#> 14 2020-09-15 2020-09-12                   3 days                     
archive_cases_dv_subset %>%
  group_by(geo_value) %>%
  epix_slide(
    percent_cli_max_time = max(time_value[!is.na(percent_cli)]),
    percent_cli_rpt_lag = .version - percent_cli_max_time,
    .versions = requested_versions
  )
#> # A tibble: 56 × 4
#> # Groups:   geo_value [4]
#>    geo_value version    percent_cli_max_time percent_cli_rpt_lag
#>  * <chr>     <date>     <date>               <drtn>             
#>  1 ca        2020-09-02 2020-08-30           3 days             
#>  2 fl        2020-09-02 2020-08-30           3 days             
#>  3 ny        2020-09-02 2020-08-30           3 days             
#>  4 tx        2020-09-02 2020-08-30           3 days             
#>  5 ca        2020-09-03 2020-08-31           3 days             
#>  6 fl        2020-09-03 2020-08-31           3 days             
#>  7 ny        2020-09-03 2020-08-31           3 days             
#>  8 tx        2020-09-03 2020-08-31           3 days             
#>  9 ca        2020-09-04 2020-09-01           3 days             
#> 10 fl        2020-09-04 2020-09-01           3 days             
#> # ℹ 46 more rows

# Backtest a forecaster "pseudoprospectively" (i.e., faithfully with respect
# to the data version history):
case_death_rate_archive %>%
  epix_slide(
    .versions = as.Date(c("2021-10-01", "2021-10-08")),
    function(x, g, v) {
      epipredict::arx_forecaster(
        x,
        outcome = "death_rate",
        predictors = c("death_rate_7d_av", "case_rate_7d_av")
      )$predictions
    }
  )
#> Registered S3 methods overwritten by 'epipredict':
#>   method                          from   
#>   print.step_naomit               recipes
#>   vec_arith.quantile_pred         hardhat
#>   vec_arith.numeric.quantile_pred hardhat
#>   vec_math.quantile_pred          hardhat
#> # A tibble: 112 × 6
#>    version    geo_value  .pred .pred_distn forecast_date target_date
#>  * <date>     <chr>      <dbl>   <qtls(7)> <date>        <date>     
#>  1 2021-10-01 ak        1.95        [1.95] 2021-09-30    2021-10-07 
#>  2 2021-10-01 al        1.36        [1.36] 2021-09-30    2021-10-07 
#>  3 2021-10-01 ar        0.572      [0.572] 2021-09-30    2021-10-07 
#>  4 2021-10-01 as        0.0128    [0.0128] 2021-09-30    2021-10-07 
#>  5 2021-10-01 az        0.537      [0.537] 2021-09-30    2021-10-07 
#>  6 2021-10-01 ca        0.260       [0.26] 2021-09-30    2021-10-07 
#>  7 2021-10-01 co        0.308      [0.308] 2021-09-30    2021-10-07 
#>  8 2021-10-01 ct        0.406      [0.406] 2021-09-30    2021-10-07 
#>  9 2021-10-01 dc        0.147      [0.147] 2021-09-30    2021-10-07 
#> 10 2021-10-01 de        0.382      [0.382] 2021-09-30    2021-10-07 
#> # ℹ 102 more rows
# See `vignette("backtesting", package="epipredict")` for a full walkthrough
# on backtesting forecasters, including plots, etc.

# --- Advanced: ---

# `epix_slide` with `all_versions=FALSE` (the default) applies a
# version-unaware computation to several versions of the data. We can also
# use `.all_versions=TRUE` to apply a version-*aware* computation to several
# versions of the data, again looking at characteristics of the data passed
# to each computation. In this case, each computation should expect an
# `epi_archive` containing the relevant version data:

archive_cases_dv_subset %>%
  group_by(geo_value) %>%
  epix_slide(
    function(x, gk, rtv) {
      tibble(
        versions_start = if (nrow(x$DT) == 0L) {
          "NA (0 rows)"
        } else {
          toString(min(x$DT$version))
        },
        versions_end = x$versions_end,
        time_range = if (nrow(x$DT) == 0L) {
          "0 `time_value`s"
        } else {
          sprintf("%s -- %s", min(x$DT$time_value), max(x$DT$time_value))
        },
        n = nrow(x$DT),
        class1 = class(x)[[1L]]
      )
    },
    .before = 5, .all_versions = TRUE,
    .versions = requested_versions
  ) %>%
  ungroup() %>%
  # Focus on one geo_value so we can better see the columns above:
  filter(geo_value == "ca") %>%
  select(-geo_value)
#> # A tibble: 14 × 6
#>    version    versions_start versions_end time_range                   n class1 
#>    <date>     <chr>          <date>       <chr>                    <int> <chr>  
#>  1 2020-09-02 2020-08-29     2020-09-02   2020-08-28 -- 2020-09-01    11 epi_ar…
#>  2 2020-09-03 2020-08-30     2020-09-03   2020-08-29 -- 2020-09-02    11 epi_ar…
#>  3 2020-09-04 2020-08-31     2020-09-04   2020-08-30 -- 2020-09-03    11 epi_ar…
#>  4 2020-09-05 2020-09-01     2020-09-05   2020-08-31 -- 2020-09-04    11 epi_ar…
#>  5 2020-09-06 2020-09-02     2020-09-06   2020-09-01 -- 2020-09-05    11 epi_ar…
#>  6 2020-09-07 2020-09-03     2020-09-07   2020-09-02 -- 2020-09-06    11 epi_ar…
#>  7 2020-09-08 2020-09-04     2020-09-08   2020-09-03 -- 2020-09-07    11 epi_ar…
#>  8 2020-09-09 2020-09-05     2020-09-09   2020-09-04 -- 2020-09-08    11 epi_ar…
#>  9 2020-09-10 2020-09-06     2020-09-10   2020-09-05 -- 2020-09-09    11 epi_ar…
#> 10 2020-09-11 2020-09-07     2020-09-11   2020-09-06 -- 2020-09-10    11 epi_ar…
#> 11 2020-09-12 2020-09-08     2020-09-12   2020-09-07 -- 2020-09-11    11 epi_ar…
#> 12 2020-09-13 2020-09-09     2020-09-13   2020-09-08 -- 2020-09-12    11 epi_ar…
#> 13 2020-09-14 2020-09-10     2020-09-14   2020-09-09 -- 2020-09-13    11 epi_ar…
#> 14 2020-09-15 2020-09-11     2020-09-15   2020-09-10 -- 2020-09-14    11 epi_ar…
```
