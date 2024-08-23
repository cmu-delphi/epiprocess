# epiprocess

Pre-1.0.0 numbering scheme: 0.x will indicate releases, while 0.x.y will indicate PR's.

# epiprocess 0.9

## Breaking changes

- `epi_slide` interface has major breaking changes.
  - All variables are now dot-prefixed to be more consistent with tidyverse
    style for functions that allow tidyeval.
  - The `before/after` arguments have been replaced with the `.window_size` and
    `.align` arguments. See documentation for how to translate.
  - `names_sep` has been removed. If you return data frames from your
    computations:
    - without a name, they will be unpacked into separate columns without name
      prefixes
    - with a name, it will become a packed data.frame-class column (see
      `tidyr::pack`).
  - `as_list_col` has been removed. You can now directly return a list from your
    slide computations instead. If you were using `as_list_col=TRUE`, you will
    need to wrap your output in a list.
- `epix_slide` interface has major changes.
  - `names_sep` has been removed. If you return data frames from your
    computations:
    - without a name, they will be unpacked into separate columns without name
      prefixes
    - with a name, it will become a packed data.frame-class column (see
      `tidyr::pack`).
  - `as_list_col` has been removed. You can now directly return a list from your
    slide computations instead. If you were using `as_list_col=TRUE`, you will
    need to wrap your output in a list.
- `as_epi_df()` or `as_epi_archive()` no longer accept `additional_metadata`.
  Use the new `other_keys` arg to specify additional key columns, such as age
  group columns or other demographic breakdowns. Miscellaneous metadata are no
  longer handled by `epiprocess`, but you can use R's built-in `attr<-` instead
  for a similar feature.

## Improvements

- Added `complete.epi_df`, which fills in missing values in an `epi_df` with
  `NA`s. Uses `tidyr::complete` underneath and preserves `epi_df` metadata.
- Inclusion of the function `revision_summary` to provide basic revision
  information for `epi_archive`s out of the box. (#492)

## Bug fixes

- Fix `epi_slide_opt` (and related functions) to correctly handle `before=Inf`.
  Also allow multiple columns specified as a list of strings.
- Disallow `after=Inf` in slide functions, since it doesn't seem like a likely
  use case and complicates code.

# epiprocess 0.8

## Breaking changes

- `epi_df`'s are now more strict about what types they allow in the time column.
  Namely, we are explicit about only supporting `Date` at the daily and weekly
  cadence and generic integer types (for yearly cadence).
- `epi_slide` `before` and `after` arguments are now require the user to
  specific time units in certain cases. The `time_step` argument has been
  removed.
- `epix_slide` `before` argument now defaults to `Inf`, and requires the user to
  specify units in some cases. The `time_step` argument has been removed.
- `detect_outlr_stl(seasonal_period = NULL)` is no longer accepted. Use
  `detect_outlr_stl(seasonal_period = <value>, seasonal_as_residual = TRUE)`
  instead. See `?detect_outlr_stl` for more details.

## Improvements

- `epi_slide` computations are now 2-4 times faster after changing how
  reference time values, made accessible within sliding functions, are
  calculated (#397).
- Add new `epi_slide_mean` function to allow much (~30x) faster rolling
  average computations in some cases (#400).
- Add new `epi_slide_sum` function to allow much faster rolling sum
  computations in some cases (#433).
- Add new `epi_slide_opt` function to allow much faster rolling computations
  in some cases, using `data.table` and `slider` optimized rolling functions
  (#433).
- Add tidyselect interface for `epi_slide_opt` and derivatives (#452).
- regenerated the `jhu_csse_daily_subset` dataset with the latest versions of
  the data from the API
- changed approach to versioning, see DEVELOPMENT.md for details
- `select` on grouped `epi_df`s now only drops `epi_df`ness if it makes sense; PR #390
- Minor documentation updates; PR #393
- Improved `epi_archive` print method. Compactified metadata and shows a snippet
  of the underlying `DT` (#341).
- Added `autoplot` method for `epi_df` objects, which creates a `ggplot2` plot of
  the `epi_df` (#382).
- Refactored internals to use `cli` for warnings/errors and `checkmate` for
  argument checking (#413).
- Fix logic to auto-assign `epi_df` `time_type` to `week` (#416) and `year`
  (#441).
- Clarified "Get started" example of getting Ebola line list data into `epi_df`
  format.
- Improved documentation web site landing page's introduction.
- Fixed documentation referring to old `epi_slide()` interface (#466, thanks
  @XuedaShen!).
- `as_epi_df` and `as_epi_archive` now support arguments to specify column names
  e.g. `as_epi_df(some_tibble, geo_value=state)`. In addition, there is a list
  of default conversions, see `time_column_names` for a list of columns that
  will automatically be recognized and converted to `time_value` column (there
  are similar functions for `geo` and `version`).
- Fixed bug where `epix_slide_ref_time_values_default()` on datetimes would
  output a huge number of `ref_time_values` spaced apart by mere seconds.
- In `epi_slide()` and `epix_slide()`:
  - Multiple "data-masking" tidy evaluation expressions can be passed in via
    `...`, rather than just one.
  - Additional tidy evaluation features from `dplyr::mutate` are supported: `!!
name_var := value`, unnamed expressions evaluating to data frames, and `=
NULL`; see `?epi_slide` for more details.

## Cleanup

- Resolved some linting messages in package checks (#468).
- Added optional `decay_to_tibble` attribute controlling `as_tibble()` behavior
  of `epi_df`s to let `{epipredict}` work more easily with other libraries (#471).
- Removed some external package dependencies.

# epiprocess 0.7.0

## Breaking changes:

- Switched `epi_df`'s `other_keys` default from `NULL` to `character(0)`; PR #390
- Refactored `epi_archive` to use S3 instead of R6 for its object model. The
  functionality stay the same, but it will break the member function interface.
  For migration, you can usually just convert `epi_archive$merge(...)` to
  `epi_archive <- epi_archive %>% epix_merge(...)` (and the same for
  `fill_through_version` and `truncate_after_version`) and
  `epi_archive$slide(...)` to `epi_archive %>% epix_slide(...)` (and the same
  for `as_of`, `group_by`, `slide`, etc.) (#340). In some limited situations,
  such as if you have a helper function that calls `epi_archive$merge` etc. on
  one of its arguments, then you may need to more carefully refactor them.

# epiprocess 0.7.0

## Improvements

- Updated vignettes for compatibility with epidatr 1.0.0 in PR #377.

## Breaking changes

- Changes to `epi_slide` and `epix_slide`:
  - If `f` is a function, it is now required to take at least three arguments.
    `f` must take an `epi_df` with the same column names as the archive's `DT`,
    minus the `version` column; followed by a one-row tibble containing the
    values of the grouping variables for the associated group; followed by a
    reference time value, usually as a `Date` object. Optionally, it can take
    any number of additional arguments after that, and forward values for those
    arguments through `epi[x]_slide`'s `...` args.
    - To make your existing slide computations work, add a third argument to
      your `f` function to accept this new input: e.g., change `f = function(x, g, <any other arguments>) { <body> }`
      to `f = function(x, g, rt, <any other arguments>) { <body> }`.

## New features

- `epi_slide` and `epix_slide` also make the window data, group key and
  reference time value available to slide computations specified as formulas or
  tidy evaluation expressions, in additional or completely new ways.
  - If `f` is a formula, it can now access the reference time value via `.z` or
    `.ref_time_value`.
  - If `f` is missing, the tidy evaluation expression in `...` can now refer to
    the window data as an `epi_df` or `tibble` with `.x`, the group key with
    `.group_key`, and the reference time value with `.ref_time_value`. The usual
    `.data` and `.env` pronouns also work, but`pick()` and `cur_data()` are not;
    work off of `.x` instead.
- `epix_slide` has been made more like `dplyr::group_modify`. It will no longer
  perform element/row recycling for size stability, accepts slide computation
  outputs containing any number of rows, and no longer supports `all_rows`.
  - To keep the old behavior, manually perform row recycling within `f`
    computations, and/or `left_join` a data frame representing the desired
    output structure with the current `epix_slide()` result to obtain the
    desired repetitions and completions expected with `all_rows = TRUE`.
- `epix_slide` will only output grouped or ungrouped tibbles. Previously, it
  would sometimes output `epi_df`s, but not consistently, and not always with
  the metadata desired. Future versions will revisit this design, and consider
  more closely whether/when/how to output an `epi_df`.
  - To keep the old behavior, convert the output of `epix_slide()` to `epi_df`
    when desired and set the metadata appropriately.

## Improvements

- `epi_slide` and `epix_slide` now support `as_list_col = TRUE` when the slide
  computations output atomic vectors, and output a list column in "chopped"
  format (see `tidyr::chop`).
- `epi_slide` now works properly with slide computations that output just a
  `Date` vector, rather than converting `slide_value` to a numeric column.
- Fix `?archive_cases_dv_subset` information regarding modifications of upstream
  data by @brookslogan in (#299).
- Update to use updated `epidatr` (`fetch_tbl` -> `fetch`) by @brookslogan in
  (#319).

# epiprocess 0.6.0

## Breaking changes

- Changes to both `epi_slide` and `epix_slide`:
  - The `n`, `align`, and `before` arguments have been replaced by new `before`
    and `after` arguments. To migrate to the new version, replace these
    arguments in every `epi_slide` and `epix_slide` call. If you were only using
    the `n` argument, then this means replacing `n = <n value>` with `before = <n value> - 1`.
    - `epi_slide`'s time windows now extend `before` time steps before and
      `after` time steps after the corresponding `ref_time_values`. See
      `?epi_slide` for details on matching old alignments.
    - `epix_slide`'s time windows now extend `before` time steps before the
      corresponding `ref_time_values` all the way through the latest data
      available at the corresponding `ref_time_values`.
  - Slide functions now keep any grouping of `x` in their results, like
    `mutate` and `group_modify`.
    - To obtain the old behavior, `dplyr::ungroup` the slide results immediately.
- Additional `epi_slide` changes:
  - When using `as_list_col = TRUE` together with `ref_time_values` and
    `all_rows=TRUE`, the marker for excluded computations is now a `NULL` entry
    in the list column, rather than a `NA`; if you are using `tidyr::unnest()`
    afterward and want to keep these missing data markers, you will need to
    replace the `NULL` entries with `NA`s. Skipped computations are now more
    uniformly detectable using `vctrs` methods.
- Additional`epix_slide` changes:
  - `epix_slide`'s `group_by` argument has been replaced by `dplyr::group_by` and
    `dplyr::ungroup` S3 methods. The `group_by` method uses "data masking" (also
    referred to as "tidy evaluation") rather than "tidy selection".
    - Old syntax:
      - `x %>% epix_slide(<other args>, group_by=c(col1, col2))`
      - `x %>% epix_slide(<other args>, group_by=all_of(colname_vector))`
    - New syntax:
      - `x %>% group_by(col1, col2) %>% epix_slide(<other args>)`
      - `x %>% group_by(across(all_of(colname_vector))) %>% epix_slide(<other args>)`
  - `epix_slide` no longer defaults to grouping by non-`time_value`, non-`version`
    key columns, instead considering all data to be in one big group.
    - To obtain the old behavior, precede each `epix_slide` call lacking a
      `group_by` argument with an appropriate `group_by` call.
  - `epix_slide` now guesses `ref_time_values` to be a regularly spaced sequence
    covering all the `DT$version` values and the `version_end`, rather than the
    distinct `DT$time_value`s. To obtain the old behavior, pass in
    `ref_time_values = unique(<ungrouped archive>$DT$time_value)`.
- `epi_archive`'s `clobberable_versions_start`'s default is now `NA`, so there
  will be no warnings by default about potential nonreproducibility. To obtain
  the old behavior, pass in `clobberable_versions_start = max_version_with_row_in(x)`.

## Potentially-breaking changes

- Fixed `[` on grouped `epi_df`s to maintain the grouping if possible when
  dropping the `epi_df` class (e.g., when removing the `time_value` column).
- Fixed `epi_df` operations to be more consistent about decaying into
  non-`epi_df`s when the result of the operation doesn't make sense as an
  `epi_df` (e.g., when removing the `time_value` column).
- Changed `bind_rows` on grouped `epi_df`s to not drop the `epi_df` class. Like
  with ungrouped `epi_df`s, the metadata of the result is still simply taken
  from the first result, and may be inappropriate
  ([#242](https://github.com/cmu-delphi/epiprocess/issues/242)).
- `epi_slide` and `epix_slide` now raise an error rather than silently filtering
  out `ref_time_values` that don't meet their expectations.

## New features

- `epix_slide`, `<epi_archive>$slide` have a new parameter `all_versions`. With
  `all_versions=TRUE`, `epix_slide` will pass a filtered `epi_archive` to each
  computation rather than an `epi_df` snapshot. This enables, e.g., performing
  pseudoprospective forecasts with a revision-aware forecaster using nested
  `epix_slide` operations.

## Improvements

- Added `dplyr::group_by` and `dplyr::ungroup` S3 methods for `epi_archive`
  objects, plus corresponding `$group_by` and `$ungroup` R6 methods. The
  `group_by` implementation supports the `.add` and `.drop` arguments, and
  `ungroup` supports partial ungrouping with `...`.
- `as_epi_archive`, `epi_archive$new` now perform checks for the key uniqueness
  requirement (part of
  [#154](https://github.com/cmu-delphi/epiprocess/issues/154)).

## Cleanup

- Added a `NEWS.md` file to track changes to the package.
- Implemented `?dplyr::dplyr_extending` for `epi_df`s
  ([#223](https://github.com/cmu-delphi/epiprocess/issues/223)).
- Fixed various small documentation issues ([#217](https://github.com/cmu-delphi/epiprocess/issues/217)).

# epiprocess 0.5.0

## Potentially-breaking changes

- `epix_slide`, `<epi_archive>$slide` now feed `f` an `epi_df` rather than
  converting to a tibble/`tbl_df` first, allowing use of `epi_df` methods and
  metadata, and often yielding `epi_df`s out of the slide as a result. To obtain
  the old behavior, convert to a tibble within `f`.

## Improvements

- Fixed `epix_merge`, `<epi_archive>$merge` always raising error on `sync="truncate"`.

## Cleanup

- Added `Remotes:` entry for `genlasso`, which was removed from CRAN.
- Added `as_epi_archive` tests.
- Added missing `epix_merge` test for `sync="truncate"`.

# epiprocess 0.4.0

## Potentially-breaking changes

- Fixed `[.epi_df` to not reorder columns, which was incompatible with
  downstream packages.
- Changed `[.epi_df` decay-to-tibble logic to more coherent with `epi_df`s
  current tolerance of nonunique keys: stopped decaying to a tibble in some
  cases where a unique key wouldn't have been preserved, since we don't
  enforce a unique key elsewhere.
- Fixed `[.epi_df` to adjust `"other_keys"` metadata when corresponding
  columns are selected out.
- Fixed `[.epi_df` to raise an error if resulting column names would be
  nonunique.
- Fixed `[.epi_df` to drop metadata if decaying to a tibble (due to removal
  of essential columns).

## Improvements

- Added check that `epi_df` `additional_metadata` is list.
- Fixed some incorrect `as_epi_df` examples.

## Cleanup

- Applied rename of upstream package in examples: `delphi.epidata` ->
  `epidatr`.
- Rounded out `[.epi_df` tests.

# epiprocess 0.3.0

## Breaking changes

- `as_epi_archive`, `epi_archive$new`:
  - Compactification (see below) by default may change results if working
    directly with the `epi_archive`'s `DT` field; to disable, pass in
    `compactify=FALSE`.
- `epi_archive`'s wrappers and R6 methods have been updated to follow these
  rules regarding reference semantics:
  - `epix_<method>` will not mutate input `epi_archive`s, but may alias them
    or alias their fields (which should not be a worry if a user sticks to
    these `epix_*` functions and "regular" R functions with
    copy-on-write-like behavior, avoiding mutating functions `[.data.table`).
  - `x$<method>` may mutate `x`; if it mutates `x`, it will return `x`
    invisibly (where this makes sense), and, for each of its fields, may
    either mutate the object to which it refers or reseat the reference (but
    not both); if `x$<method>` does not mutate `x`, its result may contain
    aliases to `x` or its fields.
- `epix_merge`, `<epi_archive>$merge`:
  - Removed `...`, `locf`, and `nan` parameters.
  - Changed the default behavior, which now corresponds to using
    `by=key(x$DT)` (but demanding that is the same set of column names as
    `key(y$DT)`), `all=TRUE`, `locf=TRUE`, `nan=NaN` (but with the
    post-filling step fixed to only apply to gaps, and no longer fill over
    `NA`s originating from `x$DT` and `y$DT`).
  - `x` and `y` are no longer allowed to share names of non-`by` columns.
  - `epix_merge` no longer mutates its `x` argument (but `$merge` continues
    to do so).
  - Removed (undocumented) capability of passing a `data.table` as `y`.
- `epix_slide`:
  - Removed inappropriate/misleading `n=7` default argument (due to
    reporting latency, `n=7` will _not_ yield 7 days of data in a typical
    daily-reporting surveillance data source, as one might have assumed).

## New features

- `as_epi_archive`, `epi_archive$new`:
  - New `compactify` parameter allows removal of rows that are redundant for the
    purposes of `epi_archive`'s methods, which use the last version of each
    observation carried forward.
  - New `clobberable_versions_start` field allows marking a range of versions
    that could be "clobbered" (rewritten without assigning new version
    tags); previously, this was hard-coded as `max(<epi_archive>$DT$version)`.
  - New `versions_end` field allows marking a range of versions beyond
    `max(<epi_archive>$DT$version)` that were observed, but contained no
    changes.
- `epix_merge`, `$merge`:
  - New `sync` parameter controls what to do if `x` and `y` aren't equally
    up to date (i.e., if `x$versions_end` and `y$versions_end` are
    different).
- New function `epix_fill_through_version`, method
  `<epi_archive>$fill_through_version`: non-mutating & mutating way to
  ensure that an archive contains versions at least through some
  `fill_versions_end`, extrapolating according to `how` if necessary.
- Example archive data object is now constructed on demand from its
  underlying data, so it will be based on the user's version of
  `epi_archive` rather than an outdated R6 implementation from whenever the
  data object was generated.

# epiprocess 0.2.0

## Breaking changes

- Removed default `n=7` argument to `epix_slide`.

## Improvements

- Ignore `NA`s when printing `time_value` range for an `epi_archive`.
- Fixed misleading column naming in `epix_slide` example.
- Trimmed down `epi_slide` examples.
- Synced out-of-date docs.

## Cleanup

- Removed dependency of some `epi_archive` tests on an example archive.
  object, and made them more understandable by reading without running.
- Fixed `epi_df` tests relying on an S3 method for `epi_df` implemented
  externally to `epiprocess`.
- Added tests for `epi_archive` methods and wrapper functions.
- Removed some dead code.
- Made `.{Rbuild,git}ignore` files more comprehensive.

# epiprocess 0.1.2

## New features

- New `new_epi_df` function is similar to `as_epi_df`, but (i) recalculates,
  overwrites, and/or drops most metadata of `x` if it has any, (ii) may
  still reorder the columns of `x` even if it's already an `epi_df`, and
  (iii) treats `x` as optional, constructing an empty `epi_df` by default.

## Improvements

- Fixed `geo_type` guessing on alphabetical strings with more than 2
  characters to yield `"custom"`, not US `"nation"`.
- Fixed `time_type` guessing to actually detect `Date`-class `time_value`s
  regularly spaced 7 days apart as `"week"`-type as intended.
- Improved printing of `epi_df`s, `epi_archives`s.
- Fixed `as_of` to not cut off any (forecast-like) data with `time_value >
max_version`.
- Expanded `epi_df` docs to include conversion from `tsibble`/`tbl_ts` objects,
  usage of `other_keys`, and pre-processing objects not following the
  `geo_value`, `time_value` naming scheme.
- Expanded `epi_slide` examples to show how to use an `f` argument with
  named parameters.
- Updated examples to print relevant columns given a common 80-column
  terminal width.
- Added growth rate examples.
- Improved `as_epi_archive` and `epi_archive$new`/`$initialize`
  documentation, including constructing a toy archive.

## Cleanup

- Added tests for `epi_slide`, `epi_cor`, and internal utility functions.
- Fixed currently-unused internal utility functions `MiddleL`, `MiddleR` to
  yield correct results on odd-length vectors.

# epiprocess 0.1.1

## New features

- New example data objects allow one to quickly experiment with `epi_df`s
  and `epi_archives` without relying/waiting on an API to fetch data.

## Improvements

- Improved `epi_slide` error messaging.
- Fixed description of the appropriate parameters for an `f` argument to
  `epi_slide`; previous description would give incorrect behavior if `f` had
  named parameters that did not receive values from `epi_slide`'s `...`.
- Added some examples throughout the package.
- Using example data objects in vignettes also speeds up vignette compilation.

## Cleanup

- Set up gh-actions CI.
- Added tests for `epi_df`s.

# epiprocess 0.1.0

## Implemented core functionality, vignettes

- Classes:
  - `epi_df`: specialized `tbl_df` for geotemporal epidemiological time
    series data, with optional metadata recording other key columns (e.g.,
    demographic breakdowns) and `as_of` what time/version this data was
    current/published. Associated functions:
    - `as_epi_df` converts to an `epi_df`, guessing the `geo_type`,
      `time_type`, `other_keys`, and `as_of` if not specified.
    - `as_epi_df.tbl_ts` and `as_tsibble.epi_df` automatically set
      `other_keys` and `key`&`index`, respectively.
    - `epi_slide` applies a user-supplied computation to a sliding/rolling
      time window and user-specified groups, adding the results as new
      columns, and recycling/broadcasting results to keep the result size
      stable. Allows computation to be provided as a function, `purrr`-style
      formula, or tidyeval dots. Uses `slider` underneath for efficiency.
    - `epi_cor` calculates Pearson, Kendall, or Spearman correlations
      between two (optionally time-shifted) variables in an `epi_df` within
      user-specified groups.
    - Convenience function: `is_epi_df`.
  - `epi_archive`: R6 class for version (patch) data for geotemporal
    epidemiological time series data sets. Comes with S3 methods and regular
    functions that wrap around this functionality for those unfamiliar with R6
    methods. Associated functions:
    - `as_epi_archive`: prepares an `epi_archive` object from a data frame
      containing snapshots and/or patch data for every available version of
      the data set.
    - `as_of`: extracts a snapshot of the data set as of some requested
      version, in `epi_df` format.
    - `epix_slide`, `<epi_archive>$slide`: similar to `epi_slide`, but for
      `epi_archive`s; for each requested `ref_time_value` and group, applies
      a time window and user-specified computation to a snapshot of the data
      as of `ref_time_value`.
    - `epix_merge`, `<epi_archive>$merge`: like `merge` for `epi_archive`s,
      but allowing for the last version of each observation to be carried
      forward to fill in gaps in `x` or `y`.
    - Convenience function: `is_epi_archive`.
- Additional functions:
  - `growth_rate`: estimates growth rate of a time series using one of a few
    built-in `method`s based on relative change, linear regression,
    smoothing splines, or trend filtering.
  - `detect_outlr`: applies one or more outlier detection methods to a given
    signal variable, and optionally aggregates the outputs to create a
    consensus result.
  - `detect_outlr_rm`: outlier detection function based on a
    rolling-median-based outlier detection function; one of the methods
    included in `detect_outlr`.
  - `detect_outlr_stl`: outlier detection function based on a seasonal-trend
    decomposition using LOESS (STL); one of the methods included in
    `detect_outlr`.
