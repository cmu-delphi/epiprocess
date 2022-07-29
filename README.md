# epiprocess
 
 <!-- badges: start -->
  [![R-CMD-check](https://github.com/cmu-delphi/epiprocess/workflows/R-CMD-check/badge.svg)](https://github.com/cmu-delphi/epiprocess/actions)
  <!-- badges: end -->

This package introduces a common data structure for epidemiological data sets
measured over space and time, and offers associated utilities to perform basic
signal processing tasks. See the getting started guide and vignettes for
examples.

## `epi_df`: snapshot of a data set

The first main data structure in the `epiprocess` package is called
[`epi_df`](reference/epi_df.html). This is simply a tibble with a couple of
required columns, `geo_value` and `time_value`. It can have any other number of
columns, which can be seen as measured variables, which we also call signal
variables. In brief, an `epi_df` object represents a snapshot of a data set that
contains the most up-to-date values of the signals variables, as of a given
time.

By convention, functions in the `epiprocess` package that operate on `epi_df`
objects begin with `epi`. For example: 

- `epi_slide()`, for iteratively applying a custom computation to a variable in
  an `epi_df` object over sliding windows in time;
  
- `epi_cor()`, for computing lagged correlations between variables in an
  `epi_df` object, (allowing for grouping by geo value, time value, or any other
  variables).

Functions in the package that operate directly on given variables do not begin
  with `epi`. For example: 

- `growth_rate()`, for estimating the growth rate of a given signal at given
  time values, using various methodologies;

- `detect_outlr()`, for detecting outliers in a given signal over time, using
  either built-in or custom methodologies.

## `epi_archive`: full version history of a data set

The second main data structure in the package is called
[`epi_archive`](reference/epi_archive.html). This is a special class (R6 format) 
wrapped around a data table that stores the archive (version history) of some
signal variables of interest.

By convention, functions in the `epiprocess` package that operate on `epi_df`
objects begin with `epix` (the "x" is meant to remind you of "archive"). These
are just wrapper functions around the public methods for the `epi_archive` R6
class. For example:

- `epix_as_of()`, for generating a snapshot in `epi_df` from the data archive,
  which represents the most up-to-date values of the signal variables, as of the
  specified version;
  
- `epix_fill_through_version()`, for filling in some fake version data following
  simple rules, for use when downstream methods expect an archive that is more
  up-to-date (e.g., if it is a forecasting deadline date and one of our data
  sources cannot be accessed to provide the latest versions of its data)

- `epix_merge()`, for merging two data archives with each other, with support
  for various approaches to handling when one of the archives is more up-to-date
  version-wise than the other;

- `epix_slide()`, for sliding a custom computation to a data archive over local
  windows in time, much like `epi_slide` for an `epi_df` object, but with one
  key difference: the sliding computation at any given reference time t is
  performed only on the **data that would have been available as of t**.
