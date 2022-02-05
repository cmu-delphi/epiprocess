# epiprocess

This package introduces a common data structure for epidemiological data sets
measured over space and time, and offers associated utilities to perform basic
signal processing tasks. See the getting started guide and vignettes for
examples.

## epi_df: snapshot of a data set

The first main data structure in the `epiprocess` package is called
[`epi_df`](reference/epi_df.html). This is simply a tibble with a couple of
required columns, `geo_value` and `time_value`. It can have any other number of
columns, which can be seen as measured variables, which we also call signal
variables. In brief, an `epi_df` object represents a snapshot of a data set that
contains the most up-to-date values of the signals variables, as of a given
time.

The functions in the `epiprocess` package that operate on `epi_df` objects all
begin with `epi`, namely:

- `epi_slide()`, for iteratively applying a custom computation to a variable in
  an `epi_df` object over sliding windows in time; 
  
- `epi_cor()`, for computing lagged correlations between variables in an
  `epi_df` object, (allowing for grouping by geo value, time value, or any other
  variables); 
  
- `epi_detect_outlr()`, for detecting and correcting outliers in a variable in
  an `epi_df` object, using either built-in or custom methodologies. 

Other notable functions include todo

## epi_archive: full version history of a data set

The second main data structure in the package is called
[`epi_archive`](reference/epi_archive.html). This is a special class wrapped
around a data table that stores the archive (version history) of some signal
variables of interest. 

An `epi_archive` object can be used to generate a snapshot of the associated
data set in `epi_df` format, which represents the most up-to-date values of the
signal variables, as of the specified version. This is accomplished by calling
the `as_of()` method for an `epi_archive` object `x`, for example:
```
x$as_of(as.Date("2022-01-15"))
```
to generate an `epi_df` object containing a data snapshot as of January
15, 2022.

Importantly, sliding computations can also be done over a data archive. This is
accomplished by calling the `slide()` method for an `epi_archive` object.  This
works similarly to the way `epi_slide()` works for an `epi_df` object, but with
one key difference: for an `epi_archive` object, the sliding computation at any
given reference time point t is performed on the **data that would have been
available as of t**.
