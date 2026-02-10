# Filter an `epi_archive` object to keep only older versions

Generates a filtered `epi_archive` from an `epi_archive` object, keeping
only rows with `version` falling on or before a specified date.

## Usage

``` r
epix_truncate_versions_after(x, max_version)

# S3 method for class 'epi_archive'
epix_truncate_versions_after(x, max_version)

# S3 method for class 'grouped_epi_archive'
epix_truncate_versions_after(x, max_version)
```

## Arguments

- x:

  An `epi_archive` object.

- max_version:

  The latest version to include in the archive.

## Value

An `epi_archive` object
