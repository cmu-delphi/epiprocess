# Like [`versions_to_vorigin_offsets()`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md), but forwarding any `NA`

For `epi_archive` version bounds like `clobberable_versions_start`,
which use a logical `NA` rather than an `NA` of the `version` class.

## Usage

``` r
version_bound_to_vorigin_offset_bound(version_bound, vorigin)
```
