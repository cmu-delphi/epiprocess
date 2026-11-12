# Convert an archive to/from a "vstretched, numeric" form

Arithmetic is slow on some time-related classes. It can be faster to
perform operations under integer time_type. These functions provide
conversions to/from integer-time-type `epi_archive`s which preserve
time-time spacing in terms of nsteps and the ordering of versions, but
which do not try to preserve anything about time-version differences or
version-version differences; thus "vstretched".

## Usage

``` r
to_vstretched_numeric_archive_info(x)

from_vstretched_numeric_archive_info(transformed_x, conversion)
```

## Arguments

- x:

  an `epi_archive`

- transformed_x:

  an `epi_archive` with `time_type` `"integer"` and bare numeric
  `time_value`s and `version`s, in the converted form described by
  `conversion`; e.g., `to_vstretched_numeric_archive_info(x)$archive` or
  some processed version of it. `time_value`s must be integerish.

- conversion:

  the `conversion` element of the `to_vstretched_numeric_archive_info`
  output.

## Value

- Of `to_vstretched_numeric_archive_info`: a list with elements:

  - `archive`: the converted `epi_archive`, with `time_type`
    `"integer"`.

  - `conversion`: a list with elements `time_type` (`x$time_type`),
    `torigin`, and `vorigin`, to pass to
    `from_vstretched_numeric_archive_info` or use when converting other
    time values and versions.

&nbsp;

- Of `from_vstretched_numeric_archive_info`: `transformed_x` with
  `time_type`, `time_value`, and `version` representations converted
  back to those of the original archive.

## Details

`to_vstretched_numeric_archive_info` converts an archive `x` to (i) an
archive with integer `time_type` and bare numeric `time_value`s and
`version`s, plus (ii) conversion metadata, with (i) and (ii) packed in a
named list. `from_vstretched_numeric_archive_info` converts from this
format back to the format of `x`.

For some verbs, we may need to transform additional arguments using
[`time_values_to_torigin_offsets`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md),
[`versions_to_vorigin_offsets`](https://cmu-delphi.github.io/epiprocess/dev/reference/versions_to_vorigin_offsets.md),
or
[`version_bound_to_vorigin_offset_bound`](https://cmu-delphi.github.io/epiprocess/dev/reference/version_bound_to_vorigin_offset_bound.md),
forwarding the appropriate extra arguments from the info object's
conversion metadata.

Verbs that change the number of archive rows from zero to nonzero are
incompatible with the "from" transformation.
