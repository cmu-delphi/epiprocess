# Convert an `epi_df` to/from a "vstretched, numeric" form

Analogue of
[`to_vstretched_numeric_archive_info`](https://cmu-delphi.github.io/epiprocess/dev/reference/to_vstretched_numeric_archive_info.md)
for `epi_df`s: `to_vstretched_numeric_edf_info` converts an `epi_df` `x`
to (i) an `epi_df` with integer `time_type` and bare numeric
`time_value`s, plus (ii) conversion metadata, with (i) and (ii) packed
in a named list. `from_vstretched_numeric_edf_info` converts from this
format back to the format of `x`.

## Usage

``` r
to_vstretched_numeric_edf_info(x)

from_vstretched_numeric_edf_info(transformed_x, conversion)
```

## Arguments

- x:

  an `epi_df`

- transformed_x:

  an `epi_df` with `time_type` `"integer"` and bare numeric
  `time_value`s, in the converted form described by `conversion`; e.g.,
  `to_vstretched_numeric_edf_info(x)$edf` or some processed version of
  it.

- conversion:

  the `conversion` element of the `to_vstretched_numeric_edf_info`
  output.

## Value

- Of `to_vstretched_numeric_edf_info`: a list with elements:

  - `edf`: the converted `epi_df`, with `time_type` `"integer"`.

  - `conversion`: a list with elements `time_type` (`x`'s `time_type`)
    and `torigin`, to pass to `from_vstretched_numeric_edf_info` or use
    when converting other time values.

&nbsp;

- Of `from_vstretched_numeric_edf_info`: `transformed_x` with
  `time_type` and `time_value` representations converted back to those
  of the original `epi_df`.

## Details

`time_value`s are converted with
[`time_values_to_torigin_offsets`](https://cmu-delphi.github.io/epiprocess/dev/reference/time_values_to_torigin_offsets.md);
other time values (e.g., function arguments) can be converted
consistently by forwarding `conversion$torigin` and
`conversion$time_type` to it, and back with
[`torigin_offsets_to_time_values`](https://cmu-delphi.github.io/epiprocess/dev/reference/torigin_offsets_to_time_values.md).
`epi_df`s have no `version` column, and `as_of` is left unchanged (it's
not converted to the stretched scale), so verbs shouldn't relate it to
the converted `time_value`s.

Grouping is preserved, but `x` must not be grouped by `time_value`.

Verbs that change the number of rows from zero to nonzero are
incompatible with the "from" transformation.
