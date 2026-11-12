# Calculate input and output column names for an `{epiprocess}` [`dplyr::across`](https://dplyr.tidyverse.org/reference/across.html)-like operations

Calculate input and output column names for an `{epiprocess}`
[`dplyr::across`](https://dplyr.tidyverse.org/reference/across.html)-like
operations

## Usage

``` r
across_ish_names_info(
  .x,
  time_type,
  col_names_quo,
  .f_namer,
  .window_size,
  .align,
  .prefix,
  .suffix,
  .new_col_names
)
```

## Arguments

- .x:

  data.frame to perform input column tidyselection on

- time_type:

  as in
  [`new_epi_df`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_df.md)

- col_names_quo:

  enquosed input column tidyselect expression

- .f_namer:

  function taking an input column object and outputting a name for a
  corresponding output column; see
  [`upstream_slide_f_info`](https://cmu-delphi.github.io/epiprocess/dev/reference/upstream_slide_f_info.md)

- .window_size:

  as in
  [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)

- .align:

  as in
  [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)

- .prefix:

  as in
  [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)

- .suffix:

  as in
  [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)

- .new_col_names:

  as in
  [`epi_slide_opt`](https://cmu-delphi.github.io/epiprocess/dev/reference/epi_slide_opt.md)

## Value

named list with two elements: `input_col_names`, chr, subset of
`names(.x)`; and `output_colnames`, chr, same length as
`input_col_names`
