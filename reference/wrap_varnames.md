# Line wrap `chr` holding variable/column/other names, with prefix&indent

Line wrap `chr` holding variable/column/other names, with prefix&indent

## Usage

``` r
wrap_varnames(
  nms,
  initial = "",
  common_prefix = "",
  none_str = "<none>",
  width = getOption("width", 80L)
)
```

## Arguments

- nms:

  Character vector: the variable names (potentially empty)

- initial:

  Optional; single string: a prefix for the initial line in the result;
  e.g., "Variable names: ". Defaults to "". Any non-initial lines will
  be indented with whitespace matching the (estimated) visual width of
  `initial`.

- common_prefix:

  Optional; single string: a prefix for every line (will appear before
  `initial`); e.g., "# ". Defaults to "".

- none_str:

  Optional; single string: what to display when given `length`-0 input.
  Will be combined with `common_prefix` and `initial`.

- width:

  Optional; single integer: desired maximum formatted line width. The
  formatted output may not obey this setting if `common_prefix` plus
  `initial` is long or the printing width is very narrow.

## Value

`chr`; to print, use
[`base::writeLines`](https://rdrr.io/r/base/writeLines.html).
