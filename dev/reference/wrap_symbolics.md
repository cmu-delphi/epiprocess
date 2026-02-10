# Line wrap list holding [symbolic](https://rlang.r-lib.org/reference/is_expression.html), with prefix&indent

Helps pretty-print these objects. Adds backticks, commas, prefixes, and
indentation. Wraps lines, but won't insert line breaks in the middle of
any name while doing so.

## Usage

``` r
wrap_symbolics(
  symbolics,
  initial = "",
  common_prefix = "",
  none_str = "<none>",
  width = getOption("width", 80L)
)
```

## Arguments

- symbolics:

  List of
  [symbolic](https://rlang.r-lib.org/reference/is_expression.html)
  objects: the variable names (potentially empty)

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
