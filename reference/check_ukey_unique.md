# Check that a unique key is indeed unique in a tibble (TRUE/str)

A `checkmate`-style check function.

## Usage

``` r
check_ukey_unique(x, ukey_names, end_cli_message = character())
```

## Arguments

- x:

  a tibble, with no particular row or column order (if you have a
  guaranteed row order based on the ukey you can probably do something
  more efficient)

- ukey_names:

  character vector; subset of column names of `x` denoting a unique key.

- end_cli_message:

  optional character vector, a cli message format string/vector;
  information/advice to tack onto any error messages.

## Value

`TRUE` if no ukey is duplicated (i.e., `x[ukey_names]` has no duplicated
rows); string with an error message if there are errors.
