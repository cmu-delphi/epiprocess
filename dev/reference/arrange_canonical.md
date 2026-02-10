# Arrange an epi_df into a standard order

Moves
[`key_colnames()`](https://cmu-delphi.github.io/epiprocess/dev/reference/key_colnames.md)
to the left, then arranges rows based on that ordering. This function is
mainly for use in tests and so that other function output will be in
predictable order, where necessary.

## Usage

``` r
arrange_canonical(x, ...)
```

## Arguments

- x:

  an `epi_df`. Other objects will produce a warning and return as is.

- ...:

  not used
