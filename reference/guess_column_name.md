# rename potential time_value columns

potentially renames

## Usage

``` r
guess_column_name(x, column_name, substitutions)
```

## Arguments

- x:

  the tibble to potentially rename

- column_name:

  str; both the column name to complain about lacking, and the basis for
  the `*_column_name()` function to suggest looking at

- substitutions:

  a named vector. the potential substitions, with every name
  `time_value`
