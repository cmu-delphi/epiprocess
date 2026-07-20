# Convert to tibble

Converts an `epi_df` object into a tibble, dropping metadata, any
grouping, and any unrelated classes and attributes.

## Usage

``` r
# S3 method for class 'epi_df'
as_tibble(x, ...)
```

## Arguments

- x:

  an `epi_df`

- ...:

  if present, forwarded to
  [`tibble::as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)

## Details

Advanced: if you are working with a third-party package that uses
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
on `epi_df`s but you actually want them to remain `epi_df`s, use
`attr(your_epi_df, "decay_to_tibble") <- FALSE` beforehand.
