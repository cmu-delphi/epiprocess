# Format each entry in a character vector via quoting; special replacement for length 0

Performs no escaping within the strings; if you want something that
reader could copy-paste to debug, look into `format_deparse` (note that
this collapses into a single string).

## Usage

``` r
format_chr_with_quotes(x, empty = "*none*")
```

## Arguments

- x:

  chr; e.g., `colnames` of some data frame

- empty:

  chr, likely string; what should be output if `x` is of length 0?

## Value

chr; same `length` as `x` if `x` had nonzero length; value of `empty`
otherwise

## Examples

``` r
cli::cli_inform('{epiprocess:::format_chr_with_quotes("x")}')
#> "x"
cli::cli_inform('{epiprocess:::format_chr_with_quotes(c("x","y"))}')
#> "x" and "y"
nms <- c("x", "\"Total Cases\"")
cli::cli_inform("{epiprocess:::format_chr_with_quotes(nms)}")
#> "x" and ""Total Cases""
cli::cli_inform("{epiprocess:::format_chr_with_quotes(character())}")
#> *none*
```
