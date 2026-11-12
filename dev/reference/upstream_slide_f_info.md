# Validate & get information about an upstream slide function

Validate & get information about an upstream slide function

## Usage

``` r
upstream_slide_f_info(.f, ...)
```

## Arguments

- .f:

  function such as
  [`data.table::frollmean`](https://rdrr.io/pkg/data.table/man/froll.html)
  or
  [`slider::slide_mean`](https://slider.r-lib.org/reference/summary-slide.html);
  must appear in
  [`upstream_slide_f_possibilities`](https://cmu-delphi.github.io/epiprocess/dev/reference/upstream_slide_f_possibilities.md)

- ...:

  additional configuration args to `.f` (besides the data and window
  size&alignment); used to validate `.f` is used in a supported way

## Value

named list with two elements: `from_package`, a string containing the
upstream package name ("data.table" or "slider"), and `namer`, a
function that takes a column to call `.f` on and outputs a basic name or
abbreviation for what operation `.f` represents on that kind of column
(e.g., "sum", "av", "count").
