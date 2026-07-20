# Merge two `epi_archive` objects

Merges two `epi_archive`s that share a common `geo_value`, `time_value`,
and set of key columns. When they also share a common `versions_end`,
using `epix_as_of` on the result should be the same as using
`epix_as_of` on `x` and `y` individually, then performing a full join of
the `DT`s on the non-version key columns (potentially consolidating
multiple warnings about clobberable versions). If the `versions_end`
values differ, the `sync` parameter controls what is done.

## Usage

``` r
epix_merge(
  x,
  y,
  sync = c("forbid", "na", "locf", "truncate"),
  compactify = TRUE,
  compactify_abs_tol = 0
)
```

## Arguments

- x, y:

  Two `epi_archive` objects to join together.

- sync:

  Optional; character. The argument that decides how to handle the
  situation when one signal has a more recent revision than another
  signal for a key that they have both already observed. The options
  are:

  - `"forbid"`: the default and the strictest option, throws an error;
    this is likely not what you want, but it is strict to make the user
    aware of the issues,

  - `"locf"`: carry forward the last observed version of the missing
    signal to the new version and use
    `max(x$versions_end, y$versions_end)` as the result's
    `versions_end`,

  - `"na"`: fill the unobserved values with `NA`'s (this can be handy
    when you know that source data is truly missing upstream and you
    want to represent the lack of information accurately, for instance)
    and use `max(x$versions_end, y$versions_end)` as the result's
    `versions_end`,

  - `"truncate"`: discard any rows containing update rows for later
    versions and use `min(x$versions_end, y$versions_end)` as the
    result's `versions_end`.

- compactify:

  Optional; `TRUE` (default), `FALSE`, or `"message"`; should the result
  be compactified? See
  [`as_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md)
  for details.

- compactify_abs_tol:

  As in
  [`as_epi_archive()`](https://cmu-delphi.github.io/epiprocess/reference/epi_archive.md).

## Value

the resulting `epi_archive`

## Details

When merging archives, unless the archives have identical data release
patterns, we often have to handle the situation when one signal has a
more recent observation for a key than another signal. In this case, we
have two options:

- if the the other signal has never observed that key, we need to
  introduce `NA`s in the non-key variables for the missing signal,

- if the other signal has observed that key previously, but at an ealier
  revision date, then we need to decide how to handle the missing value
  in the more recent signal; the `sync` argument controls this behavior.

In all cases, `clobberable_versions_start` will be set to the earliest
version that could be clobbered in either input archive.

## Examples

``` r
# Example 1
# The s1 signal at August 1st gets revised from 10 to 11 on August 2nd
s1 <- tibble::tibble(
  geo_value = c("ca", "ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
  version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-02")),
  signal1 = c(10, 11, 7)
)
s2 <- tibble::tibble(
  geo_value = c("ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-02")),
  version = as.Date(c("2024-08-03", "2024-08-03")),
  signal2 = c(2, 3)
)
s1 <- s1 %>% as_epi_archive()
s2 <- s2 %>% as_epi_archive()
merged <- epix_merge(s1, s2, sync = "locf")
merged[["DT"]]
#> Key: <geo_value, time_value, version>
#>    geo_value time_value    version signal1 signal2
#>       <char>     <Date>     <Date>   <num>   <num>
#> 1:        ca 2024-08-01 2024-08-01      10      NA
#> 2:        ca 2024-08-01 2024-08-02      11      NA
#> 3:        ca 2024-08-01 2024-08-03      11       2
#> 4:        ca 2024-08-02 2024-08-02       7      NA
#> 5:        ca 2024-08-02 2024-08-03       7       3

# Example 2
# The s1 signal at August 1st gets revised from 12 to 13 on August 3rd
s1 <- tibble::tibble(
  geo_value = c("ca", "ca", "ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-03")),
  version = as.Date(c("2024-08-01", "2024-08-03", "2024-08-03", "2024-08-03")),
  signal1 = c(12, 13, 22, 19)
)
s2 <- tibble::tibble(
  geo_value = c("ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-02")),
  version = as.Date(c("2024-08-02", "2024-08-02")),
  signal2 = c(4, 5),
)
s1 <- s1 %>% as_epi_archive()
s2 <- s2 %>% as_epi_archive()
merged <- epix_merge(s1, s2, sync = "locf")
merged[["DT"]]
#> Key: <geo_value, time_value, version>
#>    geo_value time_value    version signal1 signal2
#>       <char>     <Date>     <Date>   <num>   <num>
#> 1:        ca 2024-08-01 2024-08-01      12      NA
#> 2:        ca 2024-08-01 2024-08-02      12       4
#> 3:        ca 2024-08-01 2024-08-03      13       4
#> 4:        ca 2024-08-02 2024-08-02      NA       5
#> 5:        ca 2024-08-02 2024-08-03      22       5
#> 6:        ca 2024-08-03 2024-08-03      19      NA


# Example 3:
s1 <- tibble::tibble(
  geo_value = c("ca", "ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
  version = as.Date(c("2024-08-01", "2024-08-02", "2024-08-03")),
  signal1 = c(14, 11, 9)
)
# The s2 signal at August 1st gets revised from 3 to 5 on August 3rd
s2 <- tibble::tibble(
  geo_value = c("ca", "ca", "ca"),
  time_value = as.Date(c("2024-08-01", "2024-08-01", "2024-08-02")),
  version = as.Date(c("2024-08-02", "2024-08-03", "2024-08-03")),
  signal2 = c(3, 5, 2),
)
s1 <- s1 %>% as_epi_archive()
s2 <- s2 %>% as_epi_archive()
merged <- epix_merge(s1, s2, sync = "locf")
merged[["DT"]]
#> Key: <geo_value, time_value, version>
#>    geo_value time_value    version signal1 signal2
#>       <char>     <Date>     <Date>   <num>   <num>
#> 1:        ca 2024-08-01 2024-08-01      14      NA
#> 2:        ca 2024-08-01 2024-08-02      14       3
#> 3:        ca 2024-08-01 2024-08-03      14       5
#> 4:        ca 2024-08-02 2024-08-02      11      NA
#> 5:        ca 2024-08-02 2024-08-03      11       2
#> 6:        ca 2024-08-03 2024-08-03       9      NA
```
