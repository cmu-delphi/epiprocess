# [`lifecycle::is_present`](https://lifecycle.r-lib.org/reference/deprecated.html) for enquosed deprecated NSE arg

[`lifecycle::is_present`](https://lifecycle.r-lib.org/reference/deprecated.html)
is designed for use with args that undergo standard evaluation, rather
than non-standard evaluation (NSE). This function is designed to fulfill
a similar purpose, but for args we have
[enquosed](https://rlang.r-lib.org/reference/enquo.html) in preparation
for NSE.

## Usage

``` r
deprecated_quo_is_present(quo)
```

## Arguments

- quo:

  [enquosed](https://rlang.r-lib.org/reference/enquo.html) arg

## Value

bool; was `quo` "present", or did it look like a missing quosure or have
an expr that looked like `deprecated()` or
[`lifecycle::deprecated()`](https://lifecycle.r-lib.org/reference/deprecated.html)?

## Examples

``` r
fn <- function(x = deprecated()) {
  deprecated_quo_is_present(rlang::enquo(x))
}

fn() # FALSE
#> [1] FALSE
fn(.data$something) # TRUE
#> [1] TRUE

# Functions that wrap `fn` should forward the NSE arg to `fn` using
# [`{{ arg }}`][rlang::embrace-operator] (or, if they are working from an
# argument that has already been defused into a quosure, `!!quo`). (This is
# already how NSE arguments that will be enquosed should be forwarded.)

wrapper1 <- function(x = deprecated()) fn({{ x }})
wrapper2 <- function(x = lifecycle::deprecated()) fn({{ x }})
wrapper3 <- function(x) fn({{ x }})
wrapper4 <- function(x) fn(!!rlang::enquo(x))

wrapper1() # FALSE
#> [1] FALSE
wrapper2() # FALSE
#> [1] FALSE
wrapper3() # FALSE
#> [1] FALSE
wrapper4() # FALSE
#> [1] FALSE

# More advanced: wrapper that receives an already-enquosed arg:

inner_wrapper <- function(quo) fn(!!quo)
outer_wrapper1 <- function(x = deprecated()) inner_wrapper(rlang::enquo(x))

outer_wrapper1() # FALSE
#> [1] FALSE

# Improper argument forwarding from a wrapper function will cause this
# function to produce incorrect results.
bad_wrapper1 <- function(x) fn(x)
bad_wrapper1() # TRUE, bad
#> [1] TRUE
```
