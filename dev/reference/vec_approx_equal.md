# Test two vctrs vectors for equality with some tolerance in some cases

Generalizes
[`vctrs::vec_equal`](https://vctrs.r-lib.org/reference/vec_equal.html).

## Usage

``` r
vec_approx_equal(
  vec1,
  vec2,
  na_equal,
  .ptype = NULL,
  ...,
  abs_tol,
  inds1 = NULL,
  inds2 = NULL
)
```

## Arguments

- vec1, vec2:

  vctrs vectors (includes data frames). Take care when using on named
  vectors or "keyed" data frames;
  [`vec_names()`](https://vctrs.r-lib.org/reference/vec_names.html) are
  largely ignored, and key columns are treated as normal value columns
  (when they should probably generate an error if they are not lined up
  correctly, or be tested for exact rather than approximate equality).

- na_equal:

  should `NA`s be considered equal to each other? (In epiprocess, we
  usually want this to be `TRUE`, but that doesn't match the
  [`vctrs::vec_equal()`](https://vctrs.r-lib.org/reference/vec_equal.html)
  default, so this is mandatory.)

- .ptype:

  as in
  [`vctrs::vec_equal()`](https://vctrs.r-lib.org/reference/vec_equal.html).

- ...:

  should be empty (it's here to force later arguments to be passed by
  name)

- abs_tol:

  absolute tolerance; will be used for bare numeric `vec1`, `vec2`, or
  any such columns within `vec1`, `vec2` if they are data frames.

- inds1, inds2:

  optional (row) indices into vec1 and vec2 compatible with
  [`vctrs::vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.html);
  output should be consistent with `vec_slice`-ing to these indices
  beforehand, but can give faster computation if `vec1` and `vec2` are
  data frames. Currently, any speedup is only by making sure that
  `vec_slice` is used rather than `[` for data frames.

## Value

logical vector, with length matching the result of recycling `vec1` (at
`inds1` if provided) and `vec2` (at `inds2` if provided); entries should
all be `TRUE` or `FALSE` if `na_equal = TRUE`.

## Examples

``` r

library(dplyr)

# On numeric vectors:
vec_approx_equal(
  c(1, 2, 3, NA),
  c(1, 2 + 1e-10, NA, NA),
  na_equal = TRUE,
  abs_tol = 1e-8
)
#> [1]  TRUE  TRUE FALSE  TRUE

# On tibbles:
tbl1 <- tibble(
  a = 1:5,
  b = list(1:5, 1:4, 1:3, 1:2, 1:1) %>% lapply(as.numeric),
  c = tibble(
    c1 = 1:5
  ),
  d = matrix(1:10, 5, 2)
)
tbl2 <- tbl1
tbl2$a[[2]] <- tbl1$a[[2]] + 1e-10
tbl2$b[[3]][[1]] <- tbl1$b[[3]][[1]] + 1e-10
tbl2$c$c1[[4]] <- tbl1$c$c1[[4]] + 1e-10
tbl2$d[[5, 2]] <- tbl1$d[[5, 2]] + 1e-10
vctrs::vec_equal(tbl1, tbl2, na_equal = TRUE)
#> [1]  TRUE FALSE FALSE FALSE FALSE
vec_approx_equal(tbl1, tbl2, na_equal = TRUE, abs_tol = 1e-12)
#> [1]  TRUE FALSE FALSE FALSE FALSE
vec_approx_equal(tbl1, tbl2, na_equal = TRUE, abs_tol = 1e-8)
#> [1] TRUE TRUE TRUE TRUE TRUE


# Type comparison within lists is stricter, matching vctrs:
vctrs::vec_equal(list(1:2), list(as.numeric(1:2)))
#> [1] FALSE
vec_approx_equal(list(1:2), list(as.numeric(1:2)), FALSE, abs_tol = 0)
#> [1] FALSE
```
