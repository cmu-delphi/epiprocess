# Variant of [`vctrs::vec_cast`](https://vctrs.r-lib.org/reference/vec_cast.html) that allows chr \<-\> date, disallows is.numeric x \<-\> some times

Doesn't implement other conversions implied by the hierarchy, e.g., chr
\<-\> POSIX{c,l}t.

## Usage

``` r
vec_cast_patched(
  x,
  to,
  ...,
  x_arg = caller_arg(x),
  to_arg = "",
  call = caller_env()
)
```

## Arguments

- x:

  Vectors to cast.

- to:

  Type to cast to. If `NULL`, `x` will be returned as is.

- ...:

  For `vec_cast_common()`, vectors to cast. For `vec_cast()`,
  `vec_cast_default()`, and `vec_restore()`, these dots are only for
  future extensions and should be empty.

- x_arg:

  Argument name for `x`, used in error messages to inform the user about
  the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.html)).

- to_arg:

  Argument name `to` used in error messages to inform the user about the
  locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.html)).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
