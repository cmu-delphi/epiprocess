# Is `x` an "int" with a sensible class? TRUE/FALSE

Like
[`checkmate::test_int`](https://mllg.github.io/checkmate/reference/checkInt.html)
but disallowing some non-sensible classes that `test_int` accepts, such
as `difftime`s. We rely on
[`is.numeric`](https://rdrr.io/r/base/numeric.html) to determine class
appropriateness; note that `is.numeric` is NOT simply checking for the
class to be "numeric" (or else we'd fail on integer class).

## Usage

``` r
test_sensible_int(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE
)
```

## Arguments

- x:

  object

## Value

Boolean
