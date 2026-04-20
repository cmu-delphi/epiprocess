# Find the greatest common divisor of all entries in a numeric vector

Find the greatest common divisor of all entries in a numeric vector

## Usage

``` r
gcd_num(dividends, ..., rrtol = 1e-06, pqlim = 1e+06, irtol = 1e-06)
```

## Arguments

- dividends:

  `is.numeric`, `length` \> 0; the dividends for which to find the
  greatest common divisor.

- ...:

  should be empty; forces the following parameters to be passed by name

- rrtol:

  Optional, length 1, `is.numeric`, non-negative; the remainder relative
  tolerance: consider a remainder from a division operation to be zero
  if it is `abs(remainder/divisor) <= rrtol`. Could also be described as
  a tolerance on the fractional part of the proper quotient. Default is
  1e-6.

- pqlim:

  Optional, length 1, `is.numeric`, non-negative; the proper quotient
  limit: consider a divisor to be zero if
  `abs(dividend/divisor) >= pqlim`.

- irtol:

  Optional, length 1, `is.numeric`, non-negative; the iterand relative
  tolerance: consider `a` and `b` to have no gcd if the absolute value
  of an iterand (and consequently also any result that might be
  subsequently produced, as absolute values of iterands are decreasing)
  is `<= irtol * a` or `<= irtol * b`. Also can be seen as the
  reciprocal of a limit on the number `k` needed to achieve
  `k * gcd_result == max(abs(a),abs(b))`.

## Value

Same
[`vctrs::vec_ptype`](https://vctrs.r-lib.org/reference/vec_ptype.html)
as `dividends`, `length` 1: the gcd. (Or an error.)
