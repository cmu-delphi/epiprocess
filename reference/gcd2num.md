# Find the greatest common divisor of two numeric scalars

Not expected to be used directly; output class isn't precise, and checks
could be moved away into
[`gcd_num`](https://cmu-delphi.github.io/epiprocess/reference/gcd_num.md).

## Usage

``` r
gcd2num(a, b, rrtol = 1e-06, pqlim = 1e+06, irtol = 1e-06)
```

## Arguments

- a:

  Length 1, `is.numeric`; the first number

- b:

  Length 1, `is.numeric`; the second number

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

Length 1, `is.numeric`: the gcd. (Or an error.) Expected to be a double
unless `b` is the GCD and an integer, in which case it is expected be an
integer.

## Details

An implementation of a least absolute remainder Euclidean algorithm
(See, e.g., Moore, Thomas. "On the least absolute remainder Euclidean
algorithm." The Fibonacci Quarterly (1992).)

Notes on this implementation:

- We allow positive or negative inputs, and don't require \|a\| \>
  \|b\|.

- `round` combines the job of truncating division and deciding between
  positive and negative remainders.

- We use some tolerance parameters and different checks to allow this to
  work on floating-point numbers. Perhaps they could be altered or
  removed if we are passed integers, but for simplicity, we always
  perform these checks.
