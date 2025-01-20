# global param constructor errors when required

    Code
      growth_rate_global_params(df = -5)
    Condition
      Error in `growth_rate_global_params()`:
      ! Assertion on 'df' failed: Element 1 is not >= 0.

---

    Code
      growth_rate_global_params(nlambda = 5:8)
    Condition
      Error in `growth_rate_global_params()`:
      ! Assertion on 'nlambda' failed: Must have length 1.

# new setup args and warnings are as expected

    Code
      growth_rate(-10:10, log_scale = TRUE)
    Condition
      Warning:
      `y` contains 0 or negative values. Taking logs may produce strange results.
    Output
       [1]       NaN       NaN       NaN       NaN       NaN       NaN       NaN
       [8]       NaN       NaN       NaN       NaN       NaN       NaN       NaN
      [15]       NaN       NaN       Inf 0.1950407 0.1633248 0.1431934       NaN

---

    Code
      growth_rate(c(1:20, NA, 22:30), method = "smooth_spline")
    Condition
      Error in `growth_rate()`:
      ! "smooth_spline" requires all real values without missingness.
      i Set `na_rm = TRUE` and / or check for infinite values.
      i Using `log_scale = TRUE` may induce either case.

---

    Code
      growth_rate(c(1:20, NA, 22:30), method = "trend_filter")
    Condition
      Error in `growth_rate()`:
      ! "trend_filter" requires all real values without missingness.
      i Set `na_rm = TRUE` and / or check for infinite values.
      i Using `log_scale = TRUE` may induce either case.

---

    Code
      growth_rate(c(1:20, -5, 22:30), log_scale = TRUE, method = "smooth_spline")
    Condition
      Warning:
      `y` contains 0 or negative values. Taking logs may produce strange results.
      Error in `growth_rate()`:
      ! "smooth_spline" requires all real values without missingness.
      i Set `na_rm = TRUE` and / or check for infinite values.
      i Using `log_scale = TRUE` may induce either case.

---

    Code
      growth_rate(c(1:20, -5, 22:30), log_scale = TRUE, method = "trend_filter")
    Condition
      Warning:
      `y` contains 0 or negative values. Taking logs may produce strange results.
      Error in `growth_rate()`:
      ! "trend_filter" requires all real values without missingness.
      i Set `na_rm = TRUE` and / or check for infinite values.
      i Using `log_scale = TRUE` may induce either case.

---

    Code
      growth_rate(y = c(1:20), method = "smooth_spline", params = growth_rate_global_params(
        lambda = 1:20))
    Condition
      Error in `growth_rate()`:
      ! "smooth_spline" requires 1 `lambda` but more were used.

