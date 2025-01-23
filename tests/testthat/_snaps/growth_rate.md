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
      growth_rate(y = -10:10, log_scale = TRUE)
    Condition
      Warning:
      `y` contains 0 or negative values. Taking logs may produce strange results.
      Error in `growth_rate()`:
      ! Either the first or last `y` values are not finite. This may be due to `log_scale = TRUE`.

---

    Code
      growth_rate(y = -10:10, log_scale = TRUE, method = "smooth_spline")
    Condition
      Warning:
      `y` contains 0 or negative values. Taking logs may produce strange results.
      Error in `growth_rate()`:
      ! Either the first or last `y` values are not finite. This may be due to `log_scale = TRUE`.

---

    Code
      growth_rate(y = 1:30, x = c(1:20, NA, 22:30), na_rm = TRUE)
    Condition
      Error in `growth_rate()`:
      ! Neither `x` nor `x0` may contain `NA`s.

---

    Code
      growth_rate(y = 1:20, method = "smooth_spline", params = growth_rate_global_params(
        lambda = 1:20))
    Condition
      Error in `growth_rate()`:
      ! "smooth_spline" requires 1 `lambda` but more were used.

# parser sees all cases

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be `NULL` or a vector.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be `NULL` or a vector.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be `NULL` or a vector.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be `NULL` or a vector.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! When `cv = TRUE`, `df` must be `NULL` or character and `lambda` must be `NULL` or a vector.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! `df` a character implies using CV, but also setting `lambda` to a single value implies no CV.

---

    Code
      parse_trendfilter_params(l)
    Condition
      Error in `parse_trendfilter_params()`:
      ! `df` and `lambda` cannot both be scalars.

