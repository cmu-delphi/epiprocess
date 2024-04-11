#' @details To "slide" means to apply a function over a rolling window of time
#'   steps for each data group, where the window is centered at a reference
#'   time and left and right endpoints are given by the `before` and `after`
#'   arguments. The unit (the meaning of one time step) is implicitly defined
#'   by the way the `time_value` column treats addition and subtraction; for
#'   example, if the time values are coded as `Date` objects, then one time
#'   step is one day, since `as.Date("2022-01-01") + 1` equals `as.Date
#'   ("2022-01-02")`. Alternatively, the time step can be set explicitly using
#'   the `time_step` argument (which if specified would override the default
#'   choice based on `time_value` column). If there are not enough time steps
#'   available to complete the window at any given reference time, then
#'   `epi_slide_*()` will fail; it requires a complete window to perform the
#'   computation. For a centrally-aligned slide of `n` `time_value`s in a
#'   sliding window, set `before = (n-1)/2` and `after = (n-1)/2` when the
#'   number of `time_value`s in a sliding window is odd and `before = n/2-1`
#'   and `after = n/2` when `n` is even.
#'
#'   Sometimes, we want to experiment with various trailing or leading window
#'   widths and compare the slide outputs. In the (uncommon) case where
#'   zero-width windows are considered, manually pass both the `before` and
#'   `after` arguments in order to prevent potential warnings. (E.g., `before=k`
#'   with `k=0` and `after` missing may produce a warning. To avoid warnings,
#'   use `before=k, after=0` instead; otherwise, it looks too much like a
#'   leading window was intended, but the `after` argument was forgotten or
#'   misspelled.)
