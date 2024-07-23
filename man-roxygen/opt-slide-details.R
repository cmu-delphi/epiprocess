#' @details To "slide" means to apply a function over a rolling window of time
#'   steps for each data group, where the window is centered at a reference time
#'   and left and right endpoints are given by the `before` and `after`
#'   arguments.

#'   If there are not enough time steps available to complete the window at any
#'   given reference time, then `epi_slide_*()` will fail; it requires a
#'   complete window to perform the computation. For a centrally-aligned slide
#'   of `n` `time_value`s in a sliding window, set `before = (n-1)/2` and `after
#'   = (n-1)/2` when the number of `time_value`s in a sliding window is odd and
#'   `before = n/2-1` and `after = n/2` when `n` is even.
#'
#'   Sometimes, we want to experiment with various trailing or leading window
#'   widths and compare the slide outputs. In the (uncommon) case where
#'   zero-width windows are considered, manually pass both the `before` and
#'   `after` arguments.
