#' @details To "slide" means to apply a function or formula over a rolling
#'   window. The `.window_size` arg determines the width of the window
#'   (including the reference time) and the `.align` arg governs how the window
#'   is aligned (see below for examples). The `.ref_time_values` arg controls
#'   which time values to consider for the slide and `.all_rows` allows you to
#'   keep NAs around.
#'
#'   `epi_slide_*()` does not require a complete window (such as on the left
#'   boundary of the dataset) and will attempt to perform the computation
#'   anyway. The issue of what to do with partial computations (those run on
#'   incomplete windows) is therefore left up to the user, either through the
#'   specified function or formula `f`, or through post-processing.
#'
#'   Let's look at some window examples, assuming that the reference time value
#'   is "tv". With .align = "right" and .window_size = 3, the window will be:
#'
#'   time_values: tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:              tv - 2, tv - 1, tv
#'
#'   With .align = "center" and .window_size = 3, the window will be:
#'
#'   time_values: tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                      tv - 1, tv, tv + 1
#'
#'   With .align = "center" and .window_size = 4, the window will be:
#'
#'   time_values: tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:              tv - 2, tv - 1, tv, tv + 1
#'
#'   With .align = "left" and .window_size = 3, the window will be:
#'
#'   time_values: ttv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                               tv, tv + 1, tv + 2
