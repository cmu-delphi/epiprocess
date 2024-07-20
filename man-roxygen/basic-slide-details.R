#' @details To "slide" means to apply a function or formula over a rolling
#'   window of time steps for each data group, where the window is centered at a
#'   reference time and left and right endpoints are given by the `before` and
#'   `after` arguments.
#'
#'   If there are not enough time steps available to complete the window at any
#'   given reference time, then `epi_slide()` still attempts to perform the
#'   computation anyway (it does not require a complete window). The issue of
#'   what to do with partial computations (those run on incomplete windows) is
#'   therefore left up to the user, either through the specified function or
#'   formula `f`, or through post-processing. For a centrally-aligned slide of
#'   `n` `time_value`s in a sliding window, set `before = (n-1)/2` and `after =
#'   (n-1)/2` when the number of `time_value`s in a sliding window is odd and
#'   `before = n/2-1` and `after = n/2` when `n` is even.
#'
#'   Sometimes, we want to experiment with various trailing or leading window
#'   widths and compare the slide outputs. In the (uncommon) case where
#'   zero-width windows are considered, manually pass both the `before` and
#'   `after` arguments.
#'
#'   If `f` is missing, then an expression for tidy evaluation can be specified,
#'   for example, as in:
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), before = 6)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, function(x, g) mean(x$cases), before = 6,
#'             new_col_name = "cases_7dav")
#'   ```
#'   Thus, to be clear, when the computation is specified via an expression for
#'   tidy evaluation (first example, above), then the name for the new column is
#'   inferred from the given expression and overrides any name passed explicitly
#'   through the `new_col_name` argument.
