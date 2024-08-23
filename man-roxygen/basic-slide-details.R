#' @details To "slide" means to apply a function or formula over a rolling
#'   window. The `.window_size` arg determines the width of the window
#'   (including the reference time) and the `.align` arg governs how the window
#'   is aligned (see below for examples). The `.ref_time_values` arg controls
#'   which time values to consider for the slide and `.all_rows` allows you to
#'   keep NAs around.
#'
#'   `epi_slide()` does not require a complete window (such as on the left
#'   boundary of the dataset) and will attempt to perform the computation
#'   anyway. The issue of what to do with partial computations (those run on
#'   incomplete windows) is therefore left up to the user, either through the
#'   specified function or formula `f`, or through post-processing.
#'
#'   Let's look at some window examples, assuming that the reference time value
#'   is `tv`. With .align = "right" and .window_size = 3, the window will be:
#'
#'   time_values: tv - 4, tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                     [tv - 2, tv - 1, tv]
#'
#'   With .align = "center" and .window_size = 3, the window will be:
#'
#'   time_values: tv - 4, tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                             [tv - 1, tv, tv + 1]
#'
#'   With .align = "center" and .window_size = 4, the window will be:
#'
#'   time_values: tv - 4, tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                     [tv - 2, tv - 1, tv, tv + 1]
#'
#'   With .align = "left" and .window_size = 3, the window will be:
#'
#'   time_values: tv - 4, tv - 3, tv - 2, tv - 1, tv, tv + 1, tv + 2, tv + 3
#'   window:                                     [tv, tv + 1, tv + 2]
#'
#'   If `f` is missing, then ["data-masking"][rlang::args_data_masking]
#'   expression(s) for tidy evaluation can be specified, for example, as in:
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), .window_size = 7)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, function(x, g, t) mean(x$cases), .window_size = 7,
#'             new_col_name = "cases_7dav")
#'   ```
#'   In a manner similar to [`dplyr::mutate`]:
#'   * Expressions evaluating to length-1 vectors will be recycled to
#'     appropriate lengths.
#'   * `, name_var := value` can be used to set the output column name based on
#'     a variable `name_var` rather than requiring you to use a hard-coded
#'     name. (The leading comma is needed to make sure that `f` is treated as
#'     missing.)
#'   * `= NULL` can be used to remove results from previous expressions (though
#'     we don't allow it to remove pre-existing columns).
#'   * `, fn_returning_a_data_frame(.x)` will unpack the output of the function
#'     into multiple columns in the result.
#'   * Named expressions evaluating to data frames will be placed into
#'     [`tidyr::pack`]ed columns.
#'
#'   In addition to [`.data`] and [`.env`], we make some additional
#'   "pronoun"-like bindings available:
#'   * .x, which is like `.x` in [`dplyr::group_modify`]; an ordinary object
#'     like an `epi_df` rather than an rlang [pronoun][rlang::as_data_pronoun]
#'     like [`.data`]; this allows you to use additional {dplyr}, {tidyr}, and
#'     {epiprocess} operations. If you have multiple expressions in `...`, this
#'     won't let you refer to the output of the earlier expressions, but `.data`
#'     will.
#'   * .group_key, which is like `.y` in [`dplyr::group_modify`].
#'   * .ref_time_value, which is the element of `ref_time_values` that
#'     determined the time window for the current computation.
