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
#'   If `f` is missing, then ["data-masking"][rlang::args_data_masking]
#'   expression(s) for tidy evaluation can be specified, for example, as in:
#'   ```
#'   epi_slide(x, cases_7dav = mean(cases), before = 6)
#'   ```
#'   which would be equivalent to:
#'   ```
#'   epi_slide(x, function(x, g, t) mean(x$cases), before = 6,
#'             new_col_name = "cases_7dav")
#'   ```
#'   In a manner similar to [`dplyr::mutate`]:
#'   * Expressions evaluating to length-1 vectors will be recycled to
#'     appropriate lengths.
#'   * `= NULL` can be used to remove results from previous expressions (though
#'     we don't allow it to remove pre-existing columns).
#'   * Unnamed expressions evaluating to data frames will be unpacked into
#'     multiple columns in the result; to use this feature, you will need to add
#'     an extra comma before your first data-masking expression to make sure `f`
#'     appears as missing.
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
