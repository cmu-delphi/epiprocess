#' More general form of [`epi_slide_opt`] for rolling/running computations
#'
#' Most rolling/running computations can be handled by [`epi_slide_mean`],
#' [`epi_slide_sum`], or the medium-generality [`epi_slide_opt`] functions
#' instead, which are much faster. You typically only need to consider
#' `epi_slide()` if you have a computation that depends on multiple columns
#' simultaneously, outputs multiple columns simultaneously, or produces
#' non-numeric output.  For example, this computation depends on multiple
#' columns:
#'
#' ```
#' cases_deaths_subset %>%
#'   epi_slide(
#'     cfr_estimate_v0 = death_rate_7d_av[[22]]/case_rate_7d_av[[1]],
#'     .window_size = 22
#'   ) %>%
#'   print(n = 30)
#' ```
#'
#' (Here, the value 22 was selected using `epi_cor()` and averaging across
#' `geo_value`s. See
#' \href{https://www.medrxiv.org/content/10.1101/2024.12.27.24319518v1}{this
#' manuscript}{this manuscript} for some warnings & information using similar
#' types of CFR estimators.)
#'
#' See `vignette("epi_df")` for more examples.
#'
#' @template basic-slide-params
#' @param .f,... The computation to slide. The input will be a time window of
#'   the data for a single subpopulation (i.e., a single `geo_value` and single
#'   value for any [`other_keys`][as_epi_df] you set up, such as age groups, race, etc.).
#'   The input will always have the same size, determined by `.window_size`, and
#'   will fill in any missing `time_values`, using `NA` values for missing
#'   measurements. The output should be a scalar value or a 1-row data frame;
#'   these outputs will be collected into a new column or columns in the
#'   `epi_slide()` result. Data frame outputs will be unpacked into multiple
#'   columns in the result by default, or [`tidyr::pack`]ed into a single
#'   data-frame-type column if you provide a name for such a column (e.g., via
#'   `.new_col_name`).
#'
#' You can specify the computation in one of the following ways:
#'
#' - Don't provide `.f`, and instead use one or more
#'   [`dplyr::summarize`]-esque ["data-masking"][rlang::args_data_masking]
#'   expressions in `...`, e.g., `cfr_estimate_v0 =
#'   death_rate_7d_av[[22]]/case_rate_7d_av[[1]]`. This way is sometimes more
#'   convenient, but also has the most computational overhead.
#'
#' - Provide a formula in `.f`, e.g., `~
#'   .x$death_rate_7d_av[[22]]/.x$case_rate_7d_av[[1]]`. In this formula, `.x`
#'   is an `epi_df` containing data for a single time window as described above,
#'   taken from the original `.x` fed into `epi_slide()`.
#'
#' - Provide a function in `.f`, e.g., `function(x, g, t)
#'   x$death_rate_7d_av[[22]]/x$case_rate_7d_av[[1]]`. The function should be of
#'   the form `function(x, g, t)` or `function(x, g, t, <additional
#'   configuration arguments>)`, where:
#'
#'     - `x` is a data frame with the same column names as the original object,
#'       minus any grouping variables, with only the windowed data for one
#'       group-`.ref_time_value` combination
#'
#'     - `g` is a one-row tibble specifying the `geo_value` and value of any
#'       `other_keys` for this computation
#'
#'     - `t` is the `.ref_time_value` for the current window
#'
#'     - If you have a complex `.f` containing `<additional configuration
#'     arguments>`, you can provide values for those arguments in the `...`
#'     argument to `epi_slide()`.
#'
#'   The values of `g` and `t` are also available to data-masking expression and
#'   formula-based computations as `.group_key` and `.ref_time_value`,
#'   respectively. Formula computations also let you use `.y` or `.z`,
#'   respectively, as additional names for these same quantities (similar to
#'   [`dplyr::group_modify`]).
#'
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `.f`. Alternatively, if `.f` is missing, then the `...` is interpreted
#'   as a ["data-masking"][rlang::args_data_masking] expression or expressions
#'   for tidy evaluation.
#' @param .new_col_name Name for the new column that will contain the computed
#'   values. The default is "slide_value" unless your slide computations output
#'   data frames, in which case they will be unpacked (as in `tidyr::unpack()`)
#'   into the constituent columns and those names used. New columns should not
#'   be given names that clash with the existing columns of `.x`.
#'
#' @details
#'
#' ## Motivation and lower-level alternatives
#'
#' `epi_slide()` is focused on preventing errors and providing a convenient
#' interface. If you need computational speed, many computations can be optimized
#' by one of the following:
#'
#' * Performing core sliding operations with `epi_slide_opt()` with
#'   `frollapply`, and using potentially-grouped `mutate()`s to transform or
#'   combine the results.
#'
#' * Grouping by `geo_value` and any `other_keys`; [`complete()`]ing with
#'   `full_seq()` to fill in time gaps; `arrange()`ing by `time_value`s within
#'   each group; using `mutate()` with vectorized operations and shift operators
#'   like `dplyr::lead()` and `dplyr::lag()` to perform the core operations,
#'   being careful to give the desired results for the least and most recent
#'   `time_value`s (often `NA`s for the least recent); ungrouping; and
#'   `filter()`ing back down to only rows that existed before the `complete()`
#'   stage if necessary.
#'
#' ## Advanced uses of `.f` via tidy evaluation
#'
#' If specifying `.f` via tidy evaluation, in addition to the standard [`.data`]
#' and [`.env`], we make some additional "pronoun"-like bindings available:
#'
#'   - .x, which is like `.x` in [`dplyr::group_modify`]; an ordinary object
#'     like an `epi_df` rather than an rlang [pronoun][rlang::as_data_pronoun]
#'     like [`.data`]; this allows you to use additional `dplyr`, `tidyr`, and
#'     `epiprocess` operations. If you have multiple expressions in `...`, this
#'     won't let you refer to the output of the earlier expressions, but `.data`
#'     will.
#'   - .group_key, which is like `.y` in [`dplyr::group_modify`].
#'   - .ref_time_value, which is the element of `.ref_time_values` that
#'     determined the time window for the current computation.
#'
#' @importFrom lubridate days weeks
#' @importFrom dplyr group_map group_vars filter select
#' @importFrom rlang .data .env !! enquos sym env missing_arg
#' @export
#' @seealso [`epi_slide_opt`] for optimized slide functions
#' @examples
#' library(dplyr)
#'
#' # Generate some simple time-varying CFR estimates:
#' with_cfr_estimates <- cases_deaths_subset %>%
#'   epi_slide(
#'     cfr_estimate_v0 = death_rate_7d_av[[22]] / case_rate_7d_av[[1]],
#'     .window_size = 22
#'   )
#' with_cfr_estimates %>%
#'   print(n = 30)
#' # (Here, the value 22 was selected using `epi_cor()` and averaging across
#' # `geo_value`s. See
#' # https://www.medrxiv.org/content/10.1101/2024.12.27.24319518v1 for some
#' # warnings & information using CFR estimators along these lines.)
#'
#' # In addition to the [`dplyr::mutate`]-like syntax, you can feed in a
#' # function or formula in a way similar to [`dplyr::group_modify`]; these
#' # often run much more quickly:
#' my_computation <- function(window_data) {
#'   tibble(
#'     cfr_estimate_v0 = window_data$death_rate_7d_av[[nrow(window_data)]] /
#'       window_data$case_rate_7d_av[[1]]
#'   )
#' }
#' with_cfr_estimates2 <- cases_deaths_subset %>%
#'   epi_slide(
#'     ~ my_computation(.x),
#'     .window_size = 22
#'   )
#' with_cfr_estimates3 <- cases_deaths_subset %>%
#'   epi_slide(
#'     function(window_data, g, t) {
#'       tibble(
#'         cfr_estimate_v0 = window_data$death_rate_7d_av[[nrow(window_data)]] /
#'           window_data$case_rate_7d_av[[1]]
#'       )
#'     },
#'     .window_size = 22
#'   )
#'
#'
#' #### Advanced: ####
#'
#' # The tidyverse supports ["packing"][tidyr::pack] multiple columns into a
#' # single tibble-type column contained within some larger tibble. Like dplyr,
#' # we normally don't pack output columns together. However, packing behavior can be turned on
#' # by providing a name for a tibble-type output:
#' cases_deaths_subset %>%
#'   epi_slide(
#'     slide_packed = tibble(
#'       cases_7sd = sd(.x$cases, na.rm = TRUE),
#'       cases_7dav = mean(.x$cases, na.rm = TRUE)
#'     ),
#'     .window_size = 7
#'   ) %>%
#'   select(geo_value, time_value, cases, slide_packed)
#' cases_deaths_subset %>%
#'   epi_slide(
#'     ~ tibble(
#'       cases_7sd = sd(.x$cases, na.rm = TRUE),
#'       cases_7dav = mean(.x$cases, na.rm = TRUE)
#'     ),
#'     .new_col_name = "slide_packed",
#'     .window_size = 7
#'   ) %>%
#'   select(geo_value, time_value, cases, slide_packed)
#'
#' # You can also get ["nested"][tidyr::nest] format by wrapping your results in
#' # a list:
#' cases_deaths_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(
#'     function(x, g, t) {
#'       list(tibble(
#'         cases_7sd = sd(x$cases, na.rm = TRUE),
#'         cases_7dav = mean(x$cases, na.rm = TRUE)
#'       ))
#'     },
#'     .window_size = 7
#'   ) %>%
#'   ungroup() %>%
#'   select(geo_value, time_value, slide_value)
#'
#'
#'
#' # Use the geo_value or the ref_time_value in the slide computation
#' cases_deaths_subset %>%
#'   epi_slide(~ .x$geo_value[[1]], .window_size = 7)
#'
#' cases_deaths_subset %>%
#'   epi_slide(~ .x$time_value[[1]], .window_size = 7)
epi_slide <- function(
    .x, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
    .ref_time_values = NULL, .new_col_name = NULL, .all_rows = FALSE) {
  # Deprecated argument handling
  provided_args <- rlang::call_args_names(rlang::call_match())
  if (any(provided_args %in% c("x", "f", "ref_time_values", "new_col_name", "all_rows"))) {
    cli::cli_abort(
      "epi_slide: you are using one of the following old argument names: `x`, `f`, `ref_time_values`,
      `new_col_name`, or `all_rows`. Please use the new dot-prefixed names: `.x`, `.f`, `.ref_time_values`,
      `.new_col_name`, `.all_rows`."
    )
  }
  if ("as_list_col" %in% provided_args) {
    cli::cli_abort(
      "epi_slide: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead."
    )
  }
  if ("names_sep" %in% provided_args) {
    cli::cli_abort(
      "epi_slide: the argument `names_sep` is deprecated. If NULL, you can remove it, it is now default.
      If a string, please manually prefix your column names instead."
    )
  }
  if ("before" %in% provided_args || "after" %in% provided_args) {
    cli::cli_abort(
      "epi_slide: `before` and `after` are deprecated for `epi_slide`. Use `.window_size` and `.align` instead.
      See the slide documentation for more details."
    )
  }

  # Validate arguments
  assert_class(.x, "epi_df")
  .x_orig_groups <- groups(.x)
  if (inherits(.x, "grouped_df")) {
    expected_group_keys <- .x %>%
      key_colnames(exclude = "time_value") %>%
      sort()
    if (!identical(.x %>% group_vars() %>% sort(), expected_group_keys)) {
      cli_abort(
        "`.x` must be either grouped by {expected_group_keys} or ungrouped; if the latter,
         we'll temporarily group by {expected_group_keys} for this operation. You may need
        to aggregate your data first; see sum_groups_epi_df().",
        class = "epiprocess__epi_slide__invalid_grouping"
      )
    }
  } else {
    .x <- group_epi_df(.x, exclude = "time_value")
  }
  if (nrow(.x) == 0L) {
    return(.x)
  }
  # If `.f` is missing, interpret ... as an expression for tidy evaluation
  if (missing(.f)) {
    used_data_masking <- TRUE
    quosures <- enquos(...)
    if (length(quosures) == 0) {
      cli_abort("If `.f` is missing then a computation must be specified via `...`.")
    }

    .f <- quosures
    .f_arg <- ".f" # dummy val, shouldn't be used since we're not using `.f`
    # Magic value that passes zero args as dots in calls below. Equivalent to
    # `... <- missing_arg()`, but `assign` avoids warning about improper use of
    # dots.
    assign("...", missing_arg())
  } else {
    used_data_masking <- FALSE
    .f_arg <- caller_arg(.f)
  }
  .slide_comp <- as_time_slide_computation(.f, ..., .f_arg = .f_arg)

  .align <- rlang::arg_match(.align)
  time_type <- attr(.x, "metadata")$time_type
  if (is.null(.window_size)) {
    cli_abort("epi_slide: `.window_size` must be specified.")
  }
  validate_slide_window_arg(.window_size, time_type)
  window_args <- get_before_after_from_window(.window_size, .align, time_type)

  if (is.null(.ref_time_values)) {
    .ref_time_values <- unique(.x$time_value)
  } else {
    assert_numeric(.ref_time_values, min.len = 1L, null.ok = FALSE, any.missing = FALSE, unique = TRUE)
    if (!test_subset(.ref_time_values, unique(.x$time_value))) {
      cli_abort(
        "epi_slide: `ref_time_values` must be a unique subset of the time values in `x`.",
        class = "epiprocess__epi_slide_invalid_ref_time_values"
      )
    }
  }
  .ref_time_values <- sort(.ref_time_values)

  assert_character(.new_col_name, null.ok = TRUE)
  if (!is.null(.new_col_name)) {
    if (.new_col_name %in% names(.x)) {
      cli_abort(c("`.new_col_name` cannot overlap with existing column names",
        "x" = "{sym(.new_col_name)} already exists in `.x`",
        ">" = "Try using a different `.new_col_name` instead."
      ))
    }
  }

  assert_logical(.all_rows, len = 1)

  # Check for duplicated time values within groups
  assert(check_ukey_unique(ungroup(.x), c(group_vars(.x), "time_value")))

  # Begin handling completion. This will create a complete time index between
  # the smallest and largest time values in the data. This is used to ensure
  # that the slide function is called with a complete window of data. Each slide
  # group will filter this down to between its min and max time values. We also
  # mark which dates were in the data and which were added by our completion.
  date_seq_list <- full_date_seq(.x, window_args$before, window_args$after, time_type)
  .x$.real <- TRUE

  # Create a wrapper that calculates and passes `.ref_time_value` to the
  # computation. `i` is contained in the `slide_comp_wrapper_factory`
  # environment such that when called within `slide_one_grp` `i` advances
  # through the list of reference time values within a group and then resets
  # back to 1 when switching groups.
  slide_comp_wrapper_factory <- function(kept_ref_time_values) {
    i <- 1L
    slide_comp_wrapper <- function(.x, .group_key, ...) {
      .ref_time_value <- kept_ref_time_values[[i]]
      i <<- i + 1L
      .slide_comp(.x, .group_key, .ref_time_value, ...)
    }
    slide_comp_wrapper
  }

  # - If .x is not grouped, then the trivial group is applied:
  #   https://dplyr.tidyverse.org/reference/group_map.html
  # - We create a lambda that forwards the necessary slide arguments to
  #   `epi_slide_one_group`.
  # - `...` from top of `epi_slide` are forwarded to `.f` here through
  #   group_modify and through the lambda.
  result <- group_map(
    .x,
    .f = function(.data_group, .group_key, ...) {
      epi_slide_one_group(
        .data_group, .group_key, ...,
        .slide_comp_factory = slide_comp_wrapper_factory,
        .before = window_args$before,
        .after = window_args$after,
        .ref_time_values = .ref_time_values,
        .all_rows = .all_rows,
        .new_col_name = .new_col_name,
        .used_data_masking = used_data_masking,
        .time_type = time_type,
        .date_seq_list = date_seq_list
      )
    },
    ...,
    .keep = TRUE
  ) %>%
    list_rbind() %>%
    `[`(.$.real, names(.) != ".real") %>%
    arrange_col_canonical() %>%
    group_by(!!!.x_orig_groups)

  # If every group in epi_slide_one_group takes the
  # length(available_ref_time_values) == 0 branch then we end up here.
  if (ncol(result) == ncol(.x[names(.x) != ".real"])) {
    cli_abort(
      "epi_slide: no new columns were created. This can happen if every group has no available ref_time_values.
      This is likely a mistake in your data, in the slide computation, or in the ref_time_values argument.",
      class = "epiprocess__epi_slide_no_new_columns"
    )
  }
  return(result)
}

# Slide applied to one group.  See `?group_modify` for the expected structure. The dots
# `...` forward their inputs to the function `f`.
epi_slide_one_group <- function(
    .data_group, .group_key,
    ...,
    .slide_comp_factory, .before, .after, .ref_time_values, .all_rows,
    .new_col_name, .used_data_masking, .time_type, .date_seq_list) {
  available_ref_time_values <- .ref_time_values[
    .ref_time_values >= min(.data_group$time_value) & .ref_time_values <= max(.data_group$time_value)
  ]

  # Unpack the date_seq_list argument and complete the data group with missing
  # time values, padding on the left and right as needed.
  all_dates <- .date_seq_list$all_dates
  missing_times <- all_dates[!vec_in(all_dates, .data_group$time_value)]
  .data_group <- reclass(vec_rbind(
    .data_group,
    # (^ epi_df; epi_slide uses .keep = TRUE)
    # (v tibble -> vec_rbind outputs tibble)
    new_tibble(vec_recycle_common(
      !!!.group_key,
      time_value = c(
        missing_times,
        .date_seq_list$pad_early_dates,
        .date_seq_list$pad_late_dates
      ),
      .real = FALSE
    ))
    # we should be adding time values of the same time_type (and shouldn't be
    # introducing duplicate epikeytime values); we can reclass without checks:
  ), attr(.data_group, "metadata")) %>%
    `[`(vec_order(.$time_value), )

  # If the data group does not contain any of the reference time values, return
  # the original .data_group without slide columns and let vec_rbind at the end
  # of group_modify handle filling the empty data frame with NA values.
  if (length(available_ref_time_values) == 0L) {
    if (.all_rows) {
      return(.data_group)
    }
    return(.data_group[0, ])
  }

  # Get stateful function that tracks ref_time_value per group and sends it to
  # `f` when called.
  .slide_comp <- .slide_comp_factory(available_ref_time_values)

  if (.time_type == "yearmonth" && identical(.before, Inf)) {
    # <yearmonth> - Inf is NA(s) rather than -Inf as a yearmonth; feed in -Inf manually
    # (it will successfully be cast to -Inf as a yearmonth)
    starts <- rep(-Inf, length(available_ref_time_values))
    stops <- available_ref_time_values + .after
  } else {
    starts <- available_ref_time_values - .before
    stops <- available_ref_time_values + .after
  }

  # Compute the slide values. slider::hop_index will return a list of f outputs
  # e.g. list(f(.slide_group_1, .group_key, .ref_time_value_1),
  # f(.slide_group_1, .group_key, .ref_time_value_2), ...)
  slide_values_list <- slider::hop_index(
    .x = .data_group,
    .i = .data_group$time_value,
    .starts = starts,
    .stops = stops,
    .f = .slide_comp,
    .group_key, ...
  )

  # Validate returned values. This used to only happen when
  # .used_data_masking=FALSE, so if it seems too slow, consider bringing that
  # back.
  return_types <- purrr::map_chr(slide_values_list, function(x) {
    if (is.data.frame(x)) {
      "data.frame"
    } else if (vctrs::obj_is_vector(x) && is.null(vctrs::vec_names(x))) {
      "vector"
    } else {
      "other"
    }
  }) %>% unique()
  # Returned values must be data.frame or vector.
  if ("other" %in% return_types) {
    cli_abort(
      "epi_slide: slide computations must always return either data frames without rownames
      or unnamed vectors (as determined by the vctrs package).",
      class = "epiprocess__invalid_slide_comp_value"
    )
  }
  # Returned values must all be the same type.
  if (length(return_types) != 1L) {
    cli_abort(
      "epi_slide: slide computations must always return either a data.frame or a vector (as determined by the
      vctrs package), but not a mix of the two.",
      class = "epiprocess__invalid_slide_comp_value"
    )
  }
  # Returned values must always be a scalar vector or a data frame with one row.
  if (any(vctrs::list_sizes(slide_values_list) != 1L)) {
    cli_abort(
      "epi_slide: slide computations must return a single element (e.g. a scalar value, a single data.frame row,
      or a list).",
      class = "epiprocess__invalid_slide_comp_value"
    )
  }
  # Flatten the output list. This will also error if the user's slide function
  # returned inconsistent types.
  slide_values <- slide_values_list %>% vctrs::list_unchop()

  # If all rows, then pad slide values with NAs, else filter down data group
  if (.all_rows) {
    orig_values <- slide_values
    slide_values <- vec_rep(vec_cast(NA, orig_values), nrow(.data_group))
    vec_slice(slide_values, vec_in(.data_group$time_value, available_ref_time_values)) <- orig_values
  } else {
    .data_group <- .data_group[vec_in(.data_group$time_value, available_ref_time_values), ]
  }

  # To label the result, we will parallel some code from `epix_slide`, though
  # some logic is different and some optimizations are less likely to be
  # needed as we're at a different loop depth.

  # Unlike `epix_slide`, we will not every have to deal with a 0-row
  # `.group_key`: we return early if `epi_slide`'s `.x` has 0 rows, and our
  # loop over groups is the outer loop (>= 1 row into the group loop ensures
  # we will have only 1-row `.group_key`s). Further, unlike `epix_slide`, we
  # actually will be using `.group_data` rather than work with `.group_key` at
  # all, in order to keep the pre-existing non-key columns. We will also try
  # to work directly with `epi_df`s instead of listified tibbles; since we're
  # not in as tight of a loop, the increased overhead hopefully won't matter.
  # We'll need to use `bind_cols` rather than `c` to avoid losing
  # `epi_df`ness.

  res <- .data_group

  if (is.null(.new_col_name)) {
    if (inherits(slide_values, "data.frame")) {
      # Sometimes slide_values can parrot back columns already in `res`; allow
      # this, but balk if a column has the same name as one in `res` but a
      # different value:
      comp_nms <- names(slide_values)
      overlaps_existing_names <- comp_nms %in% names(res)
      for (comp_i in which(overlaps_existing_names)) {
        if (!identical(slide_values[[comp_i]], res[[comp_nms[[comp_i]]]])) {
          lines <- c(
            cli::format_error(c(
              "New column and old column clash",
              "x" = "slide computation output included a
                     {format_varname(comp_nms[[comp_i]])} column, but `.x` already had a
                     {format_varname(comp_nms[[comp_i]])} column with differing values",
              "Here are examples of differing values, where the grouping variables were
               {format_tibble_row(.group_key)}:"
            )),
            capture.output(print(waldo::compare(
              res[[comp_nms[[comp_i]]]], slide_values[[comp_i]],
              x_arg = rlang::expr_deparse(dplyr::expr(`$`(!!"existing", !!sym(comp_nms[[comp_i]])))), # nolint: object_usage_linter
              y_arg = rlang::expr_deparse(dplyr::expr(`$`(!!"comp_value", !!sym(comp_nms[[comp_i]])))) # nolint: object_usage_linter
            ))),
            cli::format_message(c(
              ">" = "You likely want to rename or remove this column from your slide
                     computation's output, or debug why it has a different value."
            ))
          )
          rlang::abort(paste(collapse = "\n", lines),
            class = "epiprocess__epi_slide_output_vs_existing_column_conflict"
          )
        }
      }
      # Unpack into separate columns (without name prefix). If there are
      # columns duplicating existing columns, de-dupe and order them as if they
      # didn't exist in slide_values.
      res <- dplyr::bind_cols(res, slide_values[!overlaps_existing_names])
    } else {
      # Apply default name (to vector or packed data.frame-type column):
      if ("slide_value" %in% names(res)) {
        cli_abort(c("Cannot guess a good column name for your output",
          "x" = "`slide_value` already exists in `.x`",
          ">" = "Please provide a `.new_col_name`."
        ))
      }
      res[["slide_value"]] <- slide_values
    }
  } else {
    # Vector or packed data.frame-type column (note: overlaps with existing
    # column names should already be forbidden by earlier validation):
    res[[.new_col_name]] <- slide_values
  }

  res
}

get_before_after_from_window <- function(window_size, align, time_type) {
  if (identical(window_size, Inf)) {
    if (align == "right") {
      before <- Inf
      # styler: off
      after <- switch(time_type,
        day = , week = as.difftime(0, units = glue::glue("{time_type}s")),
        yearmonth = , integer = 0L,
        cli_abort("Unrecognized time_type: {time_type}.")
      )
      # styler: on
    } else {
      cli_abort(
        "`epi_slide`: center and left alignment are not supported with an infinite window size."
      )
    }
  } else {
    if (align == "right") {
      before <- window_size - 1
      after <- 0
    } else if (align == "center") {
      # For window_size = 5, before = 2, after = 2. For window_size = 4, before = 2, after = 1.
      before <- floor(window_size / 2)
      after <- window_size - before - 1
    } else if (align == "left") {
      before <- 0
      after <- window_size - 1
    }
  }
  list(before = before, after = after)
}

#' Calculate rolling or running means, sums, etc., or custom calculations
#'
#' @description These methods take each subpopulation (i.e., a single
#'   `geo_value` and combination of any `other_keys` you set up for age groups,
#'   etc.) and perform a `.window_size`-width time window rolling/sliding
#'   computation, or alternatively, a running/cumulative computation (with
#'   `.window_size = Inf`) on the requested columns. Explicit `NA` measurements
#'   are temporarily added to fill in any time gaps, and, for rolling
#'   computations, to pad the time series to ensure that the first & last
#'   computations use exactly `.window_size` values.
#'
#' `epi_slide_opt` allows you to use any [data.table::froll] or
#' [slider::summary-slide] function. If none of those specialized functions fit
#' your usecase, you can use `data.table::frollapply` together with a non-rolling
#' function (e.g., `median`). See [`epi_slide`] if you need to work with
#' multiple columns at once or output a custom type.
#'
#' @template basic-slide-params
#' @param .col_names <[`tidy-select`][dplyr_tidy_select]> An unquoted column
#'   name (e.g., `cases`), multiple column names (e.g., `c(cases, deaths)`),
#'   [other tidy-select expression][tidyselect::language], or a vector of
#'   characters (e.g. `c("cases", "deaths")`). Variable names can be used as if
#'   they were positions in the data frame, so expressions like `x:y` can be
#'   used to select a range of variables.
#'
#'   The tidy-selection renaming interface is not supported, and cannot be used
#'   to provide output column names; if you want to customize the output column
#'   names, use [`dplyr::rename`] after the slide.
#' @param .f Function; together with `...` specifies the computation to slide.
#'  `.f` must be one of `data.table`'s rolling functions
#'  (`frollmean`, `frollsum`, `frollapply`. See [data.table::roll]) or one
#'  of `slider`'s specialized sliding functions (`slide_mean`, `slide_sum`,
#'  etc. See [slider::summary-slide]).
#'
#'  The optimized `data.table` and `slider` functions can't be directly passed
#'  as the computation function in `epi_slide` without careful handling to make
#'  sure each computation group is made up of the `.window_size` dates rather
#'  than `.window_size` points. `epi_slide_opt` (and wrapper functions
#'  `epi_slide_mean` and `epi_slide_sum`) take care of window completion
#'  automatically to prevent associated errors.
#' @param ... Additional arguments to pass to the slide computation `.f`, for
#'   example, `algo` or `na.rm` in data.table functions. You don't need to
#'   specify `.x`, `.window_size`, or `.align` (or `before`/`after` for slider
#'   functions).
#' @param .prefix Optional [`glue::glue`] format string; name the slide result
#'   column(s) by attaching this prefix to the corresponding input column(s).
#'   Some shorthand is supported for basing the output names on `.window_size`
#'   or other arguments; see "Prefix and suffix shorthand" below.
#' @param .suffix Optional [`glue::glue`] format string; like `.prefix`. The
#'   default naming behavior is equivalent to `.suffix =
#'   "_{.n}{.time_unit_abbr}{.align_abbr}{.f_abbr}"`. Can be used in combination
#'   with `.prefix`.
#' @param .new_col_names Optional character vector with length matching the
#'   number of input columns from `.col_names`; name the slide result column(s)
#'   with these names. Cannot be used in combination with `.prefix` and/or
#'   `.suffix`.
#'
#' @section Prefix and suffix shorthand:
#'
#' [`glue::glue`] format strings specially interpret content within curly
#' braces. E.g., `glue::glue("ABC{2 + 2}")` evaluates to `"ABC4"`. For `.prefix`
#' and `.suffix`, we provide `glue` with some additional variable bindings:
#'
#' - `{.n}` will be the number of time steps in the computation
#'    corresponding to the `.window_size`.
#' - `{.time_unit_abbr}` will be a lower-case letter corresponding to the
#'     `time_type` of `.x`
#' - `{.align_abbr}` will be `""` if `.align` is the default of `"right"`;
#'    otherwise, it will be the first letter of `.align`
#' - `{.f_abbr}` will be a character vector containing a short abbreviation
#'    for `.f` factoring in the input column type(s) for `.col_names`
#'
#' @importFrom dplyr mutate %>% arrange tibble select all_of
#' @importFrom rlang enquo expr_label caller_arg quo_get_env
#' @importFrom tidyselect eval_select
#' @importFrom glue glue
#' @importFrom purrr map map_lgl
#' @importFrom data.table frollmean frollsum frollapply
#' @importFrom lubridate as.period
#' @importFrom checkmate assert_function
#' @importFrom slider slide_sum slide_prod slide_mean slide_min slide_max slide_all slide_any
#' @export
#' @seealso [`epi_slide`] for the more general slide function
#' @examples
#' library(dplyr)
#'
#' # Add a column (`cases_7dsum`) containing a 7-day trailing sum on `cases`:
#' cases_deaths_subset %>%
#'   select(geo_value, time_value, cases) %>%
#'   epi_slide_sum(cases, .window_size = 7)
#'
#' # Add a column (`cases_rate_7dav`) containing a 7-day trailing average on `case_rate`:
#' covid_case_death_rates_extended %>%
#'   epi_slide_mean(case_rate, .window_size = 7)
#'
#' # Use a less common specialized slide function:
#' cases_deaths_subset %>%
#'   epi_slide_opt(cases, slider::slide_min, .window_size = 7)
#'
#' # Specify output column names and/or a naming scheme:
#' cases_deaths_subset %>%
#'   select(geo_value, time_value, cases) %>%
#'   group_by(geo_value) %>%
#'   epi_slide_sum(cases, .window_size = 7, .new_col_names = "case_sum") %>%
#'   ungroup()
#' cases_deaths_subset %>%
#'   select(geo_value, time_value, cases) %>%
#'   group_by(geo_value) %>%
#'   epi_slide_sum(cases, .window_size = 7, .prefix = "sum_") %>%
#'   ungroup()
#'
#' # Additional settings can be sent to the {data.table} and {slider} functions
#' # via `...`. This example passes some arguments to `frollmean` settings for
#' # speed, accuracy, and to allow partially-missing windows:
#' covid_case_death_rates_extended %>%
#'   epi_slide_mean(
#'     case_rate,
#'     .window_size = 7,
#'     na.rm = TRUE, algo = "exact", hasNA = TRUE
#'   )
#'
#' # If the more specialized possibilities for `.f` don't cover your needs, you
#' # can use `epi_slide_opt` with `.f = data.table::frollapply` to apply a
#' # custom function at the cost of more computation time. See also `epi_slide`
#' # if you need something even more general.
#' cases_deaths_subset %>%
#'   select(geo_value, time_value, case_rate_7d_av, death_rate_7d_av) %>%
#'   epi_slide_opt(c(case_rate_7d_av, death_rate_7d_av),
#'     data.table::frollapply,
#'     FUN = median, .window_size = 28,
#'     .suffix = "_{.n}{.time_unit_abbr}_median"
#'   ) %>%
#'   print(n = 40)
epi_slide_opt <- function(
    .x, .col_names, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
    .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
    .ref_time_values = NULL, .all_rows = FALSE) {
  assert_class(.x, "epi_df")

  # Deprecated argument handling
  provided_args <- rlang::call_args_names(rlang::call_match())
  if (any(purrr::map_lgl(provided_args, ~ .x %in% c("x", "col_names", "f", "ref_time_values", "all_rows")))) {
    cli::cli_abort(
      "epi_slide_opt: you are using one of the following old argument names: `x`, `col_names`, `f`, `ref_time_values`,
      or `all_rows`. Please use the new dot-prefixed names: `.x`, `.col_names`, `.f`,
      `.ref_time_values`, `.all_rows`."
    )
  }
  if ("as_list_col" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead."
    )
  }
  if ("before" %in% provided_args || "after" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: `before` and `after` are deprecated for `epi_slide`. Use `.window_size` and `.align` instead.
      See the slide documentation for more details."
    )
  }
  if ("new_col_name" %in% provided_args || ".new_col_name" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: the argument `new_col_name` is not supported for `epi_slide_opt`. If you want to customize
      the output column names, use `.prefix =`, `.suffix =`, or `.new_col_**names** =`.",
      class = "epiprocess__epi_slide_opt__new_name_not_supported"
    )
  }
  if ("names_sep" %in% provided_args || ".names_sep" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: the argument `names_sep` is not supported for `epi_slide_opt`. If you want to customize
      the output column names, use `dplyr::rename` after the slide.",
      class = "epiprocess__epi_slide_opt__name_sep_not_supported"
    )
  }

  assert_class(.x, "epi_df")
  .x_orig_groups <- groups(.x)
  if (inherits(.x, "grouped_df")) {
    expected_group_keys <- .x %>%
      key_colnames(exclude = "time_value") %>%
      sort()
    if (!identical(.x %>% group_vars() %>% sort(), expected_group_keys)) {
      cli_abort(
        "`.x` must be either grouped by {expected_group_keys} or ungrouped; if the latter,
         we'll temporarily group by {expected_group_keys} for this operation. You may need
        to aggregate your data first; see sum_groups_epi_df().",
        class = "epiprocess__epi_slide_opt__invalid_grouping"
      )
    }
  } else {
    .x <- group_epi_df(.x, exclude = "time_value")
  }
  if (nrow(.x) == 0L) {
    cli_abort(
      c(
        "input data `.x` unexpectedly has 0 rows",
        "i" = "If this computation is occuring within an `epix_slide` call,
          check that `epix_slide` `.versions` argument was set appropriately
          so that you don't get any completely-empty snapshots"
      ),
      class = "epiprocess__epi_slide_opt__0_row_input",
      epiprocess__x = .x
    )
  }

  # Check for duplicated time values within groups
  assert(check_ukey_unique(ungroup(.x), c(group_vars(.x), "time_value")))

  # The position of a given column can be differ between input `.x` and
  # `.data_group` since the grouping step by default drops grouping columns.
  # To avoid rerunning `eval_select` for every `.data_group`, convert
  # positions of user-provided `col_names` into string column names. We avoid
  # using `names(pos)` directly for robustness and in case we later want to
  # allow users to rename fields via tidyselection.
  col_names_quo <- enquo(.col_names)
  pos <- eval_select(col_names_quo, data = .x, allow_rename = FALSE)
  col_names_chr <- names(.x)[pos]

  # Check that slide function `.f` is one of those short-listed from
  # `data.table` and `slider` (or a function that has the exact same definition,
  # e.g. if the function has been reexported or defined locally). Extract some
  # metadata. `namer` will be mapped over columns (.x will be a column, not the
  # entire edf).
  f_possibilities <-
    tibble::tribble(
      ~f, ~package, ~namer,
      frollmean, "data.table", ~ if (is.logical(.x)) "prop" else "av",
      frollsum, "data.table", ~ if (is.logical(.x)) "count" else "sum",
      frollapply, "data.table", ~"slide",
      slide_sum, "slider", ~ if (is.logical(.x)) "count" else "sum",
      slide_prod, "slider", ~"prod",
      slide_mean, "slider", ~ if (is.logical(.x)) "prop" else "av",
      slide_min, "slider", ~"min",
      slide_max, "slider", ~"max",
      slide_all, "slider", ~"all",
      slide_any, "slider", ~"any",
    )
  f_info <- f_possibilities %>%
    filter(map_lgl(.data$f, ~ identical(.f, .x)))
  if (nrow(f_info) == 0L) {
    # `f` is from somewhere else and not supported
    cli_abort(
      c(
        "problem with {rlang::expr_label(rlang::caller_arg(f))}",
        "i" = "`f` must be one of `data.table`'s rolling functions (`frollmean`,
              `frollsum`, `frollapply`. See `?data.table::roll`) or one of
              `slider`'s specialized sliding functions (`slide_mean`, `slide_sum`,
              etc. See `?slider::\`summary-slide\`` for more options)."
      ),
      class = "epiprocess__epi_slide_opt__unsupported_slide_function",
      epiprocess__f = .f
    )
  }
  if (nrow(f_info) > 1L) {
    cli_abort('epiprocess internal error: looking up `.f` in table of possible
               functions yielded multiple matches. Please report it using "New
               issue" at https://github.com/cmu-delphi/epiprocess/issues, using
               reprex::reprex to provide a minimal reproducible example.')
  }
  f_from_package <- f_info$package

  user_provided_rtvs <- !is.null(.ref_time_values)
  if (!user_provided_rtvs) {
    .ref_time_values <- unique(.x$time_value)
  } else {
    assert_numeric(.ref_time_values, min.len = 1L, null.ok = FALSE, any.missing = FALSE)
    if (!test_subset(.ref_time_values, unique(.x$time_value))) {
      cli_abort(
        "`ref_time_values` must be a unique subset of the time values in `x`.",
        class = "epiprocess__epi_slide_opt_invalid_ref_time_values"
      )
    }
    if (anyDuplicated(.ref_time_values) != 0L) {
      cli_abort(
        "`ref_time_values` must not contain any duplicates; use `unique` if appropriate.",
        class = "epiprocess__epi_slide_opt_invalid_ref_time_values"
      )
    }
  }
  ref_time_values <- sort(.ref_time_values)

  # Handle window arguments
  .align <- rlang::arg_match(.align)
  time_type <- attr(.x, "metadata")$time_type
  if (is.null(.window_size)) {
    cli_abort("epi_slide_opt: `.window_size` must be specified.")
  }
  validate_slide_window_arg(.window_size, time_type)
  window_args <- get_before_after_from_window(.window_size, .align, time_type)

  # Handle output naming
  if ((!is.null(.prefix) || !is.null(.suffix)) && !is.null(.new_col_names)) {
    cli_abort(
      "Can't use both .prefix/.suffix and .new_col_names at the same time.",
      class = "epiprocess__epi_slide_opt_incompatible_naming_args"
    )
  }
  assert_string(.prefix, null.ok = TRUE)
  assert_string(.suffix, null.ok = TRUE)
  assert_character(.new_col_names, len = length(col_names_chr), null.ok = TRUE)
  if (is.null(.prefix) && is.null(.suffix) && is.null(.new_col_names)) {
    .suffix <- "_{.n}{.time_unit_abbr}{.align_abbr}{.f_abbr}"
    # ^ does not account for any arguments specified to underlying functions via
    # `...` such as `na.rm =`, nor does it distinguish between functions from
    # different packages accomplishing the same type of computation. Those are
    # probably only set one way per task, so this probably produces cleaner
    # names without clashes (though maybe some confusion if switching between
    # code with different settings).
  }
  if (!is.null(.prefix) || !is.null(.suffix)) {
    .prefix <- .prefix %||% ""
    .suffix <- .suffix %||% ""
    if (identical(.window_size, Inf)) {
      n <- "running_"
      time_unit_abbr <- ""
      align_abbr <- ""
    } else {
      n <- time_delta_to_n_steps(.window_size, time_type)
      time_unit_abbr <- time_type_unit_abbr(time_type)
      align_abbr <- c(right = "", center = "c", left = "l")[[.align]]
    }
    glue_env <- rlang::env(
      .n = n,
      .time_unit_abbr = time_unit_abbr,
      .align_abbr = align_abbr,
      .f_abbr = purrr::map_chr(.x[col_names_chr], unwrap(f_info$namer)),
      quo_get_env(col_names_quo)
    )
    .new_col_names <- unclass(
      glue(.prefix, .envir = glue_env) +
        col_names_chr +
        glue(.suffix, .envir = glue_env)
    )
  } else {
    # `.new_col_names` was provided by user; we don't need to do anything.
  }
  if (any(.new_col_names %in% names(.x))) {
    cli_abort(c(
      "Naming conflict between new columns and existing columns",
      "x" = "Overlapping names: {format_varnames(intersect(.new_col_names, names(.x)))}"
    ), class = "epiprocess__epi_slide_opt_old_new_name_conflict")
  }
  if (anyDuplicated(.new_col_names)) {
    cli_abort(c(
      "New column names contain duplicates",
      "x" = "Duplicated names: {format_varnames(unique(.new_col_names[duplicated(.new_col_names)]))}"
    ), class = "epiprocess__epi_slide_opt_new_name_duplicated")
  }
  result_col_names <- .new_col_names

  # Make a complete date sequence between min(.x$time_value) and max(.x$time_value).
  date_seq_list <- full_date_seq(.x, window_args$before, window_args$after, time_type)
  all_dates <- date_seq_list$all_dates
  pad_early_dates <- date_seq_list$pad_early_dates
  pad_late_dates <- date_seq_list$pad_late_dates

  slide_one_grp <- function(.data_group, .group_key, ...) {
    missing_times <- all_dates[!vec_in(all_dates, .data_group$time_value)]
    # `frollmean` requires a full window to compute a result. Add NA values
    # to beginning and end of the group so that we get results for the
    # first `before` and last `after` elements.
    .data_group <- vec_rbind(
      .data_group, # (tibble; epi_slide_opt uses .keep = FALSE)
      new_tibble(vec_recycle_common(
        time_value = c(missing_times, pad_early_dates, pad_late_dates),
        .real = FALSE
      ))
    ) %>%
      `[`(vec_order(.$time_value), )

    if (f_from_package == "data.table") {
      # Grouping should ensure that we don't have duplicate time values.
      # Completion above should ensure we have at least .window_size rows. Check
      # that we don't have more than .window_size rows (or fewer somehow):
      if (nrow(.data_group) != length(c(all_dates, pad_early_dates, pad_late_dates))) {
        cli_abort(
          c(
            "group contains an unexpected number of rows",
            "i" = c("Input data may contain `time_values` closer together than the
              expected `time_step` size")
          ),
          class = "epiprocess__epi_slide_opt__unexpected_row_number",
          epiprocess__data_group = .data_group,
          epiprocess__group_key = .group_key
        )
      }

      # `frollmean` is 1-indexed, so create a new window width based on our
      # `before` and `after` params. Right-aligned `frollmean` results'
      # `ref_time_value`s will be `after` timesteps ahead of where they should
      # be; shift results to the left by `after` timesteps.
      if (window_args$before != Inf) {
        window_size <- window_args$before + window_args$after + 1L
        roll_output <- .f(x = .data_group[, col_names_chr], n = window_size, ...)
      } else {
        window_size <- list(seq_along(.data_group$time_value))
        roll_output <- .f(x = .data_group[, col_names_chr], n = window_size, adaptive = TRUE, ...)
      }
      if (window_args$after >= 1) {
        .data_group[, result_col_names] <- purrr::map(roll_output, function(.x) {
          c(.x[(window_args$after + 1L):length(.x)], rep(NA, window_args$after))
        })
      } else {
        .data_group[, result_col_names] <- roll_output
      }
    }
    if (f_from_package == "slider") {
      for (i in seq_along(col_names_chr)) {
        .data_group[, result_col_names[i]] <- .f(
          x = .data_group[[col_names_chr[i]]],
          before = as.numeric(window_args$before),
          after = as.numeric(window_args$after),
          ...
        )
      }
    }

    .data_group
  }

  result <- .x %>%
    `[[<-`(".real", value = TRUE) %>%
    group_modify(slide_one_grp, ..., .keep = FALSE) %>%
    `[`(.$.real, names(.) != ".real") %>%
    arrange_col_canonical() %>%
    group_by(!!!.x_orig_groups)

  if (.all_rows) {
    result[!vec_in(result$time_value, ref_time_values), result_col_names] <- NA
  } else if (user_provided_rtvs) {
    result <- result[vec_in(result$time_value, ref_time_values), ]
  }

  if (!is_epi_df(result)) {
    # `.all_rows` handling strips epi_df format and metadata.
    # Restore them.
    result <- reclass(result, attributes(.x)$metadata)
  }

  return(result)
}

#' @rdname epi_slide_opt
#' @description `epi_slide_mean` is a wrapper around `epi_slide_opt` with `.f =
#' data.table::frollmean`.
#'
#' @export
epi_slide_mean <- function(
    .x, .col_names, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
    .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
    .ref_time_values = NULL, .all_rows = FALSE) {
  # Deprecated argument handling
  provided_args <- rlang::call_args_names(rlang::call_match())
  if (any(purrr::map_lgl(provided_args, ~ .x %in% c("x", "col_names", "f", "ref_time_values", "all_rows")))) {
    cli::cli_abort(
      "epi_slide_mean: you are using one of the following old argument names: `x`, `col_names`, `f`, `ref_time_values`,
      or `all_rows`. Please use the new dot-prefixed names: `.x`, `.col_names`, `.f`,
      `.ref_time_values`, `.all_rows`."
    )
  }
  if ("as_list_col" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_mean: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead."
    )
  }
  if ("before" %in% provided_args || "after" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_mean: `before` and `after` are deprecated for `epi_slide`. Use `.window_size` and `.align` instead.
      See the slide documentation for more details."
    )
  }
  if ("new_col_name" %in% provided_args || ".new_col_name" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: the argument `new_col_name` is not supported for `epi_slide_opt`. If you want to customize
      the output column names, use `.prefix =`, `.suffix =`, or `.new_col_**names** =`.",
      class = "epiprocess__epi_slide_opt__new_name_not_supported"
    )
  }
  if ("names_sep" %in% provided_args || ".names_sep" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_mean: the argument `names_sep` is not supported. If you want to customize
      the output column names, use `dplyr::rename` after the slide."
    )
  }

  epi_slide_opt(
    .x = .x,
    .col_names = {{ .col_names }},
    .f = data.table::frollmean,
    ...,
    .window_size = .window_size,
    .align = .align,
    .prefix = .prefix,
    .suffix = .suffix,
    .new_col_names = .new_col_names,
    .ref_time_values = .ref_time_values,
    .all_rows = .all_rows
  )
}

#' @rdname epi_slide_opt
#' @description `epi_slide_sum` is a wrapper around `epi_slide_opt` with `.f =
#' data.table::frollsum`.
#'
#' @export
epi_slide_sum <- function(
    .x, .col_names, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
    .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
    .ref_time_values = NULL, .all_rows = FALSE) {
  # Deprecated argument handling
  provided_args <- rlang::call_args_names(rlang::call_match())
  if (any(purrr::map_lgl(provided_args, ~ .x %in% c("x", "col_names", "f", "ref_time_values", "all_rows")))) {
    cli::cli_abort(
      "epi_slide_sum: you are using one of the following old argument names: `x`, `col_names`, `f`, `ref_time_values`,
      or `all_rows`. Please use the new dot-prefixed names: `.x`, `.col_names`, `.f`,
      `.ref_time_values`, `.all_rows`."
    )
  }
  if ("as_list_col" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_sum: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead."
    )
  }
  if ("before" %in% provided_args || "after" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_sum: `before` and `after` are deprecated for `epi_slide`. Use `.window_size` and `.align` instead.
      See the slide documentation for more details."
    )
  }
  if ("new_col_name" %in% provided_args || ".new_col_name" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_opt: the argument `new_col_name` is not supported for `epi_slide_opt`. If you want to customize
      the output column names, use `.prefix =`, `.suffix =`, or `.new_col_**names** =`.",
      class = "epiprocess__epi_slide_opt__new_name_not_supported"
    )
  }
  if ("names_sep" %in% provided_args || ".names_sep" %in% provided_args) {
    cli::cli_abort(
      "epi_slide_sum: the argument `names_sep` is not supported. If you want to customize
      the output column names, use `dplyr::rename` after the slide."
    )
  }
  epi_slide_opt(
    .x = .x,
    .col_names = {{ .col_names }},
    .f = data.table::frollsum,
    ...,
    .window_size = .window_size,
    .align = .align,
    .prefix = .prefix,
    .suffix = .suffix,
    .new_col_names = .new_col_names,
    .ref_time_values = .ref_time_values,
    .all_rows = .all_rows
  )
}

#' Make a complete date sequence between min(x$time_value) and max
#' (x$time_value). Produce lists of dates before min(x$time_value) and after
#' max(x$time_value) for padding initial and final windows to size `n`.
#'
#' `before` and `after` args are assumed to have been validated by the calling
#' function (using `validate_slide_window_arg`).
#'
#' @importFrom checkmate assert_function
#' @keywords internal
full_date_seq <- function(x, before, after, time_type) {
  if (!time_type %in% c("day", "week", "yearmonth", "integer")) {
    cli_abort(
      "time_type must be one of 'day', 'week', or 'integer'."
    )
  }

  pad_early_dates <- c()
  pad_late_dates <- c()

  # `tsibble` time types have their own behavior, where adding 1 corresponds to
  # incrementing by a quantum (smallest resolvable unit) of the date class. For
  # example, one step = 1 quarter for `yearquarter`.
  if (time_type %in% c("yearmonth", "integer")) {
    all_dates <- seq(min(x$time_value), max(x$time_value), by = 1L)

    if (before != 0 && before != Inf) {
      pad_early_dates <- all_dates[1L] - before:1
    }
    if (after != 0) {
      pad_late_dates <- all_dates[length(all_dates)] + 1:after
    }
  } else {
    by <- switch(time_type,
      day = "days",
      week = "weeks",
    )

    all_dates <- seq(min(x$time_value), max(x$time_value), by = by)
    if (before != 0 && before != Inf) {
      # The behavior is analogous to the branch with tsibble types above. For
      # more detail, note that the function `seq.Date(from, ..., length.out =
      # n)` returns `from + 0:n`. Since we want `from + 1:n`, we drop the first
      # element. Adding "-1" to the `by` arg makes `seq.Date` go backwards in
      # time.
      pad_early_dates <- sort(seq(all_dates[1L], by = paste("-1", by), length.out = before + 1)[-1])
    }
    if (after != 0) {
      pad_late_dates <- seq(all_dates[length(all_dates)], by = by, length.out = after + 1)[-1]
    }
  }

  list(
    all_dates = all_dates,
    pad_early_dates = pad_early_dates,
    pad_late_dates = pad_late_dates
  )
}
