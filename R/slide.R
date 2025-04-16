#' Slide a function over variables in an `epi_df` object
#'
#' @description Slides a given function over variables in an `epi_df` object.
#' This is useful for computations like rolling averages. The function supports
#' many ways to specify the computation, but by far the most common use case is
#' as follows:
#'
#' ```
#' # Create new column `cases_7dmed` that contains a 7-day trailing median of cases
#' epi_slide(edf, cases_7dmed = median(cases), .window_size = 7)
#' ```
#'
#' For two very common use cases, we provide optimized functions that are much
#' faster than `epi_slide`: `epi_slide_mean()` and `epi_slide_sum()`. We
#' recommend using these functions when possible.
#'
#' See `vignette("epi_df")` for more examples.
#'
#' @template basic-slide-params
#' @param .f Function, formula, or missing; together with `...` specifies the
#'   computation to slide. The return of the computation should either be a
#'   scalar or a 1-row data frame. Data frame returns will be
#'    `tidyr::unpack()`-ed, if named, and will be [`tidyr::pack`]-ed columns, if
#'    not named. See examples.
#'
#'   - If `.f` is missing, then `...` will specify the computation via
#'     tidy-evaluation. This is usually the most convenient way to use
#'     `epi_slide`. See examples.
#'   - If `.f` is a formula, then the formula should use `.x` (not the same as
#'     the input `epi_df`) to operate on the columns of the input `epi_df`, e.g.
#'     `~mean(.x$var)` to compute a mean of `var`.
#'   - If a function, `.f` must have the form `function(x, g, t, ...)`, where:
#'     - `x` is a data frame with the same column names as the original object,
#'     minus any grouping variables, with only the windowed data for one
#'     group-`.ref_time_value` combination
#'     - `g` is a one-row tibble containing the values of the grouping variables
#'     for the associated group
#'     - `t` is the `.ref_time_value` for the current window
#'     - `...` are additional arguments
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
#' # Get the 7-day trailing standard deviation of cases and the 7-day trailing mean of cases
#' cases_deaths_subset %>%
#'   epi_slide(
#'     cases_7sd = sd(cases, na.rm = TRUE),
#'     cases_7dav = mean(cases, na.rm = TRUE),
#'     .window_size = 7
#'   ) %>%
#'   select(geo_value, time_value, cases, cases_7sd, cases_7dav)
#' # Note that epi_slide_mean could be used to more quickly calculate cases_7dav.
#'
#' # In addition to the [`dplyr::mutate`]-like syntax, you can feed in a function or
#' # formula in a way similar to [`dplyr::group_modify`]:
#' my_summarizer <- function(window_data) {
#'   window_data %>%
#'     summarize(
#'       cases_7sd = sd(cases, na.rm = TRUE),
#'       cases_7dav = mean(cases, na.rm = TRUE)
#'     )
#' }
#' cases_deaths_subset %>%
#'   epi_slide(
#'     ~ my_summarizer(.x),
#'     .window_size = 7
#'   ) %>%
#'   select(geo_value, time_value, cases, cases_7sd, cases_7dav)
#'
#'
#'
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

  # # Begin handling completion. This will create a complete time index between
  # # the smallest and largest time values in the data. This is used to ensure
  # # that the slide function is called with a complete window of data. Each slide
  # # group will filter this down to between its min and max time values. We also
  # # mark which dates were in the data and which were added by our completion.
  # date_seq_list <- full_date_seq(.x, window_args$before, window_args$after, time_type)
  # .x$.real <- TRUE

  # # Create a wrapper that calculates and passes `.ref_time_value` to the
  # # computation. `i` is contained in the `slide_comp_wrapper_factory`
  # # environment such that when called within `slide_one_grp` `i` advances
  # # through the list of reference time values within a group and then resets
  # # back to 1 when switching groups.
  # slide_comp_wrapper_factory <- function(kept_ref_time_values) {
  #   i <- 1L
  #   slide_comp_wrapper <- function(.x, .group_key, ...) {
  #     .ref_time_value <- kept_ref_time_values[[i]]
  #     i <<- i + 1L
  #     .slide_comp(.x, .group_key, .ref_time_value, ...)
  #   }
  #   slide_comp_wrapper
  # }

  # # - If .x is not grouped, then the trivial group is applied:
  # #   https://dplyr.tidyverse.org/reference/group_map.html
  # # - We create a lambda that forwards the necessary slide arguments to
  # #   `epi_slide_one_group`.
  # # - `...` from top of `epi_slide` are forwarded to `.f` here through
  # #   group_modify and through the lambda.
  # result <- group_map(
  #   .x,
  #   .f = function(.data_group, .group_key, ...) {
  #     epi_slide_one_group(
  #       .data_group, .group_key, ...,
  #       .slide_comp_factory = slide_comp_wrapper_factory,
  #       .before = window_args$before,
  #       .after = window_args$after,
  #       .ref_time_values = .ref_time_values,
  #       .all_rows = .all_rows,
  #       .new_col_name = .new_col_name,
  #       .used_data_masking = used_data_masking,
  #       .time_type = time_type,
  #       .date_seq_list = date_seq_list
  #     )
  #   },
  #   ...,
  #   .keep = TRUE
  # ) %>%
  #   list_rbind() %>%
  #   `[`(.$.real, names(.) != ".real") %>%
  #   arrange_col_canonical() %>%
  #   group_by(!!!.x_orig_groups)
  before_n_steps <- time_delta_to_n_steps(window_args$before, time_type)
  after_n_steps <- time_delta_to_n_steps(window_args$after, time_type)
  unit_step <- unit_time_delta(time_type, format = "fast")
  simple_hop <- time_slide_to_simple_hop(.slide_comp = .slide_comp, ..., .before_n_steps = before_n_steps, .after_n_steps = after_n_steps)
  result <- .x %>%
    group_map(.keep = TRUE, function(grp_data, grp_key) {
      out_time_values <- ref_time_values_to_out_time_values(grp_data, .ref_time_values)
      res <- grp_data
      slide_values <- slide_window(grp_data, grp_key, simple_hop, before_n_steps, after_n_steps, unit_step, time_type, out_time_values)
      # FIXME check, de-dupe, simplify, refactor, ...
      if (.all_rows) {
        new_slide_values <- vec_cast(rep(NA, nrow(res)), slide_values)
        vec_slice(new_slide_values, vec_match(out_time_values, res$time_value)) <- slide_values
        slide_values <- new_slide_values
      } else {
        res <- vec_slice(res, vec_match(out_time_values, res$time_value))
      }

      # TODO refactor this out and use it in epix_slide as well if possible
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
    }) %>%
  list_rbind() %>%
  arrange_col_canonical() %>% # XXX is this desired?
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
      "epi_slide: slide computations must always return either data frames
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
