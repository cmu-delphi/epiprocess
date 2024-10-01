#' Slide a function over variables in an `epi_df` object
#'
#' Slides a given function over variables in an `epi_df` object. See the
#' [slide vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html)
#' for examples.
#'
#' @template basic-slide-params
#' @param .f Function, formula, or missing; together with `...` specifies the
#'   computation to slide. To "slide" means to apply a computation within a
#'   sliding (a.k.a. "rolling") time window for each data group. The window is
#'   determined by the `.window_size` and `.align` parameters, see the details
#'   section for more. If a function, `.f` must have the form `function(x, g, t,
#'   ...)`, where
#'
#'   - `x` is a data frame with the same column names as the original object,
#'   minus any grouping variables, with only the windowed data for one
#'   group-`.ref_time_value` combination
#'   - `g` is a one-row tibble containing the values of the grouping variables
#'   for the associated group
#'   - `t` is the `.ref_time_value` for the current window
#'   - `...` are additional arguments
#'
#'   If a formula, `.f` can operate directly on columns accessed via `.x$var` or
#'   `.$var`, as in `~mean(.x$var)` to compute a mean of a column `var` for each
#'   `ref_time_value`-group combination. The group key can be accessed via `.y`.
#'   If `.f` is missing, then `...` will specify the computation.
#' @param ... Additional arguments to pass to the function or formula specified
#'   via `.f`. Alternatively, if `.f` is missing, then the `...` is interpreted
#'   as a ["data-masking"][rlang::args_data_masking] expression or expressions
#'   for tidy evaluation; in addition to referring columns directly by name, the
#'   expressions have access to `.data` and `.env` pronouns as in `dplyr` verbs,
#'   and can also refer to `.x` (not the same as the input epi_df),
#'   `.group_key`, and `.ref_time_value`. See details.
#' @param .new_col_name String indicating the name of the new column that will
#'   contain the derivative values. The default is "slide_value" unless your
#'   slide computations output data frames, in which case they will be unpacked
#'   into the constituent columns and those names used. New columns should not
#'   be given names that clash with the existing columns of `.x`; see details.
#'
#' @template basic-slide-details
#'
#' @importFrom lubridate days weeks
#' @importFrom dplyr bind_rows group_map group_vars filter select
#' @importFrom rlang .data .env !! enquos sym env missing_arg
#' @export
#' @seealso [`epi_slide_opt`] [`epi_slide_mean`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' # Simple sliding means and sums are much faster to do using
#' # the `epi_slide_mean` and `epi_slide_sum` functions instead.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), .window_size = 7) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), .window_size = 7, .align = "left") %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 7-day center-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_7dav = mean(cases), .window_size = 7, .align = "center") %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav) %>%
#'   ungroup()
#'
#' # slide a 14-day center-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(cases_14dav = mean(cases), .window_size = 14, .align = "center") %>%
#'   dplyr::select(geo_value, time_value, cases, cases_14dav) %>%
#'   ungroup()
#'
#' # nested new columns
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide(
#'     cases_2d = list(data.frame(
#'       cases_2dav = mean(cases),
#'       cases_2dma = mad(cases)
#'     )),
#'     .window_size = 2
#'   ) %>%
#'   ungroup()
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
  if (checkmate::test_class(.x, "grouped_df")) {
    expected_group_keys <- .x %>%
      key_colnames(exclude = "time_value") %>%
      sort()
    if (!identical(.x %>% group_vars() %>% sort(), expected_group_keys)) {
      cli_abort(
        "epi_slide: `.x` must be either grouped by {expected_group_keys}. (Or you can just ungroup
        `.x` and we'll do this grouping automatically.) You may need to aggregate your data first,
        see aggregate_epi_df().",
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
    # Magic value that passes zero args as dots in calls below. Equivalent to
    # `... <- missing_arg()`, but `assign` avoids warning about improper use of
    # dots.
    assign("...", missing_arg())
  } else {
    used_data_masking <- FALSE
  }
  .slide_comp <- as_time_slide_computation(.f, ...)

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
  duplicated_time_values <- .x %>%
    group_epi_df() %>%
    filter(dplyr::n() > 1) %>%
    ungroup()
  if (nrow(duplicated_time_values) > 0) {
    bad_data <- capture.output(duplicated_time_values)
    cli_abort(
      "as_epi_df: some groups in a resulting dplyr computation have duplicated time values.
      epi_df requires a unique time_value per group.",
      body = c("Sample groups:", bad_data)
    )
  }

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
    return(slide_comp_wrapper)
  }

  # - If .x is not grouped, then the trivial group is applied:
  #   https://dplyr.tidyverse.org/reference/group_map.html
  # - We create a lambda that forwards the necessary slide arguments to
  #   `epi_slide_one_group`.
  # - `...` from top of `epi_slide` are forwarded to `.f` here through
  #   group_modify and through the lambda.
  .x_groups <- groups(.x)
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
    bind_rows() %>%
    filter(.real) %>%
    select(-.real) %>%
    arrange_col_canonical() %>%
    group_by(!!!.x_groups)

  # If every group in epi_slide_one_group takes the
  # length(available_ref_time_values) == 0 branch then we end up here.
  if (ncol(result) == ncol(.x %>% select(-.real))) {
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
  missing_times <- all_dates[!(all_dates %in% .data_group$time_value)]
  .data_group <- bind_rows(
    .data_group,
    dplyr::bind_cols(
      .group_key,
      tibble(
        time_value = c(
          missing_times,
          .date_seq_list$pad_early_dates,
          .date_seq_list$pad_late_dates
        ), .real = FALSE
      )
    )
  ) %>%
    arrange(.data$time_value)

  # If the data group does not contain any of the reference time values, return
  # the original .data_group without slide columns and let bind_rows at the end
  # of group_modify handle filling the empty data frame with NA values.
  if (length(available_ref_time_values) == 0L) {
    if (.all_rows) {
      return(.data_group)
    }
    return(.data_group %>% filter(FALSE))
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
      return("data.frame")
    } else if (vctrs::obj_is_vector(x) && is.null(vctrs::vec_names(x))) {
      return("vector")
    } else {
      return("other")
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
    slide_values <- vctrs::vec_rep(vctrs::vec_cast(NA, orig_values), nrow(.data_group))
    vctrs::vec_slice(slide_values, .data_group$time_value %in% available_ref_time_values) <- orig_values
  } else {
    .data_group <- .data_group %>% filter(time_value %in% available_ref_time_values)
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
              x_arg = rlang::expr_deparse(dplyr::expr(`$`(existing, !!sym(comp_nms[[comp_i]])))), # nolint: object_usage_linter
              y_arg = rlang::expr_deparse(dplyr::expr(`$`(comp_value, !!sym(comp_nms[[comp_i]])))) # nolint: object_usage_linter
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

  return(res)
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
  return(list(before = before, after = after))
}

#' Optimized slide function for performing common rolling computations on an
#' `epi_df` object
#'
#' Slides an n-timestep [data.table::froll] or [slider::summary-slide] function
#' over variables in an `epi_df` object. See the
#' [slide vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html)
#' for examples.
#'
#' @template basic-slide-params
#' @template opt-slide-params
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
#'  example, `algo` or `na.rm` in data.table functions. You don't need to
#'  specify `.x`, `.window_size`, or `.align` (or `before`/`after` for slider
#'  functions).
#' @template opt-slide-details
#'
#' @importFrom dplyr bind_rows mutate %>% arrange tibble select all_of
#' @importFrom rlang enquo quo_get_expr as_label expr_label caller_arg
#' @importFrom tidyselect eval_select
#' @importFrom purrr map map_lgl
#' @importFrom data.table frollmean frollsum frollapply
#' @importFrom lubridate as.period
#' @importFrom checkmate assert_function
#' @importFrom slider slide_sum slide_prod slide_mean slide_min slide_max slide_all slide_any
#' @export
#' @seealso [`epi_slide`] [`epi_slide_mean`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases. This can also be done with `epi_slide_mean`
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     .f = data.table::frollmean, .window_size = 7
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed, and rename new col
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day trailing average formula on cases. Adjust `frollmean` settings for speed
#' # and accuracy, and to allow partially-missing windows.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     .f = data.table::frollmean, .window_size = 7,
#'     # `frollmean` options
#'     algo = "exact", hasNA = TRUE, na.rm = TRUE
#'   ) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     .f = slider::slide_mean, .window_size = 7, .align = "left"
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day center-aligned sum. This can also be done with `epi_slide_sum`
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_opt(
#'     cases,
#'     .f = data.table::frollsum, .window_size = 6, .align = "center"
#'   ) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
epi_slide_opt <- function(
    .x, .col_names, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
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
      the output column names, use `dplyr::rename` after the slide.",
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

  if (nrow(.x) == 0L) {
    cli_abort(
      c(
        "input data `.x` unexpectedly has 0 rows",
        "i" = "If this computation is occuring within an `epix_slide` call,
          check that `epix_slide` `.versions` argument was set appropriately"
      ),
      class = "epiprocess__epi_slide_opt__0_row_input",
      epiprocess__x = .x
    )
  }

  # Check that slide function `.f` is one of those short-listed from
  # `data.table` and `slider` (or a function that has the exact same
  # definition, e.g. if the function has been reexported or defined
  # locally).
  if (any(map_lgl(
    list(frollmean, frollsum, frollapply),
    ~ identical(.f, .x)
  ))) {
    f_from_package <- "data.table"
  } else if (any(map_lgl(
    list(slide_sum, slide_prod, slide_mean, slide_min, slide_max, slide_all, slide_any),
    ~ identical(.f, .x)
  ))) {
    f_from_package <- "slider"
  } else {
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
  align <- rlang::arg_match(.align)
  time_type <- attr(.x, "metadata")$time_type
  validate_slide_window_arg(.window_size, time_type)
  if (identical(.window_size, Inf)) {
    if (align == "right") {
      before <- Inf
      if (time_type %in% c("day", "week")) {
        after <- as.difftime(0, units = glue::glue("{time_type}s"))
      } else {
        after <- 0
      }
    } else {
      cli_abort(
        "`epi_slide`: center and left alignment are not supported with an infinite window size."
      )
    }
  } else {
    if (align == "right") {
      before <- .window_size - 1
      if (time_type %in% c("day", "week")) {
        after <- as.difftime(0, units = glue::glue("{time_type}s"))
      } else {
        after <- 0
      }
    } else if (align == "center") {
      # For .window_size = 5, before = 2, after = 2. For .window_size = 4, before = 2, after = 1.
      before <- floor(.window_size / 2)
      after <- .window_size - before - 1
    } else if (align == "left") {
      if (time_type %in% c("day", "week")) {
        before <- as.difftime(0, units = glue::glue("{time_type}s"))
      } else {
        before <- 0
      }
      after <- .window_size - 1
    }
  }

  # Make a complete date sequence between min(.x$time_value) and max(.x$time_value).
  date_seq_list <- full_date_seq(.x, before, after, time_type)
  all_dates <- date_seq_list$all_dates
  pad_early_dates <- date_seq_list$pad_early_dates
  pad_late_dates <- date_seq_list$pad_late_dates

  # The position of a given column can be differ between input `.x` and
  # `.data_group` since the grouping step by default drops grouping columns.
  # To avoid rerunning `eval_select` for every `.data_group`, convert
  # positions of user-provided `col_names` into string column names. We avoid
  # using `names(pos)` directly for robustness and in case we later want to
  # allow users to rename fields via tidyselection.
  if (inherits(quo_get_expr(enquo(.col_names)), "character")) {
    pos <- eval_select(dplyr::all_of(.col_names), data = .x, allow_rename = FALSE)
  } else {
    pos <- eval_select(enquo(.col_names), data = .x, allow_rename = FALSE)
  }
  col_names_chr <- names(.x)[pos]
  # Always rename results to "slide_value_<original column name>".
  result_col_names <- paste0("slide_value_", col_names_chr)
  slide_one_grp <- function(.data_group, .group_key, ...) {
    missing_times <- all_dates[!(all_dates %in% .data_group$time_value)]
    # `frollmean` requires a full window to compute a result. Add NA values
    # to beginning and end of the group so that we get results for the
    # first `before` and last `after` elements.
    .data_group <- bind_rows(
      .data_group,
      tibble(time_value = c(missing_times, pad_early_dates, pad_late_dates), .real = FALSE)
    ) %>%
      arrange(.data$time_value)

    if (f_from_package == "data.table") {
      # If a group contains duplicate time values, `frollmean` will still only
      # use the last `k` obs. It isn't looking at dates, it just goes in row
      # order. So if the computation is aggregating across multiple obs for the
      # same date, `epi_slide_opt` and derivates will produce incorrect results;
      # `epi_slide` should be used instead.
      if (anyDuplicated(.data_group$time_value) != 0L) {
        cli_abort(
          c(
            "group contains duplicate time values. Using `epi_slide_[opt/mean/sum]` on this
              group will result in incorrect results",
            "i" = "Please change the grouping structure of the input data so that
              each group has non-duplicate time values (e.g. `x %>% group_by(geo_value)
              %>% epi_slide_opt(.f = frollmean)`)",
            "i" = "Use `epi_slide` to aggregate across groups"
          ),
          class = "epiprocess__epi_slide_opt__duplicate_time_values",
          epiprocess__data_group = .data_group,
          epiprocess__group_key = .group_key
        )
      }

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
      if (before != Inf) {
        window_size <- before + after + 1L
        roll_output <- .f(x = .data_group[, col_names_chr], n = window_size, ...)
      } else {
        window_size <- list(seq_along(.data_group$time_value))
        roll_output <- .f(x = .data_group[, col_names_chr], n = window_size, adaptive = TRUE, ...)
      }
      if (after >= 1) {
        .data_group[, result_col_names] <- purrr::map(roll_output, function(.x) {
          c(.x[(after + 1L):length(.x)], rep(NA, after))
        })
      } else {
        .data_group[, result_col_names] <- roll_output
      }
    }
    if (f_from_package == "slider") {
      for (i in seq_along(col_names_chr)) {
        .data_group[, result_col_names[i]] <- .f(
          x = .data_group[[col_names_chr[i]]],
          before = as.numeric(before),
          after = as.numeric(after),
          ...
        )
      }
    }

    return(.data_group)
  }

  result <- mutate(.x, .real = TRUE) %>%
    group_modify(slide_one_grp, ..., .keep = FALSE) %>%
    filter(.data$.real) %>%
    select(-.real) %>%
    arrange_col_canonical()

  if (.all_rows) {
    result[!(result$time_value %in% ref_time_values), result_col_names] <- NA
  } else if (user_provided_rtvs) {
    result <- result[result$time_value %in% ref_time_values, ]
  }

  if (!is_epi_df(result)) {
    # `.all_rows` handling strips epi_df format and metadata.
    # Restore them.
    result <- reclass(result, attributes(.x)$metadata)
  }

  return(result)
}

#' Optimized slide function for performing rolling averages on an `epi_df` object
#'
#' Slides an n-timestep mean over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' Wrapper around `epi_slide_opt` with `.f = datatable::frollmean`.
#'
#' @template basic-slide-params
#' @template opt-slide-params
#' @param ... Additional arguments to pass to the slide computation `.f`, for
#'  example, `algo` or `na.rm` in data.table functions. You don't need to
#'  specify `.x`, `.window_size`, or `.align` (or `before`/`after` for slider
#'  functions).
#'
#' @template opt-slide-details
#'
#' @export
#' @seealso [`epi_slide`] [`epi_slide_opt`] [`epi_slide_sum`]
#' @examples
#' # slide a 7-day trailing average formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, .window_size = 7) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day trailing average formula on cases. Adjust `frollmean` settings for speed
#' # and accuracy, and to allow partially-missing windows.
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(
#'     cases,
#'     .window_size = 7,
#'     # `frollmean` options
#'     na.rm = TRUE, algo = "exact", hasNA = TRUE
#'   ) %>%
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day leading average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, .window_size = 7, .align = "right") %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 7-day center-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, .window_size = 7, .align = "center") %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dav = slide_value_cases) %>%
#'   ungroup()
#'
#' # slide a 14-day center-aligned average
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_mean(cases, .window_size = 14, .align = "center") %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_14dav = slide_value_cases) %>%
#'   ungroup()
epi_slide_mean <- function(
    .x, .col_names, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
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
      "epi_slide_mean: the argument `new_col_name` is not supported. If you want to customize
      the output column names, use `dplyr::rename` after the slide."
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
    .ref_time_values = .ref_time_values,
    .all_rows = .all_rows
  )
}

#' Optimized slide function for performing rolling sums on an `epi_df` object
#'
#' Slides an n-timestep sum over variables in an `epi_df` object. See the [slide
#' vignette](https://cmu-delphi.github.io/epiprocess/articles/slide.html) for
#' examples.
#'
#' Wrapper around `epi_slide_opt` with `.f = datatable::frollsum`.
#'
#' @template basic-slide-params
#' @template opt-slide-params
#' @param ... Additional arguments to pass to the slide computation `.f`, for
#'  example, `algo` or `na.rm` in data.table functions. You don't need to
#'  specify `.x`, `.window_size`, or `.align` (or `before`/`after` for slider
#'  functions).
#'
#' @template opt-slide-details
#'
#' @export
#' @seealso [`epi_slide`] [`epi_slide_opt`] [`epi_slide_mean`]
#' @examples
#' # slide a 7-day trailing sum formula on cases
#' jhu_csse_daily_subset %>%
#'   group_by(geo_value) %>%
#'   epi_slide_sum(cases, .window_size = 7) %>%
#'   # Remove a nonessential var. to ensure new col is printed
#'   dplyr::select(geo_value, time_value, cases, cases_7dsum = slide_value_cases) %>%
#'   ungroup()
epi_slide_sum <- function(
    .x, .col_names, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
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
      "epi_slide_sum: the argument `new_col_name` is not supported. If you want to customize
      the output column names, use `dplyr::rename` after the slide."
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
#' @noRd
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

  return(list(
    all_dates = all_dates,
    pad_early_dates = pad_early_dates,
    pad_late_dates = pad_late_dates
  ))
}
