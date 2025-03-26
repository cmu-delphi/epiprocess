#' Information about upstream (`{data.table}`/`{slider}`) slide functions
#'
#' Underlies [`upstream_slide_f_info`].
#'
#' @keywords internal
upstream_slide_f_possibilities <- tibble::tribble(
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

#' Validate & get information about an upstream slide function
#'
#' @param .f function such as `data.table::frollmean` or `slider::slide_mean`;
#'   must appear in [`upstream_slide_f_possibilities`]
#' @return named list with two elements: `from_package`, a string containing the
#'   upstream package name ("data.table" or "slider"), and `namer`, a function
#'   that takes a column to call `.f` on and outputs a basic name or
#'   abbreviation for what operation `.f` represents on that kind of column
#'   (e.g., "sum", "av", "count").
#'
#' @keywords internal
upstream_slide_f_info <- function(.f) {
  assert_function(.f)

  # Check that slide function `.f` is one of those short-listed from
  # `data.table` and `slider` (or a function that has the exact same definition,
  # e.g. if the function has been reexported or defined locally). Extract some
  # metadata. `namer` will be mapped over columns (.x will be a column, not the
  # entire edf).
  f_info_row <- upstream_slide_f_possibilities %>%
    filter(map_lgl(.data$f, ~ identical(.f, .x)))
  if (nrow(f_info_row) == 0L) {
    # `f` is from somewhere else and not supported
    cli_abort(
      c(
        "problem with {rlang::expr_label(rlang::caller_arg(.f))}",
        "i" = "`.f` must be one of `data.table`'s rolling functions (`frollmean`,
              `frollsum`, `frollapply`. See `?data.table::roll`) or one of
              `slider`'s specialized sliding functions (`slide_mean`, `slide_sum`,
              etc. See `?slider::\`summary-slide\`` for more options)."
      ),
      class = "epiprocess__epi_slide_opt__unsupported_slide_function",
      epiprocess__f = .f
    )
  }
  if (nrow(f_info_row) > 1L) {
    cli_abort('epiprocess internal error: looking up `.f` in table of possible
               functions yielded multiple matches. Please report it using "New
               issue" at https://github.com/cmu-delphi/epiprocess/issues, using
               reprex::reprex to provide a minimal reproducible example.')
  }
  f_from_package <- f_info_row$package
  list(
    from_package = f_from_package,
    namer = unwrap(f_info_row$namer)
  )
}

#' Calculate input and output column names for an `{epiprocess}` [`dplyr::across`]-like operations
#'
#' @param .x data.frame to perform input column tidyselection on
#' @param time_type as in [`new_epi_df`]
#' @param col_names_quo enquosed input column tidyselect expression
#' @param .f_namer function taking an input column object and outputting a name
#'   for a corresponding output column; see [`upstream_slide_f_info`]
#' @param .window_size as in [`epi_slide_opt`]
#' @param .align as in [`epi_slide_opt`]
#' @param .prefix as in [`epi_slide_opt`]
#' @param .suffix as in [`epi_slide_opt`]
#' @param .new_col_names as in [`epi_slide_opt`]
#' @return named list with two elements: `input_col_names`, chr, subset of
#'   `names(.x)`; and `output_colnames`, chr, same length as `input_col_names`
#'
#' @keywords internal
across_ish_names_info <- function(.x, time_type, col_names_quo, .f_namer,
                                  .window_size, .align, .prefix, .suffix, .new_col_names) {
  # The position of a given column can differ between input `.x` and
  # `.data_group` since the grouping step by default drops grouping columns.
  # To avoid rerunning `eval_select` for every `.data_group`, convert
  # positions of user-provided `col_names` into string column names. We avoid
  # using `names(pos)` directly for robustness and in case we later want to
  # allow users to rename fields via tidyselection.
  pos <- eval_select(col_names_quo, data = .x, allow_rename = FALSE)
  input_col_names <- names(.x)[pos]

  # Handle output naming
  if ((!is.null(.prefix) || !is.null(.suffix)) && !is.null(.new_col_names)) {
    cli_abort(
      "Can't use both .prefix/.suffix and .new_col_names at the same time.",
      class = "epiprocess__epi_slide_opt_incompatible_naming_args"
    )
  }
  assert_string(.prefix, null.ok = TRUE)
  assert_string(.suffix, null.ok = TRUE)
  assert_character(.new_col_names, len = length(input_col_names), null.ok = TRUE)
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
      .f_abbr = purrr::map_chr(.x[, c(input_col_names)], .f_namer), # compat between DT and tbl selection
      quo_get_env(col_names_quo)
    )
    .new_col_names <- unclass(
      glue(.prefix, .envir = glue_env) +
        input_col_names +
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
  output_col_names <- .new_col_names

  list(
    input_col_names = input_col_names,
    output_col_names = output_col_names
  )
}

#' Optimized slide functions for common cases
#'
#' @description
#'
#' `epi_slide_opt` calculates n-time-step rolling means&sums,
#' cumulative/"running" means&sums, or other operations supported by
#' [`data.table::froll`] or [`slider::summary-slide`] functions.
#'
#' * On `epi_df`s, it will take care of looping over `geo_value`s, temporarily
#'   filling in time gaps with `NA`s and other work needed to ensure there are
#'   exactly `n` consecutive time steps per computation, and has some other
#'   convenience features. See `vignette("epi_df")` for more examples.
#'
#' * On `epi_archive`s, it will calculate the version history for these slide
#'   computations and combine it with the version history for the rest of the
#'   columns.
#'
#' This function tends to be much faster than using `epi_slide()` and
#' `epix_slide()` directly.
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
#'
#' # You can calculate entire version histories for the derived signals by
#' # calling `epi_slide_opt()` on an `epi_archive`:
#' case_death_rate_archive %>%
#'   epi_slide_mean(case_rate, .window_size = 14)
#'
#' @export
epi_slide_opt <- function(
    .x, .col_names, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"),
    .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
    .ref_time_values = NULL, .all_rows = FALSE) {
  UseMethod("epi_slide_opt")
}

#' @export
epi_slide_opt.epi_df <- function(.x, .col_names, .f, ...,
                                 .window_size = NULL, .align = c("right", "center", "left"),
                                 .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
                                 .ref_time_values = NULL, .all_rows = FALSE) {
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

  # Validate/process .col_names, .f:
  col_names_quo <- enquo(.col_names)
  f_info <- upstream_slide_f_info(.f)
  f_from_package <- f_info$from_package

  # Validate/process .ref_time_values:
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

  # Handle output naming:
  names_info <- across_ish_names_info(
    .x, time_type, col_names_quo, f_info$namer,
    .window_size, .align, .prefix, .suffix, .new_col_names
  )
  input_col_names <- names_info$input_col_names
  output_col_names <- names_info$output_col_names

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
        roll_output <- .f(x = .data_group[, input_col_names], n = window_size, ...)
      } else {
        window_size <- list(seq_along(.data_group$time_value))
        roll_output <- .f(x = .data_group[, input_col_names], n = window_size, adaptive = TRUE, ...)
      }
      if (window_args$after >= 1) {
        .data_group[, output_col_names] <- purrr::map(roll_output, function(.x) {
          c(.x[(window_args$after + 1L):length(.x)], rep(NA, window_args$after))
        })
      } else {
        .data_group[, output_col_names] <- roll_output
      }
    }
    if (f_from_package == "slider") {
      for (i in seq_along(input_col_names)) {
        .data_group[, output_col_names[i]] <- .f(
          x = .data_group[[input_col_names[i]]],
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
    result[!vec_in(result$time_value, ref_time_values), output_col_names] <- NA
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
