#' Core operation of `epi_slide_opt.epi_archive` for a single epikey's history
#'
#' @param grp_updates tibble with a `version` column and measurement columns for
#'   a single epikey, without the epikey labeling columns (e.g., from
#'   `group_modify`). Interpretation is analogous to an `epi_archive` `DT`, but
#'   a specific row order is not required.
#' @param in_colnames chr; names of columns to which to apply `f_dots_baked`
#' @param f_dots_baked supported sliding function from `{data.table}` or
#'   `{slider}`, potentially with some arguments baked in with
#'   [`purrr::partial`]
#' @param f_from_package string; name of package from which `f_dots_baked`
#'   (pre-`partial`) originates
#' @param before integerish >=0 or Inf; number of time steps before each
#'   ref_time_value to include in the sliding window computation; Inf to include
#'   all times beginning with the min `time_value`
#' @param after integerish >=0; number of time steps after each ref_time_value
#'   to include in the sliding window computation
#' @param time_type as in `new_epi_archive`
#' @param out_colnames chr, same length as `in_colnames`; column names to use
#'   for results
#' @return tibble with a `version` column, pre-existing measurement columns, and
#'   new measurement columns; (compactified) diff data to put into an
#'   `epi_archive`. May not match column ordering; may not ensure any row
#'   ordering.
#'
#' @examples
#'
#' library(dplyr)
#' grp_updates <- bind_rows(
#'   tibble(version = 30, time_value = 1:20, value = 1:20),
#'   tibble(version = 32, time_value = 4:5, value = 5:4),
#'   tibble(version = 33, time_value = 8, value = 9),
#'   tibble(version = 34, time_value = 11, value = NA),
#'   tibble(version = 35, time_value = -10, value = -10),
#'   tibble(version = 56, time_value = 50, value = 50)
#' ) %>%
#'   mutate(across(c(version, time_value), ~ as.Date("2020-01-01") - 1 + .x))
#'
#' f <- purrr::partial(data.table::frollmean, algo = "exact")
#'
#' grp_updates %>%
#'   epiprocess:::epi_slide_opt_archive_one_epikey(
#'     "value", f, "data.table", 2L, 0L, "day", "slide_value"
#'   )
#'
#' @keywords internal
epi_slide_opt_archive_one_epikey <- function(
    grp_updates,
    in_colnames,
    f_dots_baked, f_from_package, before, after, time_type,
    out_colnames) {
  grp_updates_by_version <- grp_updates %>%
    nest(.by = version, .key = "subtbl") %>%
    arrange(version)
  unit_step <- unit_time_delta(time_type, format = "fast")
  prev_inp_snapshot <- NULL
  prev_out_snapshot <- NULL
  result <- map(seq_len(nrow(grp_updates_by_version)), function(version_i) {
    version <- grp_updates_by_version$version[[version_i]]
    inp_update <- grp_updates_by_version$subtbl[[version_i]]
    inp_snapshot <- tbl_patch(prev_inp_snapshot, inp_update, "time_value")
    inp_update_min_t <- min(inp_update$time_value)
    inp_update_max_t <- max(inp_update$time_value)
    # Time inp_update_max_t + before should have an output update, since it
    # depends on inp_update_max_t + before - before = inp_update_max_t, which
    # has an input update. Similarly, we could have updates beginning with
    # inp_update_min_t - after, or anything in between these two bounds.
    out_update_min_t <- time_minus_n_steps(inp_update_min_t, after, time_type)
    out_update_max_t <- time_plus_n_steps(inp_update_max_t, before, time_type)
    out_update_ts <- vec_slice(inp_snapshot$time_value, between(inp_snapshot$time_value, out_update_min_t, out_update_max_t))
    out_update <- epi_slide_opt_one_epikey(inp_snapshot, f_dots_baked, f_from_package, before, after, unit_step, time_type, out_update_ts, in_colnames, out_colnames)
    out_diff <- tbl_diff2(prev_out_snapshot, out_update, "time_value", "update")
    prev_inp_snapshot <<- inp_snapshot
    prev_out_snapshot <<- tbl_patch(prev_out_snapshot, out_diff, "time_value")
    out_diff$version <- version
    out_diff
  })
  result <- list_rbind(result)
  result
}

#' @method epi_slide_opt grouped_epi_archive
#' @export
epi_slide_opt.grouped_epi_archive <- function(.x, ...) {
  assert_set_equal(
    group_vars(.x),
    key_colnames(.x$private$ungrouped, exclude = c("time_value", "version"))
  )
  orig_group_vars <- group_vars(.x)
  orig_drop <- .x$private$drop
  .x %>%
    ungroup() %>%
    epi_slide_opt(...) %>%
    group_by(pick(all_of(orig_group_vars)), .drop = orig_drop)
}

#' @method epi_slide_opt epi_archive
#' @export
epi_slide_opt.epi_archive <-
  function(.x, .col_names, .f, ...,
           .window_size = NULL, .align = c("right", "center", "left"),
           .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
           .ref_time_values = NULL, .all_rows = FALSE,
           .progress = FALSE) {
    # Extract metadata:
    time_type <- .x$time_type
    epikey_names <- key_colnames(.x, exclude = c("time_value", "version"))
    # Validation & pre-processing:
    .align <- arg_match(.align)
    .f_info <- upstream_slide_f_info(.f, ...)
    .f_dots_baked <-
      if (rlang::dots_n(...) == 0L) {
        # Leaving `.f` unchanged slightly improves computation speed and trims
        # debug stack traces:
        .f
      } else {
        purrr::partial(.f, ...)
      }
    col_names_quo <- enquo(.col_names)
    names_info <- across_ish_names_info(
      .x$DT, time_type, col_names_quo, .f_info$namer,
      .window_size, .align, .prefix, .suffix, .new_col_names
    )
    window_args <- get_before_after_from_window(.window_size, .align, time_type)
    if (!is.null(.ref_time_values)) {
      cli_abort("epi_slide.epi_archive does not support the `.ref_time_values` argument",
        class = "epiprocess__epi_slide_opt_archive__ref_time_values_unsupported"
      )
    }
    if (!identical(.all_rows, FALSE)) {
      cli_abort("epi_slide.epi_archive does not support the `.all_rows` argument",
        class = "epiprocess__epi_slide_opt_archive__all_rows_unsupported"
      )
    }
    assert(
      checkmate::check_logical(.progress, any.missing = FALSE, len = 1L, names = "unnamed"),
      checkmate::check_string(.progress)
    )
    if (isTRUE(.progress)) {
      .progress <- "Time series processed:"
    }
    use_progress <- !isFALSE(.progress)
    # Perform the slide:
    updates_grouped <- .x$DT %>%
      as.data.frame() %>%
      as_tibble(.name_repair = "minimal") %>%
      # 0 rows input -> 0 rows output for any drop = FALSE groups with 0 rows, so
      # we can just say drop = TRUE:
      grouped_df(epikey_names, drop = TRUE)
    if (use_progress) progress_bar_id <- cli::cli_progress_bar(.progress, total = n_groups(updates_grouped))
    result <- updates_grouped %>%
      group_modify(function(group_values, group_key) {
        res <- epi_slide_opt_archive_one_epikey(
          group_values,
          names_info$input_col_names,
          .f_dots_baked, .f_info$from_package, window_args$before, window_args$after, time_type,
          names_info$output_col_names
        )
        if (use_progress) cli::cli_progress_update(id = progress_bar_id)
        res
      }) %>%
      as.data.frame() %>% # data.table#6859
      as.data.table(key = key(.x$DT)) %>%
      new_epi_archive(
        .x$geo_type, .x$time_type, .x$other_keys,
        .x$clobberable_versions_start, .x$versions_end
      )
    if (use_progress) cli::cli_progress_done(id = progress_bar_id)
    # Keep ordering of old columns, place new columns at end:
    setcolorder(result$DT, names(.x$DT))
    result
  }
