#' Core operation of `epi_slide_opt.epi_archive` for a single epikey's history
#'
#' @param inp_updates tibble with a `version` column and measurement columns for
#'   a single epikey, without the epikey labeling columns (e.g., from
#'   `group_modify`). Interpretation is analogous to an `epi_archive` `DT`, but
#'   a specific row order is not required.
#' @inheritParams epi_slide_opt_edf_one_epikey
#' @param inf_slide_time_min_changes `NULL` (required if `before_n_steps` is
#'   finite) or a tibble with columns `version` and `time_min`, sorted by
#'   `version`, recording each version at which the minimum `time_value` across
#'   all epikeys in the archive snapshot decreased (including the first
#'   version), and the new minimum. When `before_n_steps` is `Inf`, this is used
#'   to start running windows at the archive-wide minimum `time_value` as of
#'   each version, matching `epi_slide` on `epix_as_of` snapshots. `NULL` with
#'   `Inf` `before_n_steps` will instead start at this epikey's own minimum.
#' @return tibble with a `version` column, pre-existing measurement columns, and
#'   new measurement columns; (compactified) diff data to put into an
#'   `epi_archive`. May not match column ordering; may not ensure any row
#'   ordering.
#'
#' @examples
#'
#' library(dplyr)
#' inp_updates <- bind_rows(
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
#' inp_updates %>%
#'   epiprocess:::epi_slide_opt_archive_one_epikey(
#'     "value", f, "data.table", 2L, 0L, "day", "slide_value",
#'     NULL
#'   )
#'
#' @importFrom dplyr left_join join_by
#' @keywords internal
epi_slide_opt_archive_one_epikey <- function(
  inp_updates,
  in_colnames,
  f_dots_baked, f_from_package,
  before_n_steps, after_n_steps, time_type,
  out_colnames,
  inf_slide_time_min_changes
) {
  inp_updates_by_version <- inp_updates %>%
    nest(.by = version, .key = "subtbl") %>%
    arrange(version)
  if (!is.null(inf_slide_time_min_changes)) {
    # The slide computation's output depends both on the epikey snapshot and the
    # overall time min, so we need to know when either is updated. This is
    # essentially epix_merge() the epikey's row updates with the
    # inf_slide_time_min_changes, but it doesn't handle this configuration yet.
    # We also want to ensure empty updates are represented with 0-row tibbles,
    # not NULL.
    #
    # But first: we don't care about inf_slide_time_min_changes before our
    # epikey's first update; we'd output 0 rows anyway. In fact, we need to
    # ignore it to avoid some inconveniences when performing computations
    # (output type ambiguity, ...). Do a "versions_filter" on
    # inf_slide_time_min_changes:
    x <- inp_updates_by_version
    first_version <- min(x$version)
    y <- inf_slide_time_min_changes
    y <- vec_rbind(
      tibble(
        version = first_version,
        time_min = y$time_min[[max(which(y$version <= first_version))]]
      ),
      y[y$version > first_version, ]
    )
    # Now, the merge:
    extra_versions <- vec_set_difference(y$version, x$version)
    empty_update <- vec_slice(inp_updates_by_version$subtbl[[1L]], 0L)
    xy_rows_x_cols <- vec_rbind(
      x,
      tibble(
        version = extra_versions,
        subtbl = rep(list(empty_update), length(extra_versions))
      )
    ) %>%
      arrange(version)
    # Now, for both original and extra versions, line up with the archive-wide
    # time_min as of that version:
    inp_updates_by_version <-
      left_join(
        xy_rows_x_cols,
        y %>% select(
          inf_slide_time_min_version = version,
          inf_slide_time_min = time_min
        ),
        by = join_by(closest(version >= inf_slide_time_min_version))
      ) %>%
      select(-inf_slide_time_min_version)
  }
  unit_step <- unit_time_delta(time_type, format = "fast")
  prev_inp_snapshot <- NULL
  prev_out_snapshot <- NULL
  prev_inf_slide_time_min <- NULL
  result <- lapply(seq_len(nrow(inp_updates_by_version)), function(version_i) {
    version <- inp_updates_by_version$version[[version_i]]
    inp_update <- inp_updates_by_version$subtbl[[version_i]]
    if (is.null(inf_slide_time_min_changes)) {
      inf_slide_time_min <- NULL
      inf_slide_time_min_changed <- FALSE
    } else {
      inf_slide_time_min <- inp_updates_by_version$inf_slide_time_min[[version_i]]
      # (On this epikey's first version, `prev_inf_slide_time_min` is `NULL`, so
      # this counts as a change and we compute all outputs.)
      inf_slide_time_min_changed <- !identical(inf_slide_time_min, prev_inf_slide_time_min)
    }
    inp_snapshot <- tbl_patch(prev_inp_snapshot, inp_update, "time_value")
    if (inf_slide_time_min_changed) {
      # All running-window outputs may change; recompute all of them:
      out_update_min_t <- min(inp_snapshot$time_value)
      out_update_max_t <- max(inp_snapshot$time_value)
    } else {
      inp_update_min_t <- min(inp_update$time_value)
      inp_update_max_t <- max(inp_update$time_value)
      # Time inp_update_max_t + before should have an output update, since it
      # depends on inp_update_max_t + before - before = inp_update_max_t, which
      # has an input update. Similarly, we could have updates beginning with
      # inp_update_min_t - after, or anything in between these two bounds. If
      # before == Inf, we need to update outputs all the way to the end of the
      # input *snapshot*. nolint: commented_code_linter
      out_update_min_t <- inp_update_min_t - after_n_steps * unit_step
      if (before_n_steps == Inf) {
        out_update_max_t <- max(inp_snapshot$time_value)
      } else {
        out_update_max_t <- inp_update_max_t + before_n_steps * unit_step
      }
    }
    out_update <- epi_slide_opt_edf_one_epikey(
      inp_snapshot, in_colnames,
      f_dots_baked, f_from_package, before_n_steps, after_n_steps,
      unit_step, time_type,
      out_colnames,
      list(out_update_min_t, out_update_max_t), NULL,
      inf_slide_time_min
    )
    out_diff <- tbl_diff2(prev_out_snapshot, out_update, "time_value", "update")
    prev_inp_snapshot <<- inp_snapshot
    prev_out_snapshot <<- tbl_patch(prev_out_snapshot, out_diff, "time_value")
    prev_inf_slide_time_min <<- inf_slide_time_min
    out_diff$version <- version
    out_diff
  })
  result <- list_rbind(result)
  result
}

# (`closest` is only defined inside `join_by`.)
utils::globalVariables(c("inf_slide_time_min_version", "closest", "time_min"))

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
        purrr::partial(.f, ... = , ...) # `... =` stands in for future args
      }
    col_names_quo <- enquo(.col_names)
    if (is.null(.window_size)) {
      cli_abort(
        "epi_slide_opt: `.window_size` must be specified.",
        class = "epiprocess__epi_slide_opt__window_size_missing"
      )
    }
    validate_slide_window_arg(.window_size, time_type)
    names_info <- across_ish_names_info(
      .x$DT, time_type, col_names_quo, .f_info$namer,
      .window_size, .align, .prefix, .suffix, .new_col_names
    )
    window_args <- get_before_after_from_window(.window_size, .align, time_type)
    before_n_steps <- time_delta_to_n_steps(window_args$before, time_type)
    after_n_steps <- time_delta_to_n_steps(window_args$after, time_type)
    if (!is.null(.ref_time_values)) {
      cli_abort("epi_slide_opt.epi_archive does not support the `.ref_time_values` argument",
        class = "epiprocess__epi_slide_opt_archive__ref_time_values_unsupported"
      )
    }
    if (!identical(.all_rows, FALSE)) {
      cli_abort("epi_slide_opt.epi_archive does not support the `.all_rows` argument",
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
    if (nrow(.x$DT) == 0L) {
      # Zero input rows, zero output rows: try to add 0-row output columns,
      # using `.f` to determine their types:
      if (identical(.f, frollapply)) {
        # `frollapply`'s output type depends on its `FUN`, which we're not
        # calling. There's no good marker: logical(0) is ptype logical(), not
        # vctrs::unspecified(); and unspecified() is meant only for ptype logic:
        # it's finalized to logical() when combined (e.g., by `vec_rbind`), but
        # stored directly as a column, it would remain a strange
        # `vctrs_unspecified` column (which, e.g., `data.table::rbindlist`
        # rejects). We could fake input and attempt a guess, like
        # `dplyr::summarise()` does on 0-row data. But for now, just balk (like
        # we do with epi_dfs for all slide computation functions).
        cli_abort(c(
          "`epi_slide_opt` can't determine output column types when using
           `.f = frollapply` on an archive with 0 rows.",
          ">" = "Check whether you accidentally filtered away all the data
                 in a prior operation (e.g., filtering down to a geo that
                 lacks availability in the time period of interest).",
          ">" = "Otherwise, you'll need to check for this 0-row case and
                 manually provide the appropriate output."
        ), class = "epiprocess__epi_slide_opt__frollapply_0_rows")
      }
      result <- clone(.x)
      for (col_i in seq_along(names_info$input_col_names)) {
        inp_col <- result$DT[[names_info$input_col_names[[col_i]]]]
        out_col <-
          if (.f_info$from_package == "data.table") {
            .f_dots_baked(list(inp_col), 1L)[[1L]]
          } else {
            .f_dots_baked(inp_col, before = 0L, after = 0L)
          }
        set(result$DT, j = names_info$output_col_names[[col_i]], value = out_col)
      }
      return(result)
    }
    # Arithmetic on some time-related classes is slow; perform the computation
    # under integer time_type. The only other inputs to the computation,
    # `before_n_steps` and `after_n_steps`, are in terms of time steps, so are
    # unaffected by the conversion. (Output column names were already
    # determined above based on the original time_type.)
    if (time_type == "integer") {
      conversion <- NULL
      x <- .x
    } else {
      converted <- to_vstretched_numeric_archive_info(.x)
      x <- converted$archive
      conversion <- converted$conversion
    }
    comp_time_type <- x$time_type
    # Like `epi_slide` on `epix_as_of` snapshots, start `Inf` windows at the min
    # time_value across all epikeys in each snapshot. Snapshots never lose rows
    # (deletions are represented as NA rows), so this min can only decrease;
    # record the versions where it does:
    inf_slide_time_min_changes <-
      if (before_n_steps == Inf) {
        updates_time_mins <- x$DT[, list(time_min = min(time_value)), keyby = version] %>%
          setDF() %>%
          as_tibble()
        snapshot_time_mins <- tibble(
          version = updates_time_mins$version,
          time_min = slide_vec(updates_time_mins$time_min, min, .before = Inf)
          # (^ use generic slide since cummin, slide_min, frollmin don't/mis- handle Dates)
        )
        inf_slide_time_min_changes <- apply_compactify(snapshot_time_mins, ukey_names = "version")
      } else {
        NULL
      }
    # Perform the slide:
    updates_grouped <- x$DT %>%
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
          .f_dots_baked, .f_info$from_package, before_n_steps, after_n_steps, comp_time_type,
          names_info$output_col_names,
          inf_slide_time_min_changes
        )
        if (use_progress) cli::cli_progress_update(id = progress_bar_id)
        res
      }) %>%
      as.data.frame() %>% # data.table#6859
      as.data.table(key = key(x$DT)) %>%
      new_epi_archive(
        x$geo_type, x$time_type, x$other_keys,
        x$clobberable_versions_start, x$versions_end
      )
    if (use_progress) cli::cli_progress_done(id = progress_bar_id)
    # Keep ordering of old columns, place new columns at end:
    setcolorder(result$DT, names(x$DT))
    if (!is.null(conversion)) {
      result <- from_vstretched_numeric_archive_info(result, conversion)
    }
    result
  }
