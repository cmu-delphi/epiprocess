# TODO just make this an epi_slide_opt impl?
epix_epi_slide_opt_one_epikey <- function(updates, in_colnames, f, f_from_package, before, after, time_type, out_colnames) {
  unit_step <- epiprocess:::unit_time_delta(time_type)
  prev_inp_snapshot <- NULL
  prev_out_snapshot <- NULL
  result <- map(seq_len(nrow(updates)), function(update_i) {
    version <- updates$version[[update_i]]
    inp_update <- updates$subtbl[[update_i]] # TODO decide whether DT
    ## setDF(inp_update)
    ## inp_update <- new_tibble(inp_update, nrow = nrow(inp_update))
    inp_snapshot <- tbl_patch(prev_inp_snapshot, inp_update, "time_value")
    if (before == Inf) {
      if (after != 0) {
        cli_abort('.window_size = Inf is only supported with .align = "right"')
      }
      # We need to use the entire input snapshot range, filling in time gaps. We
      # shouldn't pad the ends.
      slide_min_t <- min(inp_snapshot$time_value) # TODO check efficiency
      slide_max_t <- max(inp_snapshot$time_value)
    } else {
      # If the input had updates in the range t1..t2, this could produce changes
      # in slide outputs in the range t1-after..t2+before, and to compute those
      # slide values, we need to look at the input snapshot from
      # t1-after-before..t2+before+after.
      inp_update_min_t <- min(inp_update$time_value) # TODO check efficiency
      inp_update_max_t <- max(inp_update$time_value)
      slide_min_t <- inp_update_min_t - (before + after) * unit_step
      slide_max_t <- inp_update_max_t + (before + after) * unit_step
    }
    slide_nrow <- time_delta_to_n_steps(slide_max_t - slide_min_t, time_type) + 1L
    slide_time_values <- slide_min_t + 0L:(slide_nrow - 1L) * unit_step
    slide_inp_backrefs <- vec_match(slide_time_values, inp_snapshot$time_value)
    slide <- inp_snapshot[slide_inp_backrefs, ] # TODO vs. DT key index vs ....
    slide$time_value <- slide_time_values
    # TODO ensure before & after as integers?
    # TODO parameterize naming, slide function, options, ...
    if (f_from_package == "data.table") {
      if (before == Inf) {
        n_arg <- seq_len(slide_nrow)
        adaptive_arg <- TRUE
      } else {
        n_arg <- before + after + 1L
        adaptive_arg <- FALSE
      }
      for (col_i in seq_along(in_colnames)) {
        slide[[out_colnames[[col_i]]]] <- f(slide[[in_colnames[[col_i]]]], n_arg, adaptive = adaptive_arg)
      }
    } else if (f_from_package == "slider") {
      for (col_i in seq_along(in_colnames)) {
        # with adaptive tails that incorporate fewer inputs:
        # FIXME other arg forwarding
        out_col <- f(slide[[in_colnames[[col_i]]]], before = before, after = after)
        # XXX is this actually required or being done at the right time? we are
        # already chopping off a good amount that might include this?
        #
        # FIXME can this generate an error on very short series?
        vec_slice(out_col, seq_len(before)) <- NA
        vec_slice(out_col, slide_nrow - after + seq_len(after)) <- NA
        slide[[out_colnames[[col_i]]]] <- out_col
      }
    } else {
      cli_abort("epiprocess internal error: `f_from_package` was {format_chr_deparse(f_from_package)}")
    }
    rows_should_keep <-
      if (before == Inf) {
        # Re-introduce time gaps:
        !is.na(slide_inp_backrefs)
      } else {
        # Get back to t1-after..t2+before; times outside this range were included
        # only so those inside would have enough context for their slide
        # computations, but these "context" rows may contain invalid slide
        # computation outputs:
        vec_rep_each(c(FALSE, TRUE, FALSE), c(before, slide_nrow - before - after, after)) &
          # Only include time_values that appeared in the input snapshot:
          !is.na(slide_inp_backrefs)
      }
    out_update <- slide[rows_should_keep, ]
    out_diff <- tbl_diff2(prev_out_snapshot, out_update, "time_value", "update") # TODO avoid redundant diff2 work? though depends on compactify parms...
    out_snapshot <- tbl_patch(prev_out_snapshot, out_diff)
    prev_inp_snapshot <<- inp_snapshot
    prev_out_snapshot <<- out_snapshot # TODO avoid need to patch twice?
    out_diff$version <- version
    out_diff
  })
  result
}

#' @export
epix_epi_slide_opt <-
  function(.x, .col_names, .f, ...,
           .window_size = NULL, .align = c("right", "center", "left"),
           .prefix = NULL, .suffix = NULL, .new_col_names = NULL # ,
           ## .ref_time_values = NULL, .all_rows = FALSE
  ) {
    UseMethod("epix_epi_slide_opt")
  }

#' @method epix_epi_slide_opt grouped_epi_archive
#' @export
epix_epi_slide_opt.grouped_epi_archive <- function(.x, ...) {
  assert_set_equal(
    group_vars(.x),
    key_colnames(.x, exclude = c("time_value", "version"))
  )
  orig_group_vars <- group_vars(.x)
  orig_drop <- .x$private$drop
  .x %>%
    ungroup() %>%
    epix_epi_slide_opt(...) %>%
    group_by(pick(all_of(orig_group_vars)), .drop = orig_drop)
}

#' @method epix_epi_slide_opt epi_archive
#' @export
epix_epi_slide_opt.epi_archive <-
  function(.x, .col_names, .f, ...,
           .window_size = NULL, .align = c("right", "center", "left"),
           .prefix = NULL, .suffix = NULL, .new_col_names = NULL,
           ## , .ref_time_values = NULL, .all_rows = FALSE
           .progress = FALSE) {
    # Extract metadata:
    time_type <- .x$time_type
    epikey_names <- key_colnames(.x, exclude = c("time_value", "version"))
    # Validation & pre-processing:
    .align <- arg_match(.align)
    f_info <- upstream_slide_f_info(.f)
    col_names_quo <- enquo(.col_names)
    names_info <- across_ish_names_info(.x$DT, time_type, col_names_quo, f_info$namer, .window_size, .align, .prefix, .suffix, .new_col_names)
    window_args <- get_before_after_from_window(.window_size, .align, time_type)
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
      # 0 rows input -> 0 rows output, so we can just say drop = TRUE:
      grouped_df(epikey_names, TRUE)
    if (use_progress) progress_bar_id <- cli::cli_progress_bar(.progress, total = n_groups(updates_grouped))
    result <- updates_grouped %>%
      group_modify(function(group_values, group_key) {
        group_updates <- group_values %>%
          nest(.by = version, .key = "subtbl") %>%
          arrange(version)
        res <- epix_epi_slide_opt_one_epikey(group_updates, names_info$input_col_names, .f, f_info$from_package, window_args$before, window_args$after, time_type, names_info$output_col_names) %>%
          list_rbind()
        if (use_progress) cli::cli_progress_update(id = progress_bar_id)
        res
      }) %>%
      ungroup() %>%
      new_epi_archive(
        .x$geo_type, .x$time_type, .x$other_keys,
        .x$clobberable_versions_start, .x$versions_end
      )
    if (use_progress) cli::cli_progress_done(id = progress_bar_id)
    # Keep ordering of old columns, place new columns at end:
    setcolorder(result$DT, names(.x$DT))
    result
  }
