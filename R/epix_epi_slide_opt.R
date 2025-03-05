#' Core operation of `epix_epi_slide_opt` for a single epikey's history
#'
#' @param updates tibble with two columns: `version` and `subtbl`; `subtbl` is a
#'   list of tibbles, each with a `time_value` column and measurement columns.
#'   The epikey should not appear.
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
#' @return list of tibbles with same names as `subtbl`s plus: `c(out_colnames,
#'   "version")`; (compactified) diff data to put into an `epi_archive`
#'
#' @examples
#'
#' library(dplyr)
#' updates <- bind_rows(
#'   tibble(
#'     version = 40, time_value = 1:10, value = 1:10
#'   ),
#'   tibble(
#'     version = 12, time_value = 2:3, value = 3:2
#'   ),
#'   tibble(
#'     version = 13, time_value = 6, value = 7,
#'   ),
#'   tibble(
#'     version = 13, time_value = 7, value = NA,
#'   )
#' ) %>%
#'   mutate(across(c(version, time_value), ~ as.Date("2020-01-01") - 1 + .x)) %>%
#'   tidyr::nest(.by = version, .key = "subtbl")
#'
#' updates %>%
#'   epix_epi_slide_opt_one_epikey("value", data.table::frollmean, "data.table", 1L, 0L, "day", "slide_value")
#'
#' @keywords internal
epix_epi_slide_opt_one_epikey <- function(updates, in_colnames, f_dots_baked, f_from_package, before, after, time_type, out_colnames) {
  # TODO check for col name clobbering
  unit_step <- epiprocess:::unit_time_delta(time_type)
  prev_inp_snapshot <- NULL
  prev_out_snapshot <- NULL
  result <- map(seq_len(nrow(updates)), function(update_i) {
    version <- updates$version[[update_i]]
    inp_update <- updates$subtbl[[update_i]]
    inp_snapshot <- tbl_patch(prev_inp_snapshot, inp_update, "time_value")
    if (before == Inf) {
      if (after != 0) {
        cli_abort('.window_size = Inf is only supported with .align = "right"')
      }
      # We need to use the entire input snapshot range, filling in time gaps. We
      # shouldn't pad the ends.
      slide_t_min <- min(inp_snapshot$time_value)
      slide_t_max <- max(inp_snapshot$time_value)
    } else {
      # If the input had updates in the range t1..t2, this could produce changes
      # in slide outputs in the range t1-after..t2+before, and to compute those
      # slide values, we need to look at the input snapshot from
      # t1-after-before..t2+before+after.
      inp_update_t_min <- min(inp_update$time_value)
      inp_update_t_max <- max(inp_update$time_value)
      slide_t_min <- inp_update_t_min - (before + after) * unit_step
      slide_t_max <- inp_update_t_max + (before + after) * unit_step
    }
    slide_nrow <- time_delta_to_n_steps(slide_t_max - slide_t_min, time_type) + 1L
    slide_time_values <- slide_t_min + 0L:(slide_nrow - 1L) * unit_step
    slide_inp_backrefs <- vec_match(slide_time_values, inp_snapshot$time_value)
    # Get additional values needed from inp_snapshot + perform any NA
    # tail-padding needed to make slider results a fixed window size rather than
    # adaptive at tails + perform any NA gap-filling needed:
    slide <- inp_snapshot[slide_inp_backrefs, ]
    slide$time_value <- slide_time_values
    if (f_from_package == "data.table") {
      for (col_i in seq_along(in_colnames)) {
        if (before == Inf) {
          slide[[out_colnames[[col_i]]]] <- f_dots_baked(slide[[in_colnames[[col_i]]]], seq_len(slide_nrow), adaptive = TRUE)
        } else {
          out_col <- f_dots_baked(slide[[in_colnames[[col_i]]]], before + after + 1L)
          if (after != 0L) {
            # data.table always puts NAs at tails, even with na.rm = TRUE; chop
            # off extra NAs from beginning and place missing NAs at end:
            out_col <- c(out_col[seq(after + 1L, slide_nrow)], rep(NA, after))
          }
          slide[[out_colnames[[col_i]]]] <- out_col
        }
      }
    } else if (f_from_package == "slider") {
      for (col_i in seq_along(in_colnames)) {
        slide[[out_colnames[[col_i]]]] <- f_dots_baked(slide[[in_colnames[[col_i]]]], before = before, after = after)
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
    out_diff <- tbl_diff2(prev_out_snapshot, out_update, "time_value", "update")
    out_snapshot <- tbl_patch(prev_out_snapshot, out_diff, "time_value")
    prev_inp_snapshot <<- inp_snapshot
    prev_out_snapshot <<- out_snapshot
    out_diff$version <- version
    out_diff
  })
  result
}

# TODO just make this an epi_slide_opt impl?

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
    .f_info <- upstream_slide_f_info(.f)
    .f_dots_baked <-
      if (rlang::dots_n(...) == 0L) {
        # Leaving `.f` unchanged slightly improves computation speed and trims
        # debug stack traces:
        .f
      } else {
        purrr::partial(.f, ...)
      }
    col_names_quo <- enquo(.col_names)
    names_info <- across_ish_names_info(.x$DT, time_type, col_names_quo, .f_info$namer, .window_size, .align, .prefix, .suffix, .new_col_names)
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
        # TODO move nesting inside the helper?
        res <- epix_epi_slide_opt_one_epikey(group_updates, names_info$input_col_names, .f_dots_baked, .f_info$from_package, window_args$before, window_args$after, time_type, names_info$output_col_names) %>%
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
