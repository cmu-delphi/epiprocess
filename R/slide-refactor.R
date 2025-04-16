#' Convert time slide function to a simple hop function
#'
#' @examples
#' time_slide_to_simple_hop(as_time_slide_computation(~ .x[1L, ]),
#'   .before_n_steps = 2L, .after_n_steps = 0L
#' )(
#'   tibble(time_value = 1:5, value = 1:5),
#'   tibble(geo_value = 1),
#'   3:4
#' )
#'
#' @keywords internal
time_slide_to_simple_hop <- function(.slide_comp, ..., .before_n_steps, .after_n_steps) {
  function(grp_data, grp_key, ref_inds) {
    available_ref_time_values <- vec_slice(grp_data$time_value, ref_inds)
    i <<- 0L
    wrapped_slide_comp <- function(.x, .group_key, ...) {
      # XXX could just use enclosing dots rather than forwarding through hop()
      i <<- i + 1L
      # XXX could also use .after_n_steps to figure out...
      .slide_comp(.x, .group_key, available_ref_time_values[[i]], ...)
    }
    if (.before_n_steps == Inf) {
      starts <- 1L
    } else {
      starts <- ref_inds - .before_n_steps
    }
    stops <- ref_inds + .after_n_steps
    # Compute the slide values. slider::hop_index will return a list of f outputs
    # e.g. list(f(.slide_group_1, .group_key, .ref_time_value_1),
    # f(.slide_group_1, .group_key, .ref_time_value_2), ...)
    slide_values_list <- slider::hop(
      .x = grp_data,
      .starts = starts,
      .stops = stops,
      .f = wrapped_slide_comp,
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

    slide_values
  }
}

#' Convert upstream specialized slide function to a simple hop function
#'
#' upstream_slide_to_simple_hop(frollmean, .in_colnames = "value", .out_colnames = "slide_value", .before_n_steps = 1L, .after_n_steps = 0L)(
#'   tibble(time_value = 1:5, value = 1:5),
#'   tibble(geo_value = 1),
#'   3:4
#' )
#' upstream_slide_to_simple_hop(slide_mean, .in_colnames = "value", .out_colnames = "slide_value", .before_n_steps = 1L, .after_n_steps = 0L)(
#'   tibble(time_value = 1:5, value = 1:5),
#'   tibble(geo_value = 1),
#'   3:4
#' )
#'
#' upstream_slide_to_simple_hop(frollmean, .in_colnames = "value", .out_colnames = "slide_value", .before_n_steps = Inf, .after_n_steps = 0L)(
#'   tibble(time_value = 1:5, value = 1:5),
#'   tibble(geo_value = 1),
#'   3:4
#' )
#'
#' @keywords internal
upstream_slide_to_simple_hop <- function(.f, ..., .in_colnames, .out_colnames, .before_n_steps, .after_n_steps) {
  f_info <- upstream_slide_f_info(.f, ...)
  in_colnames <- .in_colnames # TODO refactor away
  out_colnames <- .out_colnames
  f_from_package <- f_info$from_package
  switch(f_from_package,
    data.table = if (.before_n_steps == Inf) {
      if (.after_n_steps != 0L) {
        stop(".before_n_steps only supported with .after_n_steps = 0")
      }
      function(grp_data, grp_key, ref_inds) {
        out_cols <- .f(x = grp_data[, in_colnames], n = seq_len(nrow(grp_data)), adaptive = TRUE, ...)
        names(out_cols) <- out_colnames
        vec_slice(new_tibble(out_cols, nrow = nrow(grp_data)), ref_inds)
      }
    } else {
      function(grp_data, grp_key, ref_inds) {
        out_cols <- .f(x = grp_data[, in_colnames], n = .before_n_steps + .after_n_steps + 1L, ...)
        if (.after_n_steps != 0L) {
          # Shift an appropriate amount of NA padding from the start to the end.
          # (This padding will later be cut off when we filter down to the
          # original time_values.)
          out_cols <- lapply(out_cols, function(out_col) {
            c(out_col[(.after_n_steps + 1L):length(out_col)], rep(NA, .after_n_steps))
          })
        }
        names(out_cols) <- out_colnames
        vec_slice(new_tibble(out_cols, nrow = nrow(grp_data)), ref_inds)
      }
    },
    slider =
    # TODO Inf checks?
      function(grp_data, grp_key, ref_inds) {
        out_cols <- lapply(in_colnames, function(in_colname) {
          .f(x = grp_data[[in_colname]], before = .before_n_steps, after = .after_n_steps, ...)
        })
        names(out_cols) <- out_colnames
        vec_slice(new_tibble(out_cols, nrow = nrow(grp_data)), ref_inds)
      },
    # TODO improve message
    stop("unsupported package")
  )
}

# ref_time_values_to_inp_ref_inds <- function(inp_tbl, ref_time_values) {
#   matches <- vec_match(ref_time_values, inp_tbl$time_value)
#   inp_ref_inds <- matches[!is.na(matches)]
#   inp_ref_inds
# }

# complete_for_time_slide <- function(inp_tbl, inp_ref_inds, before_n_steps, after_n_steps) {
#   if (before_n_steps == Inf) {
#     # We need to get back to inp_tbl[1L,] from inp_tbl[min(inp_ref_inds),]
#     start_padding <- min(inp_ref_inds) - 1L
#   } else {
#     start_padding <- before_n_steps
#   }
#   end_padding <- after_n_steps
#   #

#   slide_t_max <- out_t_max + after_n_steps * unit_step
#   slide_nrow <- time_delta_to_n_steps(slide_t_max - slide_t_min, time_type) + 1L
#   slide_time_values <- slide_t_min + 0L:(slide_nrow - 1L) * unit_step
#   slide_inp_backrefs <- vec_match(slide_time_values, inp_tbl$time_value)
# }

ref_time_values_to_out_time_values <- function(inp_tbl, ref_time_values) {
  vec_set_intersect(inp_tbl$time_value, ref_time_values)
}

slide_window <- function(inp_tbl, epikey, simple_hop, before_n_steps, after_n_steps, unit_step, time_type, out_time_values) {
  # TODO test whether origin time value stuff actually is helpful
  origin_time_value <- inp_tbl$time_value[[1L]]
  inp_ts <- time_minus_time_in_n_steps(inp_tbl$time_value, origin_time_value, time_type)
  out_ts <- time_minus_time_in_n_steps(out_time_values, origin_time_value, time_type)
  if (vec_size(out_ts) == 0L) {
    stop("FIXME TODO")
  } else {
    slide_ts <- seq(min(out_ts) - before_n_steps, max(out_ts) + after_n_steps) # TODO compare min/max vs. `[[`
  }
  slide_inp_backrefs <- vec_match(slide_ts, inp_ts)
  # TODO refactor to use a join if not using backrefs later anymore?
  #
  # TODO perf: try removing time_value column before slice?
  slide_tbl <- vec_slice(inp_tbl, slide_inp_backrefs)
  slide_tbl$time_value <- origin_time_value + slide_ts * unit_step

  ref_inds <- vec_match(out_ts, slide_ts)
  out_tbl <- simple_hop(slide_tbl, epikey, ref_inds)
  out_tbl
}



# # We should filter down the slide time values to ones in the input time values
#   # when preparing the output:
#   rows_should_keep1 <- !is.na(slide_inp_backrefs)
#   # We also need to apply the out_filter.
#   #
#   # TODO comments + test vs. just using inequality
#   rows_should_keep2 <- switch(out_filter_time_style,
#     range = vec_rep_each(
#       c(FALSE, TRUE, FALSE),
#       c(slide_start_padding_n, slide_nrow - slide_start_padding_n - after_n_steps, after_n_steps),
#     ),
#     set = vec_in(slide_time_values, out_time_values)
#   )
#   rows_should_keep <- rows_should_keep1 & rows_should_keep2
#   out_tbl <- vec_slice(slide, rows_should_keep)
#   out_tbl

# TODO maybe make ref_inds optional or have special handling if it's the whole sequence?  But can it ever be the full sequence in the common fixed-width window case?  Should be some truncation of it.

# TODO decide whether/where to put time range stuff

# TODO grp_ -> ek_ ?

# TODO "simple_hop" -> "skip"/"jump"?
