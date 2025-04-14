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
      .i = grp_data$time_value,
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
  in_colnames <- .in_colnames
  out_colnames <- .out_colnames
  f_from_package <- f_info$from_package
  switch(f_from_package,
    data.table = if (.before_n_steps == Inf) {
      if (.after_n_steps != 0L) {
        stop(".before_n_steps only supported with .after_n_steps = 0")
      }
      function(grp_data, grp_key, ref_inds) {
        .f(x = grp_data[, in_colnames], n = seq_len(nrow(grp_data)), adaptive = TRUE, ...)
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
        out_cols
      }
    },
    slider =
    # TODO Inf checks?
      function(grp_data, grp_key, ref_inds) {
        names(in_colnames) <- in_colnames
        lapply(in_colnames, function(in_colname) {
          .f(x = grp_data[[in_colname]], before = .before_n_steps, after = .after_n_steps, ...)
        })
      },
    # TODO improve message
    stop("unsupported package")
  )
}

# TODO maybe make ref_inds optional or have special handling if it's the whole sequence?  But can it ever be the full sequence in the common fixed-width window case?  Should be some truncation of it.

# TODO decide whether/where to put time range stuff

# TODO grp_ -> ek_ ?

# TODO "simple_hop" -> "skip"/"jump"?

# TODO tacking on output columns -> outputting output columns
