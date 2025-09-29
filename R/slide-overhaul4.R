#' Recycler function factory to get single-window computations to a common size
#'
#' This would be for `reframe`-like computations like `epix_slide`.
#'
#' @param results_env environment holding previous computations'
#'   results (which should be vectors); its entries may be mutated if
#'   the previous results need to be recycled to match a new common
#'   size.
#' @param common_monosize single integer; the common size of the
#'   results already in the `results_env`.  Should be `1L` if starting
#'   off with an empty results environment.  A value of `1L` means the
#'   size is currently 1L but previous results can be recycled to a
#'   larger size if we encounter it; any other value means that all
#'   additional results must be recyclable to that size. This value
#'   will be updated internally as the recycler is called; make sure
#'   to pass all results through the recycler first before adding them
#'   to the results environment so that this value remains valid.
#' @return a function that takes in a result and (i) validates size
#'   compatibility, (ii) updates its `common_monosize` state factoring
#'   in the new result state, (iii) recycles previous results in
#'   `results_env` (if needed), and (iv) returns the new result
#'   recycled to the new common size (but does not add it to the
#'   `results_env` as it may still need unpacked, etc.
#'
#' @examples
#'
#' results_env <- new.env(hash = FALSE, parent = emptyenv())
#' monoresult_common_recycler <- new_monoresult_common_recycler(results_env, 1L)
#' results_env[["a"]] <- monoresult_common_recycler(1)
#' results_env[["b"]] <- monoresult_common_recycler(1:5)
#' results_env[["c"]] <- monoresult_common_recycler(2)
#' rev(as.list(results_env, all.names = TRUE))
#' purrr::safely(monoresult_common_recycler)(1:3)
#'
#' @keywords internal
new_monoresult_common_recycler <- function(results_env, common_monosize) {
  function(monoresult_raw) {
    if (common_monosize != 1L) {
      # XXX could improve error messages here
      result_recycled <- vctrs::vec_recycle(monoresult_raw, common_monosize)
    } else {
      result_recycled <- monoresult_raw
      result_monosize <- vctrs::vec_size(result_recycled)
      if (result_monosize != 1L) {
        common_monosize <<- result_monosize
        for (previous_result_nm in names(results_env)) {
          results_env[[previous_result_nm]] <- vctrs::vec_recycle(results_env[[previous_result_nm]], common_monosize)
        }
      } # else `common_monosize` remains 1L
    }
    return(result_recycled)
  }
}

# XXX we may not actually use a nontrivial polyresult recycler if we
# don't add across support.

#' Recycler function factory to get polywindow computations to a common polysize
#'
#' This is analogous to [`new_monoresult_common_recycler`], but works
#' on computations that return "polyresults".  Working with
#' polyresults is essentially a vectorized version of working with
#' monoresults.  There is less computational overhead, but we don't
#' get immediate feedback on errors regarding the result classes and
#' sizes.  Thus, this recycler is meant to be used for specialized
#' multiwindow computations such as `frollmean`.
#'
#' Polywindow computation results are represented as an unnamed list
#' of two elements: (i) the result you would get from
#' `list_unchop`ping a list of the single-window results together, and
#' (ii) the "polysize" of the computation.  The polysize should be
#' the result you'd get from `list_sizes` on a list of single-window
#' results.
#'
#' @param results_env like in [`new_monoresult_common_recycler`], but
#'   instead of holding monoresults it will be holding unchopped
#'   polyresults
#' @param common_polysize the starting value of the common polysize;
#'   see above.  Analogous to `common_monosize` in
#'   [`new_monoresult_common_recycler`].
#' @return a function that takes in polyresults and outputs recycled,
#'   unchopped versions of them, along with validation and side
#'   effects as in [`new_monoresult_common_recycler`].
#'
#' @examples
#'
#' results_env <- new.env(parent = emptyenv())
#' polyresult_common_recycler <- new_polyresult_common_recycler(results_env, rep(1L, 3L))
#' results_env[["a"]] <- polyresult_common_recycler(list(c(0, 7, 8), rep(1L, 3L)))
#' results_env[["b"]] <- polyresult_common_recycler(list(c(1, 2:4, 1), c(1L, 3L, 1L)))
#' results_env[["c"]] <- polyresult_common_recycler(list(c(2:5, 3, 1), c(4L, 1L, 1L)))
#' results_env[["d"]] <- polyresult_common_recycler(list(c(0, 7, 8), rep(1L, 3L)))
#' rlang::env_get_list(results_env, letters[1:4])
#' purrr::safely(polyresult_common_recycler)(list(c(1:5, 1, 1), c(5L, 1L, 1L)))
#'
#' @keywords internal
new_polyresult_common_recycler <- function(results_env, common_polysize) {
  unconstrained_polysize <- rep(1L, length(common_polysize))
  # TODO consider bringing back the length-1 option, but only as a
  # computationally fast stand-in for rep(1L, N), not reps of other
  # values.  Also might have done common recycling a bit early before.
  function(polyresult_raw) {
    result_unchopped <- polyresult_raw[[1L]]
    result_polysize <- polyresult_raw[[2L]]
    sizes_incompatible <- result_polysize != 1L & common_polysize != 1L & result_polysize != common_polysize
    if (any(sizes_incompatible)) {
      first_incompatible_ind <- vctrs::vec_match(TRUE, sizes_incompatible)
      cli_abort(c(
        "{sum(sizes_incompatible)} of the {length(result_polysize)} subresults from a computation were incompatible with subresults from previous computations",
        "i" = "The first incompatible size was for subresult {first_incompatible_ind} of {length(result_polysize)},
               which was size {result_polysize[[first_incompatible_ind]]} in the current computation,
               incompatible with the size {common_polysize[[first_incompatible_ind]]} from previous computations."
      ))
    }
    if (!identical(common_polysize, unconstrained_polysize)) {
      new_common_polysize <- common_polysize
      new_common_polysize[common_polysize == 1L] <- result_polysize[common_polysize == 1L]
    } else {
      new_common_polysize <- result_polysize
    }
    if (identical(result_polysize, new_common_polysize)) {
      polyresult_unchopped_recycled <- result_unchopped
    } else {
      # Recycle using `vec_rep_each`.  When the ith result_polysize
      # entry is 1, we're repeating one subresult entry N times, so we
      # should generate one `times` entry, given by the ith
      # `new_common_polysize` entry.  When the ith result_polysize
      # entry is != 1, we're "repeating" N subresult entries 1 time
      # each, so we should generate N ones in the `times` vector.  We
      # also prepare this `times` vector using `vec_rep_each`.
      polyresult_unchopped_recycled <- vctrs::vec_rep_each(result_unchopped, vctrs::vec_rep_each(
        #                   Do we need to        | v-this many copies | Just one copy
        #                   recycle 1 elt or     | v of the next (1L) | each of the next
        #                   keep multiple elts?  | v element          | respolysize elts
        data.table::fifelse(result_polysize == 1L, new_common_polysize,              1L),
        data.table::fifelse(result_polysize == 1L,                  1L, result_polysize)
      ))
    }
    if (!identical(common_polysize, new_common_polysize)) {
      previous_result_recycle_times <- vctrs::vec_rep_each(
        data.table::fifelse(common_polysize == 1L, new_common_polysize,              1L),
        data.table::fifelse(common_polysize == 1L,                  1L, common_polysize)
      )
      for (previous_result_nm in names(results_env)) {
        results_env[[previous_result_nm]] <- vctrs::vec_rep_each(results_env[[previous_result_nm]], previous_result_recycle_times)
      }
    }
    common_polysize <<- new_common_polysize
    return (polyresult_unchopped_recycled)
  }
}

new_monoresult_required_recycler <- function(results_env, required_monosize) {
  function(monoresult_raw) {
    # TODO x_arg, call, or custom messaging
    #
    # TODO for required_monosize of 1, should this be something more
    # specialized?  Might also just want a more specialized recycler
    # for more custom messaging anyway...
    vctrs::vec_recycle(monoresult_raw, required_monosize)
  }
}

new_polyresult_required_recycler <- function(results_env, required_polysize) {
  stop("TODO finish")
}

new_polyresult_trivial_recycler <- function(results_env) {
  stop("TODO finish")
}

# TODO recycler -> size_policy? or back to sizer?

comp_result_unpack_assign <- function(comp_result, results_nonhashing_env, comp_name, comp_manually_named) {
  if (inherits(comp_result, "data.frame") && !comp_manually_named) {
    list2env(comp_result, results_nonhashing_env)
  } else {
    results_nonhashing_env[[comp_name]] <- comp_result
  }
}

apply_comp_quosures <- function(results_nonhashing_env, result_recycler,
                                comp_quos, manually_named,
                                quo_eval, ...) {
  nms <- names(comp_quos)
  for (quosure_i in seq_along(comp_quos)) {
    quosure_result_raw <- quo_eval(comp_quos[[quosure_i]], ...)
    if (obj_is_vector(quosure_result_raw) && is.null(vec_names(quosure_result_raw)) ||
          # vctrs considers data.frames to be vectors, but we still check
          # separately for them because certain base operations output data frames
          # with rownames, which we will allow (but might drop)
          is.data.frame(quosure_result_raw)) {
      # Check new result size and/or recycle new result (via return
      # value) and previous results (via mutation) to common size:
      quosure_result_recycled <- result_recycler(quosure_result_raw)
      # Unpack to multiple columns if appropriate:
      comp_result_unpack_assign(quosure_result_recycled, results_nonhashing_env, nms[[quosure_i]], manually_named[[quosure_i]])
    } else if (is.null(quosure_result_raw)) {
      nm <- nms[[quosure_i]]
      rlang::env_unbind(results_nonhashing_env, nm)
    } else {
      # FIXME TODO refactor this error handler alonside quo_eval.
      #
      # or... hoist loop outside of function, handle eval externally,
      # and feed in a cli_abort promise all ready for the error case?
      cli_abort("
            Problem with output of {.code
            {rlang::expr_deparse(rlang::quo_get_expr(comp_quos[[quosure_i]]))}}; it
            produced a result that was neither NULL, a data.frame, nor a vector
            without unnamed entries (as determined by the vctrs package).
          ", class = "epiprocess__invalid_slide_comp_tidyeval_output")
    }
  }
}

as_time_window_comp4 <- function(f, dots_quos, f_arg = caller_arg(f), call = caller_env()) {
  if (".col_names" %in% names(dots_quos)) {
    cli_abort(
      c("{.code epi_slide} and {.code epix_slide} do not support `.col_names`;
         consider:",
        "*" = "using {.code epi_slide_mean}, {.code epi_slide_sum}, or
               {.code epi_slide_opt}, if applicable",
        "*" = "using {.code .f = ~ .x %>%
               dplyr::reframe(across(your_col_names, list(your_func_name = your_func)))}"
      ),
      call = call,
      class = "epiprocess__as_slide_computation__given_.col_names"
    )
  }
  # stop("TODO determine output form... is this a fn of 1/2/3 args or some union?")
  if (missing(f)) {
    named_quos <- quos_auto_name(dots_quos) # resolves := among other things
    manually_named <- names2(dots_quos) != "" | vapply(dots_quos, function(quosure) {
      expression <- quo_get_expr(quosure)
      is.call(expression) && expression[[1L]] == sym(":=")
    }, FUN.VALUE = logical(1L))
    .ref_time_value_long_varnames <- ".ref_time_value"
    slide_comp_fn <- function(.x, .group_key, .ref_time_value) {
      x_as_env <- list2env(.x, parent = emptyenv())
      results_nonhashing_env <- new.env(FALSE, x_as_env)
      data_mask <- new_data_mask(bottom = results_nonhashing_env, top = x_as_env)
      data_mask$.data <- as_data_pronoun(data_mask)
      # We'll also install `.x` directly, not as an `rlang_data_pronoun`, so
      # that we can, e.g., use more dplyr and epiprocess operations. It won't be
      # (and doesn't make sense nrow-wise to be) updated with results as we loop
      # through the quosures.
      data_mask$.x <- .x
      data_mask$.group_key <- .group_key
      for (ref_time_value_long_varname in .ref_time_value_long_varnames) {
        data_mask[[ref_time_value_long_varname]] <- .ref_time_value
      }
      result_recycler <- new_monoresult_required_recycler(results_nonhashing_env, 1L)
      apply_comp_quosures(
        results_nonhashing_env, result_recycler,
        named_quos, manually_named, rlang::eval_tidy, data_mask
      )
      # validate_tibble(new_tibble(rev(as.list(results_nonhashing_env, all.names = TRUE))))
      new_data_frame(rev(as.list(results_nonhashing_env, all.names = TRUE)),
                     # TODO S3 extractor fn
                     n = environment(result_recycler)[["common_monosize"]])
      # ^ TODO consider providing nrow
      #
      # ^^ TODO consider removing/hoisting validation
      #
      # ^^^ TODO consider not actually constructing a tibble at this
      # point, or doing our own even-less-validated construction.  Or
      # accepting named lists and processing them as column lists.  Or
      # use our own builder / df subclass. Or use `new_data_frame`
      # which doesn't perform validation (consider including `n = `).
    }
    # list(
    #   fn = slide_comp_fn,
    #   out_names = "slide_value", # FIXME this isn't considering the
    #                              # manual-naming args in epi_slide();
    #                              # seems like same for the below
    #                              # cases; perhaps we need to take in
    #                              # these args, or perhaps this isn't
    #                              # the point at which to assign this
    #   out_manually_named = FALSE # FIXME
    # )
    slide_comp_fn
  } else if (is_function(f)) {
    # stop("TODO validate / fix n args")
    if (length(dots_quos) == 0L) {
      # Leaving `.f` unchanged slightly improves computation speed and trims
      # debug stack traces:
      slide_comp_fn <- f
    } else {
      slide_comp_fn <- partial(f, ...=, !!!dots_quos)
    }
    # list(
    #   fn = slide_comp_fn,
    #   out_names = "slide_value", # FIXME
    #   out_manually_named = FALSE # FIXME
    # )
    slide_comp_fn
  } else if (is_formula(f)) {
    stop("TODO finish")
  } else {
    stop("TODO balk")
  }
}

time_window_comp_to_simple_hop4 <- function(time_window_comp, before_n_steps, after_n_steps) {
  force(time_window_comp)
  force(before_n_steps)
  force(after_n_steps)
  if (before_n_steps == Inf) {
    stop("TODO, probably in separate function?")
  }
  #
  function(ek_data, ek, ref_inds) {
    ukey_colnames <- c(names(ek), "time_value")
    results <- slider::hop(
      ek_data,
      ref_inds - before_n_steps,
      ref_inds + after_n_steps,
      function(x) {
        ref_time_value <- vec_slice(x$time_value, vec_size(x) - after_n_steps)
        comp_result <- time_window_comp(x, ek, ref_time_value)
        comp_result
      }
    )

    if (!all(list_sizes(results) == 1L)) {
      cli_abort("Slide computations must all output results of size 1.")
      # TODO better message
      #
      # TODO only check if not tidyeval, which already should have checked?
    }

    return(vec_c(!!!results))
  }
}

epi_slide4 <- function(
    .x, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"), .ref_time_values = NULL,
    .new_col_name = NULL, .all_rows = FALSE) {
  # Validate arguments
  assert_no_hard_deprecated_epi_slide_args(rlang::call_match())
  assert_class(.x, "epi_df")
  # TODO instead of reversing completion, do we just warn on completion adding
  # rows? or just do it? or make it an option (maybe rename .all_rows & provide
  # 3 choices)?
  #
  if (vec_size(.x) == 0) {
    # We'll have 0 computation outputs to provide (we intersect
    # requested `.ref_time_values` with available `$time_value`s).  In
    # this situation, `dplyr` would run a computation on a 0-row input
    # with NA group key to produce output columns (differently for
    # summarize and group_modify).  That can lead to confusing error
    # messages, so let's deviate and just give up on adding output
    # columns.
    #
    # TODO consider hard error
    return(.x)
  }
  time_window_comp <- as_time_window_comp4(.f, enquos(...))
  # stop("FIXME properly use args...")
  before_n_steps <- .window_size - 1L
  after_n_steps <- 0L
  simple_hop <- time_window_comp_to_simple_hop4(time_window_comp, before_n_steps, after_n_steps)
  if (is.null(.ref_time_values)) {
    .ref_time_values <- sort(vec_unique(.x$time_value))
  } else {
    .ref_time_values <- vec_cast(.ref_time_values, .x$time_value)
    if (!test_subset(.ref_time_values, vec_unique(.x$time_value))) {
      cli_abort(
        "epi_slide: `ref_time_values` must be a unique subset of the time values in `x`.",
        class = "epiprocess__epi_slide_invalid_ref_time_values"
      )
    }
  }

  # TODO additional validation?

  time_type <- attr(.x, "metadata")$time_type
  unit_step <- unit_time_delta(time_type, "fast")

  # TODO consider determining the min_time_value by group so that this
  # is compatible with group_split, map(epi_slide), list_rbind.
  min_time_value <- min(.x$time_value)
  origin_time_value <- min_time_value # or any other time_value
  ref_timesteps <- time_minus_time_in_n_steps(.ref_time_values, origin_time_value, time_type)

  comps <- .x %>%
    group_by(pick(all_of(c("geo_value", attr(.x, "metadata")$other_keys)))) %>%
    group_map(function(ek_data, ek) {
      # TODO ensure arranged if needed
      #
      # TODO test whether origin time value stuff actually is helpful;
      # consider if can refactor to a with-ish function
      #
      # TODO consider with_temp_completion helper
      inp_timesteps <- time_minus_time_in_n_steps(ek_data$time_value, origin_time_value, time_type)
      out_timesteps <- vec_set_intersect(ref_timesteps, inp_timesteps)
      if (vec_size(out_timesteps) == 0L) {
        if (.all_rows) {
          return(vec_cbind(ek, ek_data))
        } else {
          return(vec_cbind(ek, ek_data)[0, ])
        }
      }
      slide_start_timestep <-
        if (before_n_steps == Inf) {
          min_time_value - origin_time_value
        } else {
          min(out_timesteps) - before_n_steps
        }
      slide_end_timestep <- max(out_timesteps) + after_n_steps
      slide_timesteps <- seq(slide_start_timestep, slide_end_timestep)
      slide_inp_backrefs <- vec_match(slide_timesteps, inp_timesteps)
      # TODO refactor to use a join if not using backrefs later anymore?
      #
      # TODO perf: try removing time_value column before slice?
      slide_tbl <- vec_slice(ek_data, slide_inp_backrefs)
      slide_tbl$time_value <- origin_time_value + slide_timesteps * unit_step

      ref_inds <- vec_match(out_timesteps, slide_timesteps)
      out_comp_result <- simple_hop(slide_tbl, ek, ref_inds)

      # Re-use some tidyeval stuff to perform unpacking and naming for
      # us.  It's a bit awkward, so TODO see if we can refactor away
      # the bits we need without impacting tidyeval performance and
      # without duplicating code.
      data_mask <- rlang::new_data_mask(new.env())
      unhashed_results_env <- new.env(FALSE, emptyenv())
      result_recycler <- identity # XXX or use a monoresult size checker in place of current size check
      if (is.null(.new_col_name)) {
        out_name <- "slide_value"
        out_manually_named <- FALSE
      } else {
        out_name <- .new_col_name
        out_manually_named <- TRUE
      }
      manually_named <- !is.null(.new_col_name)
      apply_comp_quosures(unhashed_results_env, result_recycler,
                          rlang::quos(
                            !!out_name := out_comp_result
                          ),
                          out_manually_named, rlang::eval_tidy, data_mask)
      out_subtbl <- new_tibble(rev(as.list(unhashed_results_env)))

      if (.all_rows) {
        res_timesteps <- inp_timesteps
        ek_data_subtbl <- ek_data
      } else {
        res_timesteps <- out_timesteps
        ek_data_subtbl <- vec_slice(ek_data, vec_match(res_timesteps, inp_timesteps))
      }
      if (identical(out_timesteps, res_timesteps)) {
        # TODO check whether this optimization needed
        maybe_result_row_inds_for_out_subtbl <- NULL
      } else {
        maybe_result_row_inds_for_out_subtbl <- vec_match(out_timesteps, res_timesteps)
      }
      result_tbl <- cbind_unpacked_comp4(ek, ek_data_subtbl, maybe_result_row_inds_for_out_subtbl, out_subtbl)
      result_tbl
    }) %>%
  list_rbind() %>%
  dplyr_reconstruct(.x)

  comps
}

cbind_unpacked_comp4 <- function(key_row, existing_val_tbl, maybe_subassign_row_inds, comp_subtbl) {
  is_part_of_ukey <- names(comp_subtbl) %in% names(key_row)
  key_overlap_names <- names(comp_subtbl)[is_part_of_ukey]
  if (!identical(vec_unique(comp_subtbl[is_part_of_ukey]), key_row[key_overlap_names])) {
    cli_abort("Computation must not output key columns with modified values")
    # TODO waldo compare etc.
  }
  if (any(names(comp_subtbl) %in% names(existing_val_tbl))) {
    cli_abort(c("Computation must not output pre-existing measurement column names",
                "x" = "Overlapping names: {format_chr_with_quotes(intersect(names(comp_subtbl), names(existing_val_tbl)))}"))
  }

  result <- vec_cbind(key_row, existing_val_tbl)
  comp_val_tbl <- comp_subtbl[!is_part_of_ukey]
  if (is.null(maybe_subassign_row_inds)) {
    result[names(comp_val_tbl)] <- comp_val_tbl
  } else {
    result[maybe_subassign_row_inds, names(comp_val_tbl)] <- comp_val_tbl
  }
  result
}
