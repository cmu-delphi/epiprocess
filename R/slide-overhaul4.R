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
#' results_env <- new.env(parent = emptyenv())
#' monoresult_common_recycler <- new_monoresult_common_recycler(results_env, 1L)
#' results_env[["a"]] <- monoresult_common_recycler(1)
#' results_env[["b"]] <- monoresult_common_recycler(1:5)
#' results_env[["c"]] <- monoresult_common_recycler(2)
#' rlang::env_get_list(results_env, letters[1:3])
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
      # `new_common_polysize` entry.  When the ith result_polysize entry
      # is != 1, we're "repeating" N subresult entries 1 time each, so
      # we should generate N ones in the `times` vector.
      polyresult_unchopped_recycled <- vctrs::vec_rep_each(result_unchopped, vctrs::vec_rep_each(
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
    vctrs::vec_recycle(monoresult_raw, required_monosize)
  }
}

new_polyresult_required_recycler <- function(results_env, required_polysize) {
  stop("TODO finish")
}

new_polyresult_trivial_recycler <- function(results_env) {
  stop("TODO finish")
}

apply_comp_quosures <- function(data_mask, results_env, results_names_sequence,
                                result_recycler,
                                comp_quos, manually_named) {
  # The results_env is an environment; it doesn't track the binding order.
  # We'll track that separately. For efficiency, we'll use `c` to add to
  # this order, and deal with binding redefinitions at the end. We'll
  # reflect deletions immediately (current implementation of `new_tibble`
  # seems like it would exclude `NULL` bindings for us but `?new_tibble`
  # doesn't reflect this behavior).
  nms <- names(comp_quos)
  for (quosure_i in seq_along(comp_quos)) {
    quosure_result_raw <- rlang::eval_tidy(comp_quos[[quosure_i]], data_mask)
    if (is.null(quosure_result_raw)) {
      nm <- nms[[quosure_i]]
      results_names_sequence <- vctrs::vec_set_difference(results_names_sequence, nm)
      rlang::env_unbind(results_env, nm)
    } else if (
      # vctrs considers data.frames to be vectors, but we still check
      # separately for them because certain base operations output data frames
      # with rownames, which we will allow (but might drop)
      is.data.frame(quosure_result_raw) ||
        vctrs::obj_is_vector(quosure_result_raw) && is.null(vctrs::vec_names(quosure_result_raw))
    ) {
      # Check new result size and/or recycle new result (via return
      # value) and previous results (via mutation) to common size:
      quosure_result_recycled <- result_recycler(quosure_result_raw)
      # Unpack to multiple columns if appropriate:
      if (inherits(quosure_result_recycled, "data.frame") && !manually_named[[quosure_i]]) {
        new_results_names_sequence <- names(quosure_result_recycled)
        results_names_sequence <- c(results_names_sequence, new_results_names_sequence)
        for (new_result_i in seq_along(quosure_result_recycled)) {
          results_env[[new_results_names_sequence[[new_result_i]]]] <- quosure_result_recycled[[new_result_i]]
        }
      } else {
        nm <- nms[[quosure_i]]
        results_names_sequence <- c(results_names_sequence, nm)
        results_env[[nm]] <- quosure_result_recycled
      }
    } else {
      cli_abort("
            Problem with output of {.code
            {rlang::expr_deparse(rlang::quo_get_expr(comp_quos[[quosure_i]]))}}; it
            produced a result that was neither NULL, a data.frame, nor a vector
            without unnamed entries (as determined by the vctrs package).
          ", class = "epiprocess__invalid_slide_comp_tidyeval_output")
    }
  }
  return(results_names_sequence)
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
    ref_time_value_long_varnames <- ".ref_time_value"
    slide_comp_fn <- function(.x, .group_key, .ref_time_value) {
      x_as_env <- rlang::as_environment(.x)
      data_mask <- rlang::new_data_mask(bottom = results_env, top = x_as_env)
      data_mask$.data <- rlang::as_data_pronoun(data_mask)
      # We'll also install `.x` directly, not as an `rlang_data_pronoun`, so
      # that we can, e.g., use more dplyr and epiprocess operations. It won't be
      # (and doesn't make sense nrow-wise to be) updated with results as we loop
      # through the quosures.
      data_mask$.x <- .x
      data_mask$.group_key <- .group_key
      for (ref_time_value_long_varname in .ref_time_value_long_varnames) {
        data_mask[[ref_time_value_long_varname]] <- .ref_time_value
      }
      results_env <- new.env(parent = emptyenv())
      result_recycler <- new_monoresult_required_recycler(results_env, 1L)
      results_names_sequence <- character()
      results_names_sequence <- apply_comp_quosures(
        data_mask, results_env, results_names_sequence, result_recycler,
        named_quos, manually_named
      )
      stop("TODO is there an efficient way to make the names sequence nice immediately?  Or just move to unhashed env & rev?")
      # If a binding was defined and redefined, we may have
      # duplications within `results_multiorder`. Because
      # `unique(results_multiorder)` is actually quite slow, we'll
      # keep the duplicates (--> duplicate result columns) and leave
      # it to various `mutate` in epi[x]_slide to resolve this to the
      # appropriate placement:
      validate_tibble(new_tibble(as.list(results_env, all.names = TRUE)[results_multiorder]))
    }
    stop("TODO finish")
  } else if (is_function(f)) {
    # stop("TODO validate / fix n args")
    if (length(dots_quos) == 0L) {
      # Leaving `.f` unchanged slightly improves computation speed and trims
      # debug stack traces:
      slide_comp_fn <- f
    } else {
      slide_comp_fn <- partial(f, ...=, !!!dots_quos)
    }
    list(
      fn = slide_comp_fn,
      out_names = "slide_value", # FIXME
      out_manually_named = FALSE # FIXME
    )
  } else if (is_formula(f)) {
    stop("TODO finish")
  } else {
    stop("TODO balk")
  }
}
