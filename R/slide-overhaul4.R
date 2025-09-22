new_result_common_monosizer <- function(results_env, common_monosize) {
  function(result_raw, result_monosize) {
    if (!is.null(common_monosize)) {
      # XXX could improve error messages here
      result_recycled <- vctrs::vec_recycle(result_raw, common_monosize)
    } else {
      result_recycled <- result_raw
      if (result_monosize != 1L) {
        common_monosize <- result_monosize
        for (previous_result_nm in names(results_env)) {
          results_env[[previous_result_nm]] <- vctrs::vec_recycle(results_env[[previous_result_nm]], common_monosize)
        }
      } # else `common_monosize` remains NULL
    }
    return(result_recycled)
  }
}

new_result_common_polysizer <- function(results_env, common_polysize) {
  function(result_raw, result_polysize) {
    # If all N subresults are of size K, we might represent the
    # polysize as just K instead of rep(K, N).  If we're dealing with
    # a mix of this abbreviated representation and a length-N
    # polysize, we need to make sure to recycle.
    polysizes <- vctrs::vec_recycle_common(result_polysize, common_polysize)
    result_polysize <- polysizes[[1L]]
    common_polysize <- polysizes[[2L]]
    # Now, deal with the subresult sizes within the polysizes:
    sizes_incompatible <- result_polysize != 1L & common_polysize != 1L & result_polysize != common_polysize
    if (any(sizes_incompatible)) {
      first_incompatible_ind <- vctrs::vec_match(TRUE, sizes_incompatible)
      cli_abort(c(
        "{sum(sizes_incompatible)} of {length(result_polysize)} subresults from a computation were incompatible with subresults from previous computations",
        "i" = "The first incompatible size was for subresult {first_incompatible_ind},
               which was size {result_polysize[[first_incompatible_ind]]} in the current computation,
               incompatible with the size {common_polysize[[first_incompatible_ind]]}"
      ))
    }
    new_common_polysize <- common_polysize
    new_common_polysize[result_polysize != 1L] <- result_polysize[result_polysize != 1L]
    if (!identical(result_size, new_common_polysize)) {
      # Recycle using `vec_rep_each`.  When the ith result_polysize
      # entry is 1, we're repeating one subresult entry N times, so we
      # should generate one `times` entry, given by the ith
      # `new_common_polysize` entry.  When the ith result_polysize entry
      # is != 1, we're "repeating" N subresult entries 1 time each, so
      # we should generate N ones in the `times` vector.
      result_recycled <- vctrs::vec_rep_each(result_raw, vctrs::vec_rep_each(
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
    common_size <- new_common_size
    stop("TODO finish; return value + refactor usage to set up and pass the second arg; maybe consider some common subexpression elimination")
  }
}

new_result_required_monosizer <- function(results_env, required_monosize) {
  stop("TODO finish")
}

new_result_required_polysizer <- function(results_env, required_polysize) {
  stop("TODO finish")
}

apply_comp_quosures <- function(data_mask, results_env, results_names_sequence, result_sizer, comp_quos, manually_named) {
  # common_size <- NULL
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
      quosure_result_recycled <- result_sizer(quosure_result_raw, vctrs::vec_size(quosure_result_raw))
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

# data_mask <- as_data_mask(tibble(a = 1:7 + 0))
# comp_quosures <- rlang::quos(s = sum(a))
# results_env <- new.env(parent = emptyenv())
# results_names_sequence <- character()
# result_sizer <- new_result_common_sizer(results_env, NULL)
# rlang::env_get_list(results_env, apply_comp_quosures(data_mask, results_env, results_names_sequence, result_sizer, comp_quosures, FALSE))
