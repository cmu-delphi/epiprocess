new_monoresult_common_monosizer <- function(results_env, common_monosize) {
  function(result_raw) {
    if (!is.null(common_monosize)) {
      # XXX could improve error messages here
      result_recycled <- vctrs::vec_recycle(result_raw, common_monosize)
    } else {
      result_recycled <- result_raw
      result_monosize <- vctrs::vec_size(result_raw)
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

new_polyresult_common_sizer <- function(results_env, common_polysize) {
  function(result_raw, result_polysize) { # TODO refactor to pass the size
    # # Suppose all are unchopped. size 1,3,1 * size 5,1,1.
    # # cumsum 1,4,5 * 5,6,7
    # # result sizes 5,3,1
    # # reps 5,1,1,1,1 * 1,1,1,1,1,3,1
    # # ---
    # # 1 --> 5, 1 times
    # # 3 --> 1, 3 times
    # # 1 --> 1, 1 time
    # # fifelse(131 == 1, 531, 1)
    # # fifelse(131 == 1, 1, 131)
    # # vec_rep_each(fifelse(a == 1, res, 1), fifelse(a == 1, 1, a))
    # # inds 1,1,1,1,1,2:4,5 * 1:5,6,6,6,7
    stop("TODO finish")
  }
}

new_monoresult_required_sizer <- function(results_env, required_monosize) {
  stop("TODO finish")
}

new_polyresult_required_sizer <- function(results_env, required_polysize) {
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
      quosure_result_recycled <- result_sizer(quosure_result_raw)
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
