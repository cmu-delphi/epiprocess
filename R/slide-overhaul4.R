

apply_comp_quosures <- function(data_mask, comp_quos, size_spec) {
  common_size <- NULL
  # The data mask is an environment; it doesn't track the binding order.
  # We'll track that separately. For efficiency, we'll use `c` to add to
  # this order, and deal with binding redefinitions at the end. We'll
  # reflect deletions immediately (current implementation of `new_tibble`
  # seems like it would exclude `NULL` bindings for us but `?new_tibble`
  # doesn't reflect this behavior).
  results_multiorder <- character(0L)
  for (quosure_i in seq_along(dots_quos)) {
    quosure_result_raw <- rlang::eval_tidy(dots_quos[[quosure_i]], data_mask)
    if (is.null(quosure_result_raw)) {
      nm <- nms[[quosure_i]]
      results_multiorder <- vctrs::vec_set_difference(results_multiorder, nm)
      rlang::env_unbind(results_env, nm)
    } else if (
      # vctrs considers data.frames to be vectors, but we still check
      # separately for them because certain base operations output data frames
      # with rownames, which we will allow (but might drop)
      is.data.frame(quosure_result_raw) ||
        vctrs::obj_is_vector(quosure_result_raw) && is.null(vctrs::vec_names(quosure_result_raw))
    ) {
      # We want something like `dplyr_col_modify()` but allowing recycling
      # of previous computations and updating `results_env` and unpacking
      # tibbles if not manually named.
      if (!is.null(common_size)) {
        # XXX could improve error messages here
        quosure_result_recycled <- vctrs::vec_recycle(quosure_result_raw, common_size)
      } else {
        quosure_result_recycled <- quosure_result_raw
        quosure_result_size <- vctrs::vec_size(quosure_result_raw)
        if (quosure_result_size != 1L) {
          common_size <- quosure_result_size
          for (previous_result_nm in names(results_env)) {
            results_env[[previous_result_nm]] <- vctrs::vec_recycle(results_env[[previous_result_nm]], common_size)
          }
        } # else `common_size` remains NULL
      }
      stop("TODO size adjustment seems like what needs to be replaceable with multisize")
      stop("TODO what about data mask stuff?  multicomp needing multimask or submask reformulation? or is there way to avoid?")
      # Idea is that fast slides would be multicomps with a regular
      # data_mask (for the group) and multisize handling, while
      # (sequences of) slow window comp args would be multicomps
      # generated from singlecomps with singlesize handling, with
      # data_masks for each window... likely (re)created each time
      # need to visit the window + incorporating relevant previous
      # results from multiresult.
      if (inherits(quosure_result_recycled, "data.frame") && !manually_named[[quosure_i]]) {
        new_results_multiorder <- names(quosure_result_recycled)
        results_multiorder <- c(results_multiorder, new_results_multiorder)
        for (new_result_i in seq_along(quosure_result_recycled)) {
          results_env[[new_results_multiorder[[new_result_i]]]] <- quosure_result_recycled[[new_result_i]]
        }
      } else {
        nm <- nms[[quosure_i]]
        results_multiorder <- c(results_multiorder, nm)
        results_env[[nm]] <- quosure_result_recycled
      }
    } else {
      cli_abort("
            Problem with output of {.code
            {rlang::expr_deparse(rlang::quo_get_expr(dots_quos[[quosure_i]]))}}; it
            produced a result that was neither NULL, a data.frame, nor a vector
            without unnamed entries (as determined by the vctrs package).
          ", class = "epiprocess__invalid_slide_comp_tidyeval_output")
    }
  }
}
