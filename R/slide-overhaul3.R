
# # minicomp: take a window, produce a value.  has a name.  flag for whether manually named.

# # using a minicomp:
# # 1. call minicomp on window + extras + running comp(?) to get result
# # 2. common size check & update
# # 3. unpack (accounting for name info)
# # 4. name checks & simple bind into result (data mask and list?)

# # using a tidy minicomp:
# # 0. (be passed or set up a mask of the window)
# # ...

# # multiminicomp: could possibly vectorize some size&name checks but might want those to abort immediately + common-size recycling needs to happen immediately

# # megacomp / hop:
# # 0. megawindow & hop index setup
# # 1. call hop on tibble + indices to get results + sizes (assuming unchopped form...)
# # 2. common sizes checks & updates (if necessary...)
# # 3. unpack (accounting for name info)
# # 4. name checks & ref_time_values-aware binding into result (tibble)

# # hop_chopped

# # hop_unchopped

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

# # "window comp" & "multiwindow comp"?

# eval_quosure <- function(mask, lefter_results, required_size,
#                          quosure, quosure_name, quosure_manually_named) {
#   quosure_result_raw <- rlang::eval_tidy(quosure, data_mask)
#   quosure_result_unpacked <- single_comp_unpack(quosure_result_raw, quosure_name, quosure_manually_named)
#   # new_size <- vec_size_common(!!!lefter_results, quosure_result_unpacked, .size = required_size)
#   # quosure_result_unpacked_recycled <- vec_recycle(quosure_result_unpacked, new_size)
#   # new_results <- vec_recycle_common(!!!lefter_results, quosure_result_unpacked, .size = required_size)
#   # # FIXME doesn't use name.  also seems like could have an issue if
#   # # have many comp cols since !!! could also be on list so it's
#   # # probably looking at several sizes
#   # # if (vec_size(new_results) != vec_size(lefter_results)) {
#   # #   env_bind(mask, !!!new_results) # NOTE sets to NULL on NULL
#   # # }
#   size <- vec_size_common(lefter_results[[1L]], quosure_result_unpacked, .size = required_size)
#   quosure_result_unpacked_recycled <- vec_recycle(quosure_result_unpacked, new_size)
#   if (!identical(size, vec_size(lefter_results[[1L]]))) {
#     stop("TODO finish")
#   }

#   stop("TODO finish")
# }

# new_comp_results <- function(binding_env, col_mods = list(), size = NULL) {
#   comp_results <- new.env(parent = emptyenv()) # TODO: size?
#   comp_results[["binding_env"]] <- binding_env
#   comp_results[["binding_order"]] <- binding_order
#   comp_results[["size"]] <- size
#   comp_results
# }

# # comp_results_add_flexible_result <- function(comp_results, flexible_result, result_name, result_was_manually_named) {
# #   binding_env <- comp_results[["binding_env"]]
# #   old_binding_order <- comp_results[["binding_order"]]
# #   old_size <- comp_results[["size"]]

# #   if (is.null(flexible_result)) {
# #     binding_env[[result_name]] <- NULL
# #     comp_results[["binding_order"]] <- vctrs::vec_set_difference(old_binding_order, result_name)
# #     return(comp_results)
# #   }

# #   if (!is.null(old_size)) {
# #     # XXX could improve error messages here
# #     recycled_packed_result <- vctrs::vec_recycle(flexible_result, old_size)
# #   } else {
# #     recycled_packed_result <- flexible_result
# #     result_size <- vec_size(recycled_packed_result)
# #     if (result_size != 1L) {
# #       comp_results[["size"]] <- result_size
# #       for (previous_result_nm in names(binding_env)) {
# #         binding_env[[previous_result_nm]] <- vctrs::vec_recycle(binding_env[[previous_result_nm]], common_size)
# #       }
# #     }
# #   }

# #   if (is.data.frame(flexible_result) && !result_was_manually_named) {
# #     ragged_result_binding_order <- flexible_result # will get listified when c'd
# #   } else {
# #     ragged_result_binding_order <- list(flexible_result)
# #     names(ragged_result_binding_order) <- result_name
# #   }



# #   size <- vctrs::vec_size_common(comp_results[["binding_order"]][[1L]], !!!ragged_result_binding_order, .size = required_size)
# #   result_binding_order <- vctrs::vec_recycle_common(ragged_result_binding_order, size)
# #   if (size !=

# #   stop("FIXME size-related stuff")

# #   binding_env <- comp_results[["binding_env"]]
# #   for (result_col_mod_i in seq_along(result_binding_order)) {
# #     result_col_mod_name <- names(result_binding_order)[[result_col_mod_i]]
# #     binding_env[[result_col_mod_name]] <- result_binding_order[[result_col_mod_i]]
# #   }

# #   comp_results[["binding_order"]] <- c(comp_results[["binding_order"]], result_binding_order)
# # }


# comp_results_bind_flexible_result <- function(comp_results, flexible_result, result_name, result_was_manually_named) {
#   results_env <- comp_results[["result_env"]]
#   old_results_multiorder <- comp_results[["results_multiorder"]]
#   if (is.null(flexible_result)) {
#     rlang::env_unbind(results_env, result_name)
#     results_env[["results_multiorder"]] <- vctrs::vec_set_difference(old_results_multiorder, result_name)
#   } else if (
#     # vctrs considers data.frames to be vectors, but we still check
#     # separately for them because certain base operations output data frames
#     # with rownames, which we will allow (but might drop)
#     is.data.frame(flexible_result) ||
#       vctrs::obj_is_vector(flexible_result) && is.null(vctrs::vec_names(flexible_result))
#   ) {
#     old_common_size <- result_env[["common_size"]]
#     # We want something like `dplyr_col_modify()` but allowing recycling
#     # of previous computations and updating `results_env` and unpacking
#     # tibbles if not manually named.
#     if (!is.null(old_common_size)) {
#       # XXX could improve error messages here
#       FIXME reflecting stuff in comp_results.... or instead parameterize the flexible result production
#       flexible_result_recycled <- vctrs::vec_recycle(flexible_result, old_common_size)
#     } else {
#       flexible_result_recycled <- flexible_result
#       flexible_result_size <- vctrs::vec_size(flexible_result)
#       if (flexible_result_size != 1L) {
#         comp_results[["common_size"]] <- flexible_result_size
#         for (previous_result_name in names(results_env)) {
#           results_env[[previous_result_name]] <- vctrs::vec_recycle(results_env[[previous_result_name]], flexible_result_size)
#         }
#       } # else `comp_results$common_size` remains NULL
#     }
#     if (inherits(flexible_result_recycled, "data.frame") && !result_was_manually_named) {
#       additional_results_multiorder <- names(flexible_result_recycled)
#       results_multiorder <- c(results_multiorder, additional_results_multiorder)
#       for (new_result_i in seq_along(flexible_result_recycled)) {
#         results_env[[additional_results_multiorder[[new_result_i]]]] <- flexible_result_recycled[[new_result_i]]
#       }
#     } else {
#       results_multiorder <- c(results_multiorder, result_name)
#       results_env[[result_name]] <- flexible_result_recycled
#     }
#   } else {
#     FIXME
#     cli_abort("
#             Problem with output of {.code
#             {rlang::expr_deparse(rlang::quo_get_expr(dots_quos[[quosure_i]]))}}; it
#             produced a result that was neither NULL, a data.frame, nor a vector
#             without unnamed entries (as determined by the vctrs package).
#           ", class = "epiprocess__invalid_slide_comp_tidyeval_output")
#   }
# }
