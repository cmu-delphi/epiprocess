epi_slide2 <- function(
    .x, .f, ...,
    .window_size = NULL, .align = c("right", "center", "left"), .ref_time_values = NULL,
    .new_col_name = NULL, .all_rows = FALSE) {
  # Validate arguments
  assert_no_hard_deprecated_epi_slide_args(rlang::call_match())
  assert_class(.x, "epi_df")
  # XXX TODO we will use groups of .x to determine completions to perform. also
  # will change the geo x ref_time_values to output?? except want size stability??
  #
  # TODO instead of reversing completion, do we just warn on completion adding
  # rows? or just do it? or make it an option (maybe rename .all_rows & provide
  # 3 choices)?
  #
  if (nrow(.x) == 0L) {
    return(.x)
  }
  time_window_comp <- as_time_window_comp(.f, enquos(...))
  # stop("FIXME properly use args...")
  before_n_steps <- .window_size - 1L
  after_n_steps <- 0L
  simple_hop <- time_window_comp_to_simple_hop(time_window_comp, before_n_steps, after_n_steps)
  if(is.null(.ref_time_values)) {
    .ref_time_values <- sort(unique(.x$time_value))
  } else {
    .ref_time_values <- vec_cast(.ref_time_values, .x$time_value)
    if (!test_subset(.ref_time_values, unique(.x$time_value))) {
      cli_abort(
        "epi_slide: `ref_time_values` must be a unique subset of the time values in `x`.",
        class = "epiprocess__epi_slide_invalid_ref_time_values"
      )
    }
  }

  # TODO additional validation?


  # TODO this should be shared with epi_slide_opt
  # .x %>%
  #   group_modify(function(grp_data, grp_key) {
  #     grp_ref_time_values <- vec_set_intersect(.ref_time_values, grp_data$time_value)
  #     if (vec_size(grp_ref_time_values) == 0L) {
  #       if (.all_rows) {
  #         return (grp_data)
  #       } else {
  #         return (vec_slice(grp_data, integer()))
  #       }
  #     }

  #     # grp_data %>%
  #     #   with_temp_completion(
  #     #     quos(epikeys__, time_value = _____),
  #     #     function(completed) {
  #     #       completed %>%
  #     #         group_by(epikeys____) %>%
  #     #         group_modify(function(ek_data, ek) {
  #     #           simple_hop(ek_data, ek, ref_inds!!)
  #     #           stop("don't have ref_inds to pass...")
  #     #         }) %>%
  #     #         ungroup()
  #     #     }
  #     #   )

  #     # grp_data %>%
  #     #   group_by(pick(all_of(key_colnames(.x)))) %>%
  #     #   group_modify(function(ek_data, ek) {
  #     #     ek_data %>%
  #     #       with_temp_completion(
  #     #         quos(time_value = ...),
  #     #         function(ek_data_completed, ref_inds) {
  #     #           ek_data_completed %>%
  #     #             simple_hop(ek_data_completed, ek, ref_inds)
  #     #         }
  #     #       )
  #     #   }) %>%
  #     #   ungroup()

  #     stop("TODO complete")
  #     stop("TODO hop")
  #     stop("TODO decomplete")
  #   })

  time_type <- attr(.x, "metadata")$time_type
  unit_step <- unit_time_delta(time_type, "fast")

  comps <- .x %>%
    group_map(.keep = TRUE, function(grp_data, grp_key) {
      # TODO reconsider this grouping behavior...

      # grp_ref_time_values <- vec_set_intersect(.ref_time_values, grp_data$time_value)
      # if (vec_size(grp_ref_time_values) == 0L) {
      #   return(new_tibble(list(), nrow = 0L))
      # }
      grp_min_time_value <- min(grp_data$time_value)
      grp_max_time_value <- max(grp_data$time_value)
      origin_time_value <- grp_min_time_value

      grp_data %>%
        group_by(pick(all_of(c("geo_value", attr(.x, "metadata")$other_keys)))) %>%
        group_map(function(ek_data, ek) {
          # TODO ensure arranged if needed
          ek_ref_time_values <- vec_set_intersect(.ref_time_values, ek_data$time_value)
          # TODO test whether origin time value stuff actually is helpful;
          # consider if can refactor to a with-ish function
          inp_timesteps <- time_minus_time_in_n_steps(ek_data$time_value, origin_time_value, time_type)
          out_timesteps <- time_minus_time_in_n_steps(ek_ref_time_values, origin_time_value, time_type)
          if (vec_size(out_timesteps) == 0L) {
            stop("TODO")
          }
          slide_start_timestep <-
            if (before_n_steps == Inf) {
              grp_min_time_value - origin_time_value
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
          # out_vec <- simple_hop(slide_tbl, epikey, ref_inds)
          # TODO optimize
          # out_tbl <- tibble(epikey = ek, time_value = out_timesteps, slide_result = out_vec)
          # out_tbl <- new_tibble(vctrs::vec_recycle_common(epikey = ek, time_value = out_timesteps, slide_result = out_vec))
          # out_vecs <- simple_hop(slide_tbl, ek, ref_inds)
          # out_tbl <- new_tibble(vctrs::vec_recycle_common(epikey = ek, time_value = out_timesteps, !!!out_vecs))
          out_subtbl <- simple_hop(slide_tbl, ek, ref_inds)
          maybe_result_row_inds_for_out_subtbl <-
            if (identical(inp_timesteps, out_timesteps)) {
              NULL
            } else {
              vec_match(out_timesteps, inp_timesteps)
            }
          result_tbl <- cbind_unpacked_comp(ek, ek_data, maybe_result_row_inds_for_out_subtbl, out_subtbl)
          result_tbl
        })
    }) %>%
  list_flatten() %>%
  list_rbind() %>%
  dplyr_reconstruct(.x)

  # TODO unpack epikey
  # TODO unpack/name result

  # TODO combine with input...

  comps
}

# TODO try using
with_integer_time_values <- function(edf, fn, origin_time_value = edf$time_value[[1L]]) {
  time_type <- attr(edf, "metadata")$time_type
  edf$time_value <- time_minus_time_in_n_steps(edf$time_value, origin_time_value, time_type)
  attr(edf, "metadata")$time_type <- "integer"
  result <- fn(edf)
  result$time_type <- time_plus_n_steps(origin_time_value, edf$time_value)
  attr(result, "metadata")$time_type <- time_type
  result
}

# TODO think simple hop will need to be list / have attribute for output col name info, so can reduce checks


# FIXME epi_slide2 is way slower



as_time_window_comp <- function(f, dots_quos, f_arg = caller_arg(f), call = caller_env()) {
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
      results_env <- new.env(parent = x_as_env)
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
      stop("FIXME it'd be nice to replace the below with something simple, but the data mask needs to be updated...")
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
          results_multiorder <- results_multiorder[results_multiorder != nm]
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
      # If a binding was defined and redefined, we may have duplications within
      # `results_multiorder`. `unique(results_multiorder, fromLast = TRUE)` is
      # actually quite slow, so we'll keep the duplicates (--> duplicate result
      # columns) and leave it to various `mutate` in epi[x]_slide to resolve
      # this to the appropriate placement:
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

# #'
# #'
# #' @examples
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(slide_value = 5), FALSE
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(v2 = 5, v3 = 7), c(TRUE, TRUE)
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(slide_value = tibble(k = 1, v2 = 5, v3 = 7)), FALSE
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(result_tbl = tibble(v2 = 5, v3 = 7)), TRUE
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(autonamed = tibble(v2 = 5, v3 = 7), v4 = 9), c(FALSE, TRUE)
# #' )
# #'
# #' \dontrun{
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(autonamed = tibble(v2 = 1:4)), FALSE
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(autonamed = tibble(v1 = 1)), FALSE
# #' )
# #' cbind_unpacked_comps(
# #'   tibble(k = 1, v1 = 1), "k",
# #'   tibble(slide_value = tibble(k = 2, v2 = 5, v3 = 7)), FALSE
# #' )
# #' }
# #'
# #' # TODO value tests
# cbind_unpacked_comps <- function(existing_tbl, existing_ukey_colnames, comps_tbl, comp_manually_named) {
#   # TODO should size checks be separated out into another function/stage?  are size checks redundant?

#   # XXX vs. comps_list.... if autoname overlaps, then will need to ensure
#   # minimal tbl name checks, a bit nonidiomatic. But if list, may need to do
#   # some recycling or size checks
#   if (nrow(comps_tbl) != nrow(existing_tbl)) {
#     cli_abort("Computation must output a result of size {nrow(existing_tbl)}, not {nrow(comps_tbl)}")
#   }
#   comps_tbl <- bind_cols(lapply(seq_along(comps_tbl), function(comp_i) {
#     comp <- comps_tbl[[comp_i]]
#     if (is.data.frame(comp) && !comp_manually_named[[comp_i]]) {
#       comp
#     } else {
#       comps_tbl[comp_i]
#     }
#   }), .name_repair = "check_unique")
#   # XXX ^ vs. tidyr::unpack
#   is_part_of_ukey <- names(comps_tbl) %in% existing_ukey_colnames
#   if (!identical(
#     comps_tbl[is_part_of_ukey],
#     existing_tbl[names(comps_tbl)[is_part_of_ukey]]
#   )) {
#     cli_abort("Computation must not output key columns with modified values")
#     # TODO waldo compare etc.
#   }
#   existing_val_colnames <- names(existing_tbl)[! names(existing_tbl) %in% existing_ukey_colnames]
#   comp_val_colnames <- names(comps_tbl)[!is_part_of_ukey]
#   if (any(comp_val_colnames %in% existing_val_colnames)) {
#     cli_abort(c("Computation must not output pre-existing measurement column names",
#                 "x" = "Overlapping names: {format_chr_with_quotes(intersect(comp_val_colnames, existing_val_colnames))}"))
#   }

#   bind_cols(existing_tbl, comps_tbl[!is_part_of_ukey], .name_repair = "minimal")
# }

# TODO change to just have attempt-unpack names?
comp_unpack <- function(results, assigned_names, manually_named) {
  stopifnot(identical(names(results), assigned_names))
  to_unpack <- vapply(results, is.data.frame, logical(1L)) & !manually_named
  unpack(results, all_of(assigned_names[to_unpack]))
}

cbind_unpacked_comp <- function(key_row, existing_val_tbl, maybe_subassign_row_inds, comp_subtbl) {
  expected_comp_subtbl_size <-
    if(is.null(maybe_subassign_row_inds)) {
      nrow(existing_val_tbl)
    } else {
      length(maybe_subassign_row_inds)
    }
  if (nrow(comp_subtbl) != expected_comp_subtbl_size) {
    cli_abort("Computation must output a result of size {expected_comp_subtbl_size}, not {nrow(comp_subtbl)}")
  }

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

  # result <- vec_cbind(key_row, existing_val_tbl, comp_subtbl[!is_part_of_ukey], .name_repair = "minimal")
  # result
  # result <- bind_cols(key_row, existing_val_tbl, comp_subtbl[!is_part_of_ukey], .name_repair = "minimal")
  # result
  # result <- vec_rep(key_row, vec_size(existing_val_tbl))
  # result[names(existing_val_tbl)] <- existing_val_tbl
  # result <- vec_cbind(key_row, existing_val_tbl)
  # comp_val_tbl <- comp_subtbl[!is_part_of_ukey]
  # result[names(comp_val_tbl)] <- comp_val_tbl
  # result

  result <- vec_cbind(key_row, existing_val_tbl)
  comp_val_tbl <- comp_subtbl[!is_part_of_ukey]
  if (is.null(NULL)) {
    result[names(comp_val_tbl)] <- comp_val_tbl
  } else {
    result[maybe_subassign_row_inds, names(comp_val_tbl)] <- comp_val_tbl
  }
  result

  # comp_val_tbl <- comp_subtbl[!is_part_of_ukey]
  # comp_val_tbl_complete <- vec_rep(vec_cast(NA, comp_val_tbl), vec_size(existing_val_tbl))
  # # comp_val_tbl_complete <- vctrs::vec_init(comp_val_tbl, vec_size(existing_val_tbl))
  # vec_slice(comp_val_tbl_complete, maybe_subassign_row_inds) <- comp_val_tbl
  # result <- vec_cbind(key_row, existing_val_tbl, comp_val_tbl_complete)
  # result
}

time_window_comp_to_simple_hop <- function(time_window_comp, before_n_steps, after_n_steps) {
  force(time_window_comp)
  force(before_n_steps)
  force(after_n_steps)
  if (before_n_steps == Inf) {
    stop("TODO, probably in separate function?")
  }
  #
  slide_comp_fn <- time_window_comp$fn
  #
  function(ek_data, ek, ref_inds) {
    ukey_colnames <- c(names(ek), "time_value")
    results <- slider::hop(
      ek_data,
      ref_inds - before_n_steps,
      ref_inds + after_n_steps,
      function(x) {
        # ref_time_value <- vec_slice(x$time_value, nrow(x) - after_n_steps)
        # # FIXME actual name
        # comp_result <- new_tibble(list(slide_value = time_window_comp(x, ek, ref_time_value)))
        # cbind_unpacked_comps( # TODO probably too deeply nested, slow
        #   bind_cols( # XXX also, this seems like extra work...
        #     ek,
        #     vec_slice(x, nrow(x) - after_n_steps),
        #     .name_repair = "minimal"
        #   ),
        #   ukey_colnames,
        #   comp_result,
        #   # FIXME actual manually-named val
        #   rep(FALSE, length())
        # )
        #
        ref_time_value <- vec_slice(x$time_value, vec_size(x) - after_n_steps)
        comp_result <- slide_comp_fn(x, ek, ref_time_value)
        comp_result
      }
    )
    # stop("TODO validate size 1?")

    if (!all(list_sizes(results) == 1L)) {
      cli_abort("Slide computations must all output results of size 1.")
      # TODO better message
    }

    result <- list(vec_c(!!!results))
    names(result) <- time_window_comp$out_names
    result <- new_tibble(result, nrow = length(results))
    result <- comp_unpack(result, time_window_comp$out_names, time_window_comp$out_manually_named)

    return(result)

    # TODO naming stuff here. hopefully can re-use same thing for name checking
    # and unpacking at this level and for checking of named and unnamed
    # data-masking expressions. but might require changing... either name
    # checking needs to be in the above function and performed multiple times,
    # or might need to return metadata naming the guaranteed output cols, or a
    # list of hop functions though that seems like more overhead w/o rewriting
    # overhead...
    #
    # Pre-known names are .new_col_name and quosure names. Dynamic names are
    # from unnamed quosures yielding dfs or autonamed vecs, and
    # functions/formulas (returning either dfs or non-dfs; latter being assigned
    # "slide_value" when we realize that auto-naming is needed)
    # return(list(slide_result = vec_c(!!!results)))
  }
}

assert_no_hard_deprecated_epi_slide_args <- function(call) {
  provided_args <- rlang::call_args_names(call)
  if (any(provided_args %in% c("x", "f", "ref_time_values", "new_col_name", "all_rows"))) {
    cli_abort(
      "You are using one of the following old argument names: `x`, `f`, `ref_time_values`,
      `new_col_name`, or `all_rows`. Please use the new dot-prefixed names: `.x`, `.f`, `.ref_time_values`,
      `.new_col_name`, `.all_rows`.",
      call = call
    )
  }
  if ("as_list_col" %in% provided_args) {
    cli_abort(
      "epi_slide: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead.",
      call = call
    )
  }
  if ("names_sep" %in% provided_args) {
    cli_abort(
      "epi_slide: the argument `names_sep` is deprecated. If NULL, you can remove it, it is now default.
      If a string, please manually prefix your column names instead.",
      call = call
    )
  }
  if ("before" %in% provided_args || "after" %in% provided_args) {
    cli_abort(
      "epi_slide: `before` and `after` are deprecated for `epi_slide`. Use `.window_size` and `.align` instead.
      See the slide documentation for more details.",
      call = call
    )
  }
}
