
#' Get var names from select-only `tidy_select`ing `...` in `.data`
#'
#' Convenience function for performing a `tidy_select` on dots according to its
#' docs, and taking the names (rather than the integer indices).
#'
#' @param ... tidyselect-syntax selection description
#' @param .data named vector / data frame; context for the description / the
#'   object to which the selections apply
#' @return character vector containing names of entries/columns of
#'   `names(.data)` denoting the selection
#'
#' @noRd
eval_pure_select_names_from_dots = function(..., .data) {
  # `?tidyselect::eval_select` tells us to use this form when we take in dots.
  # It seems a bit peculiar, since the expr doesn't pack with it a way to get at
  # the environment for the dots, but it looks like `eval_select` will assume
  # the caller env (our `environment()`) when given an expr, and thus have
  # access to the dots.
  names(tidyselect::eval_select(rlang::expr(c(...)), .data, allow_rename=FALSE))
}

#' Get names of dots without forcing the dots
#'
#' For use in functions that use nonstandard evaluation (NSE) on the dots; we
#' can't use the pattern `names(list(...))` in this case because it will attempt
#' to force/(standard-)evaluate the dots, and we want to avoid attempted forcing of the
#' dots if we're using NSE.
#'
#' @noRd
nse_dots_names = function(...) {
  names(rlang::call_match())
}
nse_dots_names2 = function(...) {
  rlang::names2(rlang::call_match())
}

#' @rdname group_by.epi_archive
#'
#' @export
group_by_drop_default.grouped_epi_archive = function(.tbl) {
  .tbl$group_by_drop_default()
}

#' @importFrom dplyr group_by_drop_default
#' @noRd
grouped_epi_archive =
  R6::R6Class(
    classname = "grouped_epi_archive",
    # (We don't R6-inherit `epi_archive` or S3-multiclass with "epi_archive";
    # any "inheritance" of functionality must be done via wrapper functions that
    # are checked/tested for sensible operation.)
    private = list(
      ungrouped = NULL,
      vars = NULL,
      drop = NULL
    ),
    public = list(
      initialize = function(ungrouped, vars, drop) {
        if (inherits(ungrouped, "grouped_epi_archive")) {
          Abort("`ungrouped` must not already be grouped (neither automatic regrouping nor nested grouping is supported).  Either use `group_by` with `.add=TRUE`, or `ungroup` first.",
                class="epiprocess__grouped_epi_archive__ungrouped_arg_is_already_grouped",
                epiprocess__ungrouped_class = class(ungrouped),
                epiprocess__ungrouped_groups = groups(ungrouped))
        }
        if (!inherits(ungrouped, "epi_archive")) {
          Abort("`ungrouped` must be an epi_archive",
                class="epiprocess__grouped_epi_archive__ungrouped_arg_is_not_epi_archive",
                epiprocess__ungrouped_class = class(ungrouped))
        }
        if (!is.character(vars)) {
          Abort("`vars` must be a character vector (any tidyselection should have already occurred in a helper method).",
                class="epiprocess__grouped_epi_archive__vars_is_not_chr",
                epiprocess__vars_class = class(vars),
                epiprocess__vars_type = typeof(vars))
        }
        if (!all(vars %in% names(ungrouped$DT))) {
          Abort("`vars` must be selected from the names of columns of `ungrouped$DT`",
                class="epiprocess__grouped_epi_archive__vars_contains_invalid_entries",
                epiprocess__vars = vars,
                epiprocess__DT_names = names(ungrouped$DT))
        }
        if ("version" %in% vars) {
          Abort("`version` has a special interpretation and cannot be used as a grouping variable")
        }
        if (!rlang::is_bool(drop)) {
          Abort("`drop` must be a Boolean",
                class="epiprocess__grouped_epi_archive__drop_is_not_bool",
                epiprocess__drop = drop)
        }
        # -----
        private$ungrouped <- ungrouped
        private$vars <- vars
        private$drop <- drop
      },
      print = function(class = TRUE, methods = TRUE) {
        if (class) cat("A `grouped_epi_archive` object:\n")
        writeLines(wrap_varnames(private$vars, initial="* Groups: "))
        # If none of the grouping vars is a factor, then $drop doesn't seem
        # relevant, so try to be less verbose and don't message about it.
        #
        # Below map-then-extract may look weird, but the more natural
        # extract-then-map appears to trigger copies of the extracted columns
        # since we are working with a `data.table` (unless we go through
        # `as.list`, but its current column-aliasing behavior is probably not
        # something to rely too much on), while map functions currently appear
        # to avoid column copies.
        if (any(purrr::map_lgl(private$ungrouped$DT, is.factor)[private$vars])) {
          cat(sprintf("* %s groups formed by factor levels that don't appear in the data",
                      if (private$drop) "Drops" else "Does not drop"))
        }
        cat("It wraps an ungrouped `epi_archive`, with metadata:\n")
        private$ungrouped$print(class = FALSE, methods = FALSE)
        if (methods) {
          cat("----------\n")
          cat("Public `grouped_epi_archive` R6 methods:\n")
          grouped_method_names = names(grouped_epi_archive$public_methods)
          ungrouped_method_names = names(epi_archive$public_methods)
          writeLines(wrap_varnames(initial = "• Specialized `epi_archive` methods: ",
                                   intersect(grouped_method_names, ungrouped_method_names)))
          writeLines(wrap_varnames(initial = "• Exclusive to `grouped_epi_archive`: ",
                                   setdiff(grouped_method_names, ungrouped_method_names)))
          writeLines(wrap_varnames(initial = "• `ungroup` to use: ",
                                   setdiff(ungrouped_method_names, grouped_method_names)))
        }
        # Return self invisibly for convenience in `$`-"pipe":
        invisible(self)
      },
      group_by_drop_default = function() {
        private$drop
      },
      group_by = function(..., .add = FALSE, .drop = dplyr::group_by_drop_default(self)) {
        if (!rlang::is_bool(.add)) {
          Abort("`.add` must be a Boolean")
        }
        if (!.add) {
          Abort('`group_by` on a `grouped_epi_archive` with `.add=FALSE` is forbidden
                 (neither automatic regrouping nor nested grouping is supported).
                 If you want to "regroup", replacing the existing grouping vars, `ungroup` first and then `group_by`.
                 If you want to add to the existing grouping vars, call `group_by` specifying `.add=TRUE`.
                ',
                class = "epiprocess__grouped_epi_archive_group_by_with_add_FALSE")
        } else {
          # `group_by` `...` computations are performed on ungrouped data (see
          # `?dplyr::group_by`)
          detailed_mutate = epix_detailed_restricted_mutate(private$ungrouped, ...)
          out_ungrouped = detailed_mutate[["archive"]]
          vars_from_dots = detailed_mutate[["request_names"]]
          vars = union(private$vars, vars_from_dots)
          grouped_epi_archive$new(private$ungrouped, vars, .drop)
        }
      },
      groups = function() {
        rlang::syms(private$vars)
      },
      ungroup = function(...) {
        if (rlang::dots_n(...) == 0L) {
          # No dots = special behavior: remove all grouping vars and convert to
          # an ungrouped class, as with `grouped_df`s.
          private$ungrouped
        } else {
          exclude_vars = eval_select_names_from_dots(..., .data=private$ungrouped$DT)
          vars = setdiff(private$vars, exclude_vars)
          # `vars` might be length 0 if the user's tidyselection removed all
          # grouping vars. Unlike with tibble, opt here to keep the result as a
          # grouped_epi_archive, for output class consistency when `...` is
          # provided.
          grouped_epi_archive$new(private$ungrouped, vars, private$drop)
        }
      },
#' @description Slides a given function over variables in a `grouped_epi_archive`
#'   object. See the documentation for the wrapper function [`epix_slide()`] for
#'   details.
#' @importFrom data.table key
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
          slide = function(f, ..., before, ref_time_values,
                           time_step, new_col_name = "slide_value",
                           as_list_col = FALSE, names_sep = "_",
                           all_rows = FALSE) {
            if ("group_by" %in% nse_dots_names(...)) {
              Abort("
                The `group_by` argument to `slide` has been removed; please use
                the `group_by` S3 generic function or `$group_by` R6 method
                before the slide instead. (If you were instead trying to pass a
                `group_by` argument to `f` or create a column named `group_by`,
                this check is a false positive, but you will still need to use a
                different column name here and rename the resulting column after
                the slide.)
              ")
            }
            
            if (missing(ref_time_values)) {
              versions_with_updates = c(private$ungrouped$DT$version, private$ungrouped$versions_end)
              ref_time_values = tidyr::full_seq(versions_with_updates, guess_period(versions_with_updates))
            } else if (length(ref_time_values) == 0L) {
              Abort("`ref_time_values` must have at least one element.")
            } else if (any(is.na(ref_time_values))) {
              Abort("`ref_time_values` must not include `NA`.")
            } else if (anyDuplicated(ref_time_values) != 0L) {
              Abort("`ref_time_values` must not contain any duplicates; use `unique` if appropriate.")
            } else if (any(ref_time_values > private$ungrouped$versions_end)) {
              Abort("All `ref_time_values` must be `<=` the `versions_end`.")
            } else {
              # Sort, for consistency with `epi_slide`, although the current
              # implementation doesn't take advantage of it.
              ref_time_values = sort(ref_time_values)
            }
            
            # Validate and pre-process `before`:
            if (missing(before)) {
              Abort("`before` is required (and must be passed by name);
                     if you did not want to apply a sliding window but rather
                     to map `as_of` and `f` across various `ref_time_values`,
                     pass a large `before` value (e.g., if time steps are days,
                     `before=365000`).")
            }
            before <- vctrs::vec_cast(before, integer())
            if (length(before) != 1L || is.na(before) || before < 0L) {
              Abort("`before` must be length-1, non-NA, non-negative")
            }

            # If a custom time step is specified, then redefine units 
            
            if (!missing(time_step)) before <- time_step(before)
            
            # Symbolize column name
            new_col = sym(new_col_name)

            # Each computation is expected to output a data frame with either
            # one element/row total or one element/row per encountered
            # nongrouping, nontime, nonversion key value. These nongrouping,
            # nontime, nonversion key columns can be seen as the "effective" key
            # of the computation; the computation might return an object that
            # reports a different key or no key, but the "effective" key should
            # still be a valid unique key for the data, and is something that we
            # could use even with `.keep = FALSE`.
            comp_effective_key_vars =
              setdiff(key(private$ungrouped$DT),
                      c(private$vars, "time_value", "version"))
            
            # Computation for one group, one time value
            comp_one_grp = function(.data_group, .group_key,
                                    f, ..., 
                                    ref_time_value,
                                    comp_effective_key_vars,
                                    new_col) {
              # Carry out the specified computation 
              comp_value = f(.data_group, .group_key, ...)

              # Calculate the number of output elements/rows we expect the
              # computation to output: one per distinct "effective computation
              # key variable" value encountered in the input. Note: this mirrors
              # how `epi_slide` does things if we're using unique keys, but can
              # diverge if using nonunique keys. The `epi_slide` approach of
              # counting occurrences of the `ref_time_value` in the `time_value`
              # column, which helps lines up the computation results with
              # corresponding rows of the input data, wouldn't quite apply here:
              # we'd want to line up with rows (from the same group) with
              # `version` matching the `ref_time_value`, but would still need to
              # summarize these rows somehow and drop the `time_value` input
              # column, but this summarization requires something like a
              # to-be-unique output key to determine a sensible number of rows
              # to output (and the contents of those rows).
              count =
                if (length(comp_effective_key_vars) != 0L) {
                  sum(!duplicated(.data_group[, comp_effective_key_vars]))
                } else {
                  # Same idea as above, but accounting for `duplicated` not
                  # working as we want on 0 columns. (Should be the same as if
                  # we were counting distinct values of a column defined as
                  # `rep(val, target_n_rows)`.)
                  if (nrow(.data_group) == 0L) {
                    0L
                  } else {
                    1L
                  }
                }

              # If we get back an atomic vector
              if (is.atomic(comp_value)) {
                if (length(comp_value) == 1) {
                  comp_value = rep(comp_value, count)
                }
                # If not a singleton, should be the right length, else abort
                else if (length(comp_value) != count) {
                  Abort('If the slide computation returns an atomic vector, then it must have either (a) a single element, or (b) one element per distinct combination of key variables, excluding the `time_value`, `version`, and grouping variables, that is present in the first argument to the computation.')
                }
              }

              # If we get back a data frame
              else if (is.data.frame(comp_value)) {
                if (nrow(comp_value) == 1) {
                  comp_value = rep(list(comp_value), count)
                }
                # If not a single row, should be the right length, else abort
                else if (nrow(comp_value) != count) {
                  Abort("If the slide computation returns a data frame, then it must have a single row, or else one row per appearance of the reference time value in the local window.")
                }
                # Make into a list
                else {
                  comp_value = split(comp_value, seq_len(nrow(comp_value)))
                }
              }

              # If neither an atomic vector data frame, then abort
              else {
                Abort("The slide computation must return an atomic vector or a data frame.")
              }
 
              # Label every result row with the `ref_time_value`:
              return(tibble::tibble(time_value = rep(.env$ref_time_value, count),
                                    !!new_col := .env$comp_value))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              if (rlang::is_formula(f)) f = rlang::as_function(f)
              x = purrr::map_dfr(ref_time_values, function(ref_time_value) {
                private$ungrouped$as_of(ref_time_value, min_time_value = ref_time_value - before) %>%
                  dplyr::group_by(dplyr::across(tidyselect::all_of(private$vars)),
                                  .drop=private$drop) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, ...,
                                      ref_time_value = ref_time_value,
                                      comp_effective_key_vars = comp_effective_key_vars,
                                      new_col = new_col,
                                      .keep = TRUE)
              })
            }

            # Else interpret ... as an expression for tidy evaluation
            else {
              quos = enquos(...)
              if (length(quos) == 0) {
                Abort("If `f` is missing then a computation must be specified via `...`.")
              }
              if (length(quos) > 1) {
                Abort("If `f` is missing then only a single computation can be specified via `...`.")
              }
              
              quo = quos[[1]]
              f = function(x, quo, ...) rlang::eval_tidy(quo, x)
              new_col = sym(names(rlang::quos_auto_name(quos)))

              x = purrr::map_dfr(ref_time_values, function(ref_time_value) {
                private$ungrouped$as_of(ref_time_value, min_time_value = ref_time_value - before) %>%
                  dplyr::group_by(dplyr::across(tidyselect::all_of(private$vars)),
                                  .drop=private$drop) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, quo = quo,
                                      ref_time_value = ref_time_value,
                                      comp_effective_key_vars = comp_effective_key_vars,
                                      new_col = new_col,
                                      .keep = TRUE)
              })
            }
            
            # Unnest if we need to
            if (!as_list_col) {
              x = tidyr::unnest(x, !!new_col, names_sep = names_sep)
            }
            
            # Join to get all rows, if we need to, then return
            if (all_rows) {
              cols = c(private$vars, "time_value")
              y = unique(private$ungrouped$DT[, ..cols])
              x = dplyr::left_join(y, x, by = cols)
            }
            return(x)
          }
    )
  )

#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr group_by
#' @export
group_by.grouped_epi_archive = function(.data, ..., .add=FALSE, .drop=dplyr::group_by_drop_default(.data)) {
  .data$group_by(..., .add=.add, .drop=.drop)
}

#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr groups
#' @export
groups.grouped_epi_archive = function(x) {
  x$groups()
}

#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_epi_archive = function(x, ...) {
  x$ungroup(x, ...)
}

#' @export
is_grouped_epi_archive = function(x) {
  inherits(x, "grouped_epi_archive")
}
