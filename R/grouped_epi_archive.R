
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
  #
  # If we were allowing renaming, we'd need to be careful about which names (new
  # vs. old vs. both) to return here.
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
          Abort("`version` has a special interpretation and cannot be used by itself as a grouping variable")
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
          cat(strwrap(init="* ", prefix="  ", sprintf(
            "%s groups formed by factor levels that don't appear in the data",
            if (private$drop) "Drops" else "Does not drop"
          )))
          cat("\n")
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
      group_by_drop_default = function() {
        private$drop
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
          exclude_vars = eval_pure_select_names_from_dots(..., .data=private$ungrouped$DT)
          # (requiring a pure selection here is a little stricter than dplyr
          # implementations, but passing a renaming selection into `ungroup`
          # seems pretty weird.)
          result_vars = private$vars[! private$vars %in% exclude_vars]
          # `vars` might be length 0 if the user's tidyselection removed all
          # grouping vars. Unlike with tibble, opt here to keep the result as a
          # grouped_epi_archive, for output class consistency when `...` is
          # provided.
          grouped_epi_archive$new(private$ungrouped, result_vars, private$drop)
        }
      },
#' @description Filter to keep only older versions by mutating the underlying
#'   `epi_archive` using `$truncate_versions_after`. Returns the mutated
#'   `grouped_epi_archive` [invisibly][base::invisible].
#' @param x as in [`epix_truncate_versions_after`]
#' @param max_version as in [`epix_truncate_versions_after`]
      truncate_versions_after = function(max_version) {
        # The grouping is irrelevant for this method; if we were to split into
        # groups and recombine appropriately, we should get the same result as
        # just leveraging the ungrouped method, so just do the latter:
        private$ungrouped$truncate_versions_after(max_version)
        return (invisible(self))
      },
#' @description Slides a given function over variables in a `grouped_epi_archive`
#'   object. See the documentation for the wrapper function [`epix_slide()`] for
#'   details.
#' @importFrom data.table key address
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
          slide = function(f, ..., before, ref_time_values,
                           time_step, new_col_name = "slide_value",
                           as_list_col = FALSE, names_sep = "_",
                           all_versions = FALSE) {
            # Perform some deprecated argument checks without using `<param> =
            # deprecated()` in the function signature, because they are from
            # early development versions and much more likely to be clutter than
            # informative in the signature.
            if ("group_by" %in% nse_dots_names(...)) {
              Abort("
                The `group_by` argument to `slide` has been removed; please use
                the `group_by` S3 generic function or `$group_by` R6 method
                before the slide instead. (If you were instead trying to pass a
                `group_by` argument to `f` or create a column named `group_by`,
                this check is a false positive, but you will still need to use a
                different column name here and rename the resulting column after
                the slide.)
              ", class = "epiprocess__epix_slide_group_by_parameter_deprecated")
            }
            if ("all_rows" %in% nse_dots_names(...)) {
              Abort("
                The `all_rows` argument has been removed from `epix_slide` (but
                is still supported in `epi_slide`). Since `epix_slide` now
                allows any number of rows out of slide computations, it's
                unclear how `all_rows=TRUE` should fill in missing results.
              ", class = "epiprocess__epix_slide_all_rows_parameter_deprecated")
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
              Abort("`before` must be length-1, non-NA, non-negative.")
            }

            # If a custom time step is specified, then redefine units 
            
            if (!missing(time_step)) before <- time_step(before)
            
            # Symbolize column name
            new_col = sym(new_col_name)

            # Validate rest of parameters:
            if (!rlang::is_bool(as_list_col)) {
              Abort("`as_list_col` must be TRUE or FALSE.")
            }
            if (! (rlang::is_string(names_sep) || is.null(names_sep)) ) {
              Abort("`names_sep` must be a (single) string or NULL.")
            }
            if (!rlang::is_bool(all_versions)) {
              Abort("`all_versions` must be TRUE or FALSE.")
            }

            # Computation for one group, one time value
            comp_one_grp = function(.data_group, .group_key,
                                    f, ..., 
                                    ref_time_value,
                                    new_col) {
              # Carry out the specified computation
              comp_value = f(.data_group, .group_key, ...)

              if (all_versions) {
                # Extract data from archive so we can do length checks below. When
                # `all_versions = TRUE`, `.data_group` will always be an ungrouped
                # archive because of the preceding `as_of` step.
                .data_group = .data_group$DT
              }

              if (! (is.atomic(comp_value) || is.data.frame(comp_value))) {
                Abort("The slide computation must return an atomic vector or a data frame.")
              }

              # Label every result row with the `ref_time_value`:
              return(tibble::tibble(time_value = .env$ref_time_value,
                                    !!new_col := .env$comp_value))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              if (rlang::is_formula(f)) f = rlang::as_function(f)
              x = purrr::map_dfr(ref_time_values, function(ref_time_value) {
                # Ungrouped as-of data; `epi_df` if `all_versions` is `FALSE`,
                # `epi_archive` if `all_versions` is `TRUE`:
                as_of_raw = private$ungrouped$as_of(ref_time_value, min_time_value = ref_time_value - before, all_versions = all_versions)

                # Set:
                # * `as_of_df`, the data.frame/tibble/epi_df/etc. that we will
                #    `group_modify` as the `.data` argument. Might or might not
                #    include version column.
                # * `group_modify_fn`, the corresponding `.f` argument
                if (!all_versions) {
                  as_of_df = as_of_raw
                  group_modify_fn = comp_one_grp
                } else {
                  as_of_archive = as_of_raw
                  # We essentially want to `group_modify` the archive, but don't
                  # provide an implementation yet. Next best would be
                  # `group_modify` on its `$DT`, but that has different behavior
                  # based on whether or not `dtplyr` is loaded. Instead, go
                  # through a , trying to avoid copies.
                  if (address(as_of_archive$DT) == address(private$ungrouped$DT)) {
                    # `as_of` aliased its the full `$DT`; copy before mutating:
                    as_of_archive$DT <- copy(as_of_archive$DT)
                  }
                  dt_key = data.table::key(as_of_archive$DT)
                  as_of_df = as_of_archive$DT
                  data.table::setDF(as_of_df)

                  # Convert each subgroup chunk to an archive before running the calculation.
                  group_modify_fn = function(.data_group, .group_key,
                                    f, ...,
                                    ref_time_value,
                                    new_col) {
                    # .data_group is coming from as_of_df as a tibble, but we
                    # want to feed `comp_one_grp` an `epi_archive` backed by a
                    # DT; convert and wrap:
                    data.table::setattr(.data_group, "sorted", dt_key)
                    data.table::setDT(.data_group, key=dt_key)
                    .data_group_archive = as_of_archive$clone()
                    .data_group_archive$DT = .data_group
                    comp_one_grp(.data_group_archive, .group_key, f = f, ...,
                                 ref_time_value = ref_time_value,
                                 new_col = new_col
                    )
                  }
                }

                return(
                  dplyr::group_by(as_of_df, dplyr::across(tidyselect::all_of(private$vars)),
                                  .drop=private$drop) %>%
                    dplyr::group_modify(group_modify_fn,
                                        f = f, ...,
                                        ref_time_value = ref_time_value,
                                        new_col = new_col,
                                        .keep = TRUE)
                )
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
                # Ungrouped as-of data; `epi_df` if `all_versions` is `FALSE`,
                # `epi_archive` if `all_versions` is `TRUE`:
                as_of_raw = private$ungrouped$as_of(ref_time_value, min_time_value = ref_time_value - before, all_versions = all_versions)

                # Set:
                # * `as_of_df`, the data.frame/tibble/epi_df/etc. that we will
                #    `group_modify` as the `.data` argument. Might or might not
                #    include version column.
                # * `group_modify_fn`, the corresponding `.f` argument
                if (!all_versions) {
                  as_of_df = as_of_raw
                  group_modify_fn = comp_one_grp
                } else {
                  as_of_archive = as_of_raw
                  # We essentially want to `group_modify` the archive, but don't
                  # provide an implementation yet. Next best would be
                  # `group_modify` on its `$DT`, but that has different behavior
                  # based on whether or not `dtplyr` is loaded. Instead, go
                  # through a , trying to avoid copies.
                  if (address(as_of_archive$DT) == address(private$ungrouped$DT)) {
                    # `as_of` aliased its the full `$DT`; copy before mutating:
                    as_of_archive$DT <- copy(as_of_archive$DT)
                  }
                  dt_key = data.table::key(as_of_archive$DT)
                  as_of_df = as_of_archive$DT
                  data.table::setDF(as_of_df)

                  # Convert each subgroup chunk to an archive before running the calculation.
                  group_modify_fn = function(.data_group, .group_key,
                                    f, ...,
                                    ref_time_value,
                                    comp_effective_key_vars,
                                    new_col) {
                    # .data_group is coming from as_of_df as a tibble, but we
                    # want to feed `comp_one_grp` an `epi_archive` backed by a
                    # DT; convert and wrap:
                    data.table::setattr(.data_group, "sorted", dt_key)
                    data.table::setDT(.data_group, key=dt_key)
                    .data_group_archive = as_of_archive$clone()
                    .data_group_archive$DT = .data_group
                    comp_one_grp(.data_group_archive, .group_key, f = f, quo = quo,
                                 ref_time_value = ref_time_value,
                                 comp_effective_key_vars = comp_effective_key_vars,
                                 new_col = new_col
                    )
                  }
                }

                return(
                  dplyr::group_by(as_of_df, dplyr::across(tidyselect::all_of(private$vars)),
                                  .drop=private$drop) %>%
                    dplyr::group_modify(group_modify_fn,
                                        f = f, quo = quo,
                                        ref_time_value = ref_time_value,
                                        comp_effective_key_vars = comp_effective_key_vars,
                                        new_col = new_col,
                                        .keep = TRUE)
                )
              })
            }
            
            # Unnest if we need to
            if (!as_list_col) {
              x = tidyr::unnest(x, !!new_col, names_sep = names_sep)
            }

            if (is_epi_df(x)) {
              # The analogue of `epi_df`'s `as_of` metadata for an archive is
              # `<archive>$versions_end`, at least in the current absence of
              # separate fields/columns denoting the "archive version" with a
              # different resolution, or from the perspective of a different
              # stage of a data pipeline. The `as_of` that is automatically
              # derived won't always match; override:

              attr(x, "metadata")[["as_of"]] <- private$ungrouped$versions_end
            }

            return(x)
          }
    )
  )

# At time of writing, roxygen parses content in collation order, impacting the
# presentation of .Rd files that document multiple functions (see
# https://github.com/r-lib/roxygen2/pull/324). Use @include tags (determining
# `Collate:`) and ordering of functions within each file in order to get the
# desired ordering.

#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr group_by
#' @export
group_by.grouped_epi_archive = function(.data, ..., .add=FALSE, .drop=dplyr::group_by_drop_default(.data)) {
  .data$group_by(..., .add=.add, .drop=.drop)
}

#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr groups
#' @export
groups.grouped_epi_archive = function(x) {
  x$groups()
}

#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_epi_archive = function(x, ...) {
  x$ungroup(...)
}

#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @export
is_grouped_epi_archive = function(x) {
  inherits(x, "grouped_epi_archive")
}

#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @export
group_by_drop_default.grouped_epi_archive = function(.tbl) {
  .tbl$group_by_drop_default()
}

#' @export
epix_truncate_versions_after.grouped_epi_archive = function(x, max_version) {
  return ((x$clone()$truncate_versions_after(max_version)))
  # ^ second set of parens drops invisibility
}
