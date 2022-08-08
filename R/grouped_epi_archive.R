
eval_select_names_from_dots = function(..., .data) {
  # `?tidyselect::eval_select` tells us to use this form when we take
  # in dots. It seems a bit peculiar, since the expr doesn't pack with
  # it a way to get at the dots, but it looks like `eval_select` will
  # assume the caller env (our `environment()`) when given an expr,
  # and thus have access to the dots.
  names(tidyselect::eval_select(rlang::expr(c(...)), .data))
}

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
      print = function(header=TRUE) {
        if (header) cat("A `grouped_epi_archive` object:\n")
        cat_varnames(private$vars, initial="* Groups: ")
        # If none of the grouping vars is a factor, then $drop doesn't seem
        # relevant, so try to be less verbose and don't message about it.
        #
        # Below map-then-extract may look weird, but the more natural
        # extract-then-map appears to trigger copies the extracted columns since
        # we are working with a data.table (unless we go through `as.list`, but
        # its current aliasing behavior is probably not something to rely on),
        # while map functions currently appear to avoid column copies.
        if (any(purrr::map_lgl(private$ungrouped$DT, is.factor)[private$vars])) {
          cat(sprintf("* %s groups formed by factor levels that don't appear in the data",
                      if (private$drop) "Drops" else "Does not drop"))
        }
        cat("It wraps an ungrouped `epi_archive`, with metadata:\n")
        private$ungrouped$print(header=FALSE)
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
          vars_from_dots = eval_select_names_from_dots(..., .data=private$ungrouped$DT)
          vars = union(self$vars, vars_from_dots)
          grouped_epi_archive$new(self$ungrouped, vars, .drop)
        }
      },
      ungroup = function(...) {
        if (missing(..1)) {
          private$ungrouped
        } else {
          exclude_vars = eval_select_names_from_dots(..., .data=private$ungrouped$DT)
          vars = setdiff(private$vars, exclude_vars)
          # `vars` might be length 0 if the user's tidyselection removed all
          # grouping vars. Opt here to keep the result as a grouped_epi_archive,
          # for output class consistency when `...` is provided.
          grouped_epi_archive$new(private$ungrouped, vars, private$drop)
        }
      },
#' @description Slides a given function over variables in a `grouped_epi_archive`
#'   object. See the documentation for the wrapper function [`epix_slide()`] for
#'   details.
#' @importFrom data.table key
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
          slide = function(f, ..., n, ref_time_values,
                           time_step, new_col_name = "slide_value",
                           groups = NULL,
                           as_list_col = FALSE, names_sep = "_",
                           all_rows = FALSE) {
            # If missing, then set ref time values to be everything; else make
            # sure we intersect with observed time values 
            if (missing(ref_time_values)) {
              ref_time_values = unique(private$ungrouped$DT$time_value)
            }
            else {
              ref_time_values = ref_time_values[ref_time_values %in%
                                                unique(private$ungrouped$DT$time_value)]
            }
            
            # If a custom time step is specified, then redefine units 
            before_num = n-1
            if (!missing(time_step)) before_num = time_step(n-1)
            
            # Symbolize column name
            new_col = sym(new_col_name)

            # Key variable names, apart from time value and version
            key_vars = setdiff(key(private$ungrouped$DT), c("time_value", "version"))
            
            # Computation for one group, one time value
            comp_one_grp = function(.data_group,
                                    f, ..., 
                                    time_value,
                                    key_vars,
                                    new_col) {
              # Carry out the specified computation 
              comp_value = f(.data_group, ...)

              # Count the number of appearances of the reference time value.
              # Note: ideally, we want to directly count occurrences of the ref
              # time value but due to latency, this will often not appear in the
              # data group. So we count the number of unique key values, outside 
              # of the time value column
              count = sum(!duplicated(.data_group[, key_vars]))

              # If we get back an atomic vector
              if (is.atomic(comp_value)) {
                if (length(comp_value) == 1) {
                  comp_value = rep(comp_value, count)
                }
                # If not a singleton, should be the right length, else abort
                else if (length(comp_value) != count) {
                  Abort("If the slide computation returns an atomic vector, then it must have a single element, or else one element per appearance of the reference time value in the local window.")
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
                  comp_value = split(comp_value, 1:nrow(comp_value))
                }
              }

              # If neither an atomic vector data frame, then abort
              else {
                Abort("The slide computation must return an atomic vector or a data frame.")
              }
 
              # Note that we've already recycled comp value to make size stable,
              # so tibble() will just recycle time value appropriately
              return(tibble::tibble(time_value = time_value, 
                                    !!new_col := comp_value))
            }
            
            # If f is not missing, then just go ahead, slide by group
            if (!missing(f)) {
              if (rlang::is_formula(f)) f = rlang::as_function(f)
              x = purrr::map_dfr(ref_time_values, function(t) {
                private$ungrouped$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>%
                  dplyr::group_by(dplyr::across(tidyselect::all_of(private$vars)),
                                  .drop=private$drop) %>%
                  dplyr::summarize(comp_one_grp(dplyr::cur_data_all(),
                                                f = f, ...,
                                                time_value = t,
                                                key_vars = key_vars,
                                                new_col = new_col),
                                   .groups = groups)
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

              x = purrr::map_dfr(ref_time_values, function(t) {
                private$ungrouped$as_of(t, min_time_value = t - before_num) %>%
                  tibble::as_tibble() %>% 
                  dplyr::group_by(dplyr::across(tidyselect::all_of(private$vars))) %>%
                  dplyr::group_modify(comp_one_grp,
                                      f = f, quo = quo,
                                      time_value = t,
                                      key_vars = key_vars,
                                      new_col = new_col,
                                      .keep = TRUE) %>%
                  dplyr::ungroup()
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

#' @export
group_by.grouped_epi_archive = function(.data, ..., .add=FALSE, .drop=dplyr::group_by_drop_default(.data)) {
  .data$group_by(..., .add=.add, .drop=.drop)
}

#' @export
is_grouped_epi_archive = function(x) {
  inherits(x, "grouped_epi_archive")
}
