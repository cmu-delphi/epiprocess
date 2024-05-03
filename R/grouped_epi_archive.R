# At time of writing, roxygen parses content in collation order, impacting the
# presentation of .Rd files that document multiple functions (see
# https://github.com/r-lib/roxygen2/pull/324). We use @include tags (determining
# `Collate:`) below to get the desired ordering.


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
eval_pure_select_names_from_dots <- function(..., .data) {
  # `?tidyselect::eval_select` tells us to use this form when we take in dots.
  # It seems a bit peculiar, since the expr doesn't pack with it a way to get at
  # the environment for the dots, but it looks like `eval_select` will assume
  # the caller env (our `environment()`) when given an expr, and thus have
  # access to the dots.
  #
  # If we were allowing renaming, we'd need to be careful about which names (new
  # vs. old vs. both) to return here.
  names(tidyselect::eval_select(rlang::expr(c(...)), .data, allow_rename = FALSE))
}


#' Get names of dots without forcing the dots
#'
#' For use in functions that use nonstandard evaluation (NSE) on the dots; we
#' can't use the pattern `names(list(...))` in this case because it will attempt
#' to force/(standard-)evaluate the dots, and we want to avoid attempted forcing of the
#' dots if we're using NSE.
#'
#' @noRd
nse_dots_names <- function(...) {
  names(rlang::call_match())
}
nse_dots_names2 <- function(...) {
  rlang::names2(rlang::call_match())
}


#' @importFrom dplyr group_by_drop_default
#' @noRd
new_grouped_epi_archive <- function(x, vars, drop) {
  if (inherits(x, "grouped_epi_archive")) {
    cli_abort(
      "`ungrouped` must not already be grouped (neither automatic regrouping
       nor nested grouping is supported).  Either use `group_by` with `.add=TRUE`,
        or `ungroup` first.",
      class = "epiprocess__grouped_epi_archive__ungrouped_arg_is_already_grouped",
      epiprocess__ungrouped_class = class(x),
      epiprocess__ungrouped_groups = groups(x)
    )
  }
  assert_class(x, "epi_archive")
  assert_character(vars)
  if (!test_subset(vars, names(x$DT))) {
    cli_abort(
      "All grouping variables `vars` must be present in the data.",
    )
  }
  if ("version" %in% vars) {
    cli_abort("`version` has a special interpretation and cannot be used by itself as a grouping variable")
  }
  assert_logical(drop, len = 1)

  # -----
  private <- list()
  private$ungrouped <- x
  private$vars <- vars
  private$drop <- drop

  return(structure(
    list(
      private = private
    ),
    class = "grouped_epi_archive"
  ))
}


#' @export
print.grouped_epi_archive <- function(x, ..., class = TRUE) {
  if (rlang::dots_n(...) > 0) {
    cli_abort(c(
      "Error in print.grouped_epi_archive()",
      "i" = "Too many arguments passed to `print.grouped_epi_archive()`."
    ))
  }

  if (class) cat("A `grouped_epi_archive` object:\n")
  writeLines(wrap_varnames(x$private$vars, initial = "* Groups: "))
  # If none of the grouping vars is a factor, then $drop doesn't seem
  # relevant, so try to be less verbose and don't message about it.
  #
  # Below map-then-extract may look weird, but the more natural
  # extract-then-map appears to trigger copies of the extracted columns
  # since we are working with a `data.table` (unless we go through
  # `as.list`, but its current column-aliasing behavior is probably not
  # something to rely too much on), while map functions currently appear
  # to avoid column copies.
  if (any(purrr::map_lgl(x$private$ungrouped$DT, is.factor)[x$private$vars])) {
    cat(strwrap(initial = "* ", prefix = "  ", sprintf(
      "%s groups formed by factor levels that don't appear in the data",
      if (x$private$drop) "Drops" else "Does not drop"
    )))
    cat("\n")
  }
  cat("It wraps an ungrouped `epi_archive`, with metadata:\n")
  print(x$private$ungrouped, class = FALSE)
  # Return self invisibly for convenience in `$`-"pipe":
  invisible(x)
}


#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr group_by
#' @export
group_by.grouped_epi_archive <- function(
    .data,
    ...,
    .add = FALSE,
    .drop = dplyr::group_by_drop_default(.data)) {
  assert_logical(.add, len = 1)
  if (!.add) {
    cli_abort('`group_by` on a `grouped_epi_archive` with `.add=FALSE` is forbidden
              (neither automatic regrouping nor nested grouping is supported).
              If you want to "regroup", replacing the existing grouping vars, `ungroup` first and then `group_by`.
              If you want to add to the existing grouping vars, call `group_by` specifying `.add=TRUE`.
            ',
      class = "epiprocess__grouped_epi_archive_group_by_with_add_FALSE"
    )
  } else {
    # `group_by` `...` computations are performed on ungrouped data (see
    # `?dplyr::group_by`)
    detailed_mutate <- epix_detailed_restricted_mutate(.data$private$ungrouped, ...)
    out_ungrouped <- detailed_mutate[["archive"]]
    vars_from_dots <- detailed_mutate[["request_names"]]
    vars <- union(.data$private$vars, vars_from_dots)
    new_grouped_epi_archive(out_ungrouped, vars, .drop)
  }
}


#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @param .tbl A `grouped_epi_archive` object.
#'
#' @export
group_by_drop_default.grouped_epi_archive <- function(.tbl) {
  .tbl$private$drop
}


#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr groups
#' @export
groups.grouped_epi_archive <- function(x) {
  rlang::syms(x$private$vars)
}


#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_epi_archive <- function(x, ...) {
  if (rlang::dots_n(...) == 0L) {
    # No dots = special behavior: remove all grouping vars and convert to
    # an ungrouped class, as with `grouped_df`s.
    x$private$ungrouped
  } else {
    exclude_vars <- eval_pure_select_names_from_dots(..., .data = x$private$ungrouped$DT)
    # (requiring a pure selection here is a little stricter than dplyr
    # implementations, but passing a renaming selection into `ungroup`
    # seems pretty weird.)
    result_vars <- x$private$vars[!x$private$vars %in% exclude_vars]
    # `vars` might be length 0 if the user's tidyselection removed all
    # grouping vars. Unlike with tibble, opt here to keep the result as a
    # grouped_epi_archive, for output class consistency when `...` is
    # provided.
    new_grouped_epi_archive(x$private$ungrouped, result_vars, x$private$drop)
  }
}


#' @rdname epix_slide
#'
#' @importFrom data.table key address rbindlist setDF copy
#' @importFrom tibble as_tibble new_tibble validate_tibble
#' @importFrom dplyr group_by groups
#' @importFrom rlang !! !!! enquo quo_is_missing enquos is_quosure sym syms
#'  env missing_arg
#' @export
epix_slide.grouped_epi_archive <- function(x, f, ..., before, ref_time_values,
                                           time_step, new_col_name = "slide_value",
                                           as_list_col = FALSE, names_sep = "_",
                                           all_versions = FALSE) {
  # Perform some deprecated argument checks without using `<param> =
  # deprecated()` in the function signature, because they are from
  # early development versions and much more likely to be clutter than
  # informative in the signature.
  if ("group_by" %in% nse_dots_names(...)) {
    cli_abort("
          The `group_by` argument to `slide` has been removed; please use
          the `group_by()` S3 generic function
          before the slide instead. (If you were instead trying to pass a
          `group_by` argument to `f` or create a column named `group_by`,
          this check is a false positive, but you will still need to use a
          different column name here and rename the resulting column after
          the slide.)
        ", class = "epiprocess__epix_slide_group_by_parameter_deprecated")
  }
  if ("all_rows" %in% nse_dots_names(...)) {
    cli_abort("
          The `all_rows` argument has been removed from `epix_slide` (but
          is still supported in `epi_slide`). Add rows for excluded
          results with a manual join instead.
        ", class = "epiprocess__epix_slide_all_rows_parameter_deprecated")
  }

  if (missing(ref_time_values)) {
    ref_time_values <- epix_slide_ref_time_values_default(x$private$ungrouped)
  } else {
    assert_numeric(ref_time_values, min.len = 1L, null.ok = FALSE, any.missing = FALSE)
    if (any(ref_time_values > x$private$ungrouped$versions_end)) {
      cli_abort("Some `ref_time_values` are greater than the latest version in the archive.")
    }
    if (anyDuplicated(ref_time_values) != 0L) {
      cli_abort("Some `ref_time_values` are duplicated.")
    }
    # Sort, for consistency with `epi_slide`, although the current
    # implementation doesn't take advantage of it.
    ref_time_values <- sort(ref_time_values)
  }

  # Validate and pre-process `before`:
  if (missing(before)) {
    cli_abort("`before` is required (and must be passed by name);
                if you did not want to apply a sliding window but rather
                to map `epix_as_of` and `f` across various `ref_time_values`,
                pass a large `before` value (e.g., if time steps are days,
                `before=365000`).")
  }
  before <- vctrs::vec_cast(before, integer())
  assert_int(before, lower = 0L, null.ok = FALSE, na.ok = FALSE)

  # If a custom time step is specified, then redefine units

  if (!missing(time_step)) before <- time_step(before)

  # Symbolize column name
  new_col <- sym(new_col_name)

  # Validate rest of parameters:
  assert_logical(as_list_col, len = 1L)
  assert_logical(all_versions, len = 1L)
  assert_character(names_sep, len = 1L, null.ok = TRUE)

  # Computation for one group, one time value
  comp_one_grp <- function(.data_group, .group_key,
                           f, ...,
                           ref_time_value,
                           new_col) {
    # Carry out the specified computation
    comp_value <- f(.data_group, .group_key, ref_time_value, ...)

    if (all_versions) {
      # Extract data from archive so we can do length checks below. When
      # `all_versions = TRUE`, `.data_group` will always be an ungrouped
      # archive because of the preceding `epix_as_of` step.
      .data_group <- .data_group$DT
    }

    assert(
      check_atomic(comp_value, any.missing = TRUE),
      check_data_frame(comp_value),
      combine = "or",
      .var.name = vname(comp_value)
    )

    # Label every result row with the `ref_time_value`
    res <- list(time_value = ref_time_value)

    # Wrap the computation output in a list and unchop/unnest later if
    # `as_list_col = FALSE`. This approach means that we will get a
    # list-class col rather than a data.frame-class col when
    # `as_list_col = TRUE` and the computations outputs are data
    # frames.
    res[[new_col]] <- list(comp_value)

    # Convert the list to a tibble all at once for speed.
    return(validate_tibble(new_tibble(res)))
  }

  # If `f` is missing, interpret ... as an expression for tidy evaluation
  if (missing(f)) {
    quos <- enquos(...)
    if (length(quos) == 0) {
      cli_abort("If `f` is missing then a computation must be specified via `...`.")
    }
    if (length(quos) > 1) {
      cli_abort("If `f` is missing then only a single computation can be specified via `...`.")
    }

    f <- quos[[1]]
    new_col <- sym(names(rlang::quos_auto_name(quos)))
    ... <- missing_arg() # nolint: object_usage_linter. magic value that passes zero args as dots in calls below
  }

  f <- as_slide_computation(f, ...)
  out <- lapply(ref_time_values, function(ref_time_value) {
    # Ungrouped as-of data; `epi_df` if `all_versions` is `FALSE`,
    # `epi_archive` if `all_versions` is `TRUE`:
    as_of_raw <- x$private$ungrouped %>% epix_as_of(
      ref_time_value,
      min_time_value = ref_time_value - before,
      all_versions = all_versions
    )

    # Set:
    # * `as_of_df`, the data.frame/tibble/epi_df/etc. that we will
    #    `group_modify` as the `.data` argument. Might or might not
    #    include version column.
    # * `group_modify_fn`, the corresponding `.f` argument
    if (!all_versions) {
      as_of_df <- as_of_raw
      group_modify_fn <- comp_one_grp
    } else {
      as_of_archive <- as_of_raw
      # We essentially want to `group_modify` the archive, but
      # haven't implemented this method yet. Next best would be
      # `group_modify` on its `$DT`, but that has different
      # behavior based on whether or not `dtplyr` is loaded.
      # Instead, go through an ordinary data frame, trying to avoid
      # copies.
      if (address(as_of_archive$DT) == address(x$private$ungrouped$DT)) {
        # `as_of` aliased its the full `$DT`; copy before mutating:
        #
        # Note: this step is probably unneeded; we're fine with
        # aliasing of the DT or its columns: vanilla operations aren't
        # going to mutate them in-place if they are aliases, and we're
        # not performing mutation.
        as_of_archive$DT <- data.table::copy(as_of_archive$DT)
      }
      dt_key <- data.table::key(as_of_archive$DT)
      as_of_df <- as_of_archive$DT
      data.table::setDF(as_of_df)

      # Convert each subgroup chunk to an archive before running the calculation.
      group_modify_fn <- function(.data_group, .group_key,
                                  f, ...,
                                  ref_time_value,
                                  new_col) {
        # .data_group is coming from as_of_df as a tibble, but we
        # want to feed `comp_one_grp` an `epi_archive` backed by a
        # DT; convert and wrap:
        data.table::setattr(.data_group, "sorted", dt_key)
        data.table::setDT(.data_group, key = dt_key)
        .data_group_archive <- as_of_archive
        .data_group_archive$DT <- .data_group
        comp_one_grp(.data_group_archive, .group_key,
          f = f, ...,
          ref_time_value = ref_time_value,
          new_col = new_col
        )
      }
    }

    return(
      dplyr::group_modify(
        dplyr::group_by(as_of_df, !!!syms(x$private$vars), .drop = x$private$drop),
        group_modify_fn,
        f = f, ...,
        ref_time_value = ref_time_value,
        new_col = new_col,
        .keep = TRUE
      )
    )
  })
  # Combine output into a single tibble
  out <- as_tibble(setDF(rbindlist(out)))
  # Reconstruct groups
  out <- group_by(out, !!!syms(x$private$vars), .drop = x$private$drop)

  # Unchop/unnest if we need to
  if (!as_list_col) {
    out <- tidyr::unnest(out, !!new_col, names_sep = names_sep)
  }

  # nolint start: commented_code_linter.
  # if (is_epi_df(x)) {
  #   # The analogue of `epi_df`'s `as_of` metadata for an archive is
  #   # `<archive>$versions_end`, at least in the current absence of
  #   # separate fields/columns denoting the "archive version" with a
  #   # different resolution, or from the perspective of a different
  #   # stage of a data pipeline. The `as_of` that is automatically
  #   # derived won't always match; override:
  #   attr(x, "metadata")[["as_of"]] <- private$ungrouped$versions_end
  # }
  # nolint end

  # XXX We need to work out when we want to return an `epi_df` and how
  # to get appropriate keys (see #290, #223, #163). We'll probably
  # need the commented-out code above if we ever output an `epi_df`.
  # However, as a stopgap measure to have some more consistency across
  # different ways of calling `epix_slide`, and to prevent `epi_df`
  # output with invalid metadata, always output a (grouped or
  # ungrouped) tibble.
  out <- decay_epi_df(out)

  return(out)
}


#' @include methods-epi_archive.R
#' @rdname group_by.epi_archive
#'
#' @export
is_grouped_epi_archive <- function(x) {
  inherits(x, "grouped_epi_archive")
}


#' @export
clone.grouped_epi_archive <- function(x, ...) {
  x$private$ungrouped <- x$private$ungrouped %>% clone()
  x
}


#' @rdname epix_truncate_versions_after
#' @export
epix_truncate_versions_after.grouped_epi_archive <- function(x, max_version) {
  # The grouping is irrelevant for this method; if we were to split into
  # groups and recombine appropriately, we should get the same result as
  # just leveraging the ungrouped method, so just do the latter:
  x$private$ungrouped <- epix_truncate_versions_after(x$private$ungrouped, max_version)
  x
}
