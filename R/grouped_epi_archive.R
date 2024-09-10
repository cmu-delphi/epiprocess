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
      epiprocess__ungrouped_group_vars = group_vars(x)
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
#' @importFrom dplyr group_vars
#' @export
group_vars.grouped_epi_archive <- function(x) {
  x$private$vars
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
epix_slide.grouped_epi_archive <- function(
    .x,
    .f,
    ...,
    .before = Inf,
    .versions = NULL,
    .new_col_name = NULL,
    .all_versions = FALSE) {
  # Perform some deprecated argument checks without using `<param> =
  # deprecated()` in the function signature, because they are from
  # early development versions and much more likely to be clutter than
  # informative in the signature.
  provided_args <- rlang::call_args_names(rlang::call_match())
  if (any(provided_args %in% c("x", "f", "before", "ref_time_values", "new_col_name", "all_versions"))) {
    cli::cli_abort(
      "epix_slide: you are using one of the following old argument names: `x`, `f`, `before`, `ref_time_values`,
      `new_col_name`, `all_versions`. Please use the new names: `.x`, `.f`, `.before`, `.ref_time_values`,
      `.new_col_name`, `.all_versions`."
    )
  }
  if ("group_by" %in% provided_args) {
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
  if ("all_rows" %in% provided_args) {
    cli_abort("
          The `all_rows` argument has been removed from `epix_slide` (but
          is still supported in `epi_slide`). Add rows for excluded
          results with a manual join instead.
        ", class = "epiprocess__epix_slide_all_rows_parameter_deprecated")
  }
  if ("as_list_col" %in% provided_args) {
    cli::cli_abort(
      "epix_slide: the argument `as_list_col` is deprecated. If FALSE, you can just remove it.
      If TRUE, have your given computation wrap its result using `list(result)` instead."
    )
  }
  if ("names_sep" %in% provided_args) {
    cli::cli_abort(
      "epix_slide: the argument `names_sep` is deprecated. If NULL, you can remove it, it is now default.
      If a string, please manually prefix your column names instead."
    )
  }

  # Argument validation
  if (is.null(.versions)) {
    .versions <- epix_slide_versions_default(.x$private$ungrouped)
  } else {
    assert_numeric(.versions, min.len = 1L, null.ok = FALSE, any.missing = FALSE)
    if (any(.versions > .x$private$ungrouped$versions_end)) {
      cli_abort("All `.versions` must be less than or equal to the latest version in the archive.")
    }
    if (anyDuplicated(.versions) != 0L) {
      cli_abort("All `.versions` must be unique.")
    }
    # Sort, for consistency with `epi_slide`, although the current
    # implementation doesn't take advantage of it.
    .versions <- sort(.versions)
  }

  validate_slide_window_arg(.before, .x$private$ungrouped$time_type)

  checkmate::assert_string(.new_col_name, null.ok = TRUE)
  if (!is.null(.new_col_name)) {
    if (.new_col_name %in% .x$private$vars) {
      cli_abort(c("`.new_col_name` must not be one of the grouping column name(s);
                   `epix_slide()` uses these column name(s) to label what group
                   each slide computation came from.",
        "i" = "{cli::qty(length(.x$private$vars))} grouping column name{?s}
                         {?was/were} {format_chr_with_quotes(.x$private$vars)}",
        "x" = "`.new_col_name` was {format_chr_with_quotes(.new_col_name)}"
      ))
    }
    if (identical(.new_col_name, "version")) {
      cli_abort('`.new_col_name` must not be `"version"`; `epix_slide()` uses that column name to attach the element of `.versions` associated with each slide computation') # nolint: line_length_linter
    }
  }

  assert_logical(.all_versions, len = 1L)

  # If `.f` is missing, interpret ... as an expression for tidy evaluation
  if (missing(.f)) {
    used_data_masking <- TRUE
    quosures <- enquos(...)
    if (length(quosures) == 0) {
      cli_abort("If `f` is missing then a computation must be specified via `...`.")
    }

    .slide_comp <- as_diagonal_slide_computation(quosures)
    # Magic value that passes zero args as dots in calls below. Equivalent to
    # `... <- missing_arg()`, but use `assign` to avoid warning about
    # improper use of dots.
    assign("...", missing_arg())
  } else {
    used_data_masking <- FALSE
    .slide_comp <- as_diagonal_slide_computation(.f, ...)
  }

  # Computation for one group, one time value
  comp_one_grp <- function(.data_group, .group_key,
                           .slide_comp, ...,
                           .version,
                           .new_col_name) {
    # Carry out the specified computation
    comp_value <- .slide_comp(.data_group, .group_key, .version, ...)

    # If this wasn't a tidyeval computation, we still need to check the output
    # types. We'll let `group_modify` and `vec_rbind` deal with checking for
    # type compatibility between the outputs.
    if (!used_data_masking && !(
      # vctrs considers data.frames to be vectors, but we still check
      # separately for them because certain base operations output data frames
      # with rownames, which we will allow (but might drop)
      is.data.frame(comp_value) ||
        vctrs::obj_is_vector(comp_value) && is.null(vctrs::vec_names(comp_value))
    )) {
      cli_abort("
        the slide computations must always return data frames or unnamed vectors
        (as determined by the vctrs package) (and not a mix of these two
        structures).
      ", class = "epiprocess__invalid_slide_comp_value")
    }

    .group_key_label <- if (nrow(.group_key) == 0L) {
      # Edge case: we'll get here if a requested `.version` had 0 rows and we
      # grouped by a nonzero number of columns using the default `.drop = TRUE`
      # (or on all non-factor columns with `.drop = FALSE` for some reason,
      # probably a user bug). Mimicking `dplyr`, we'll let `.group_key` provided
      # to the computation be 0 rows, but then label it using NAs. (In the
      # bizarre situation of grouping by a mix of factor and non-factor with
      # `.drop = FALSE`, `.group_key` will already have 1 row. For ungrouped
      # epix_slides and 0-variable-grouped epix_slides with either `.drop`
      # setting, we will have a 1x0 .group_key, although perhaps for the latter
      # this should be 0x0.)
      vctrs::vec_cast(NA, .group_key)
    } else {
      .group_key
    }

    # Construct result first as list, then tibble-ify, to try to avoid some
    # redundant work. However, we will sacrifice some performance here doing
    # checks here in the inner loop, in order to provide immediate feedback on
    # some formatting errors.
    res <- c(
      list(), # get list output; a bit faster than `as.list()`-ing `.group_key_label`
      .group_key_label,
      list(version = .version)
    )
    res <- vctrs::vec_recycle_common(!!!res, .size = vctrs::vec_size(comp_value))

    if (is.null(.new_col_name)) {
      if (inherits(comp_value, "data.frame")) {
        # Sometimes comp_value can parrot back columns already in `res`; allow
        # this, but balk if a column has the same name as one in `res` but a
        # different value:
        comp_nms <- names(comp_value)
        overlaps_label_names <- comp_nms %in% names(res)
        for (comp_i in which(overlaps_label_names)) {
          if (!identical(comp_value[[comp_i]], res[[comp_nms[[comp_i]]]])) {
            lines <- c(
              cli::format_error(c(
                "conflict detected between slide value computation labels and output:",
                "i" = "we are labeling slide computations with the following columns: {syms(names(res))}",
                "x" = "a slide computation output included a column {syms(comp_nms[[comp_i]])} that didn't match the label"
              )),
              capture.output(print(waldo::compare(res[[comp_nms[[comp_i]]]], comp_value[[comp_i]], x_arg = "label", y_arg = "comp output"))),
              cli::format_message(c("You likely want to rename or remove this column in your output, or debug why it has a different value."))
            )
            rlang::abort(paste(collapse = "\n", lines),
              class = "epiprocess__epix_slide_label_vs_output_column_conflict"
            )
          }
        }
        # Unpack into separate columns (without name prefix). If there are
        # columns duplicating label columns, de-dupe and order them as if they
        # didn't exist in comp_value.
        res <- c(res, comp_value[!overlaps_label_names])
      } else {
        # Apply default name (to vector or packed data.frame-type column):
        res[["slide_value"]] <- comp_value
        # TODO check for bizarre conflicting `slide_value` label col name.
        # Either here or on entry to `epix_slide` (even if there we don't know
        # whether vecs will be output). Or just turn this into a special case of
        # the preceding branch and let the checking code there generate a
        # complaint.
      }
    } else {
      # vector or packed data.frame-type column (note: overlaps with label
      # column names should already be forbidden by earlier validation):
      res[[.new_col_name]] <- comp_value
    }

    # Fast conversion:
    return(validate_tibble(new_tibble(res)))
  }

  out <- lapply(.versions, function(.version) {
    # Ungrouped as-of data; `epi_df` if `all_versions` is `FALSE`,
    # `epi_archive` if `all_versions` is `TRUE`:
    as_of_raw <- .x$private$ungrouped %>% epix_as_of(
      .version,
      min_time_value = .version - .before,
      all_versions = .all_versions
    )

    # Set:
    # * `as_of_df`, the data.frame/tibble/epi_df/etc. that we will
    #    `group_map` as the `.data` argument. Might or might not
    #    include version column.
    # * `group_map_fn`, the corresponding `.f` argument for `group_map`
    #   (not our `.f`)
    if (!.all_versions) {
      as_of_df <- as_of_raw
      group_map_fn <- comp_one_grp
    } else {
      as_of_archive <- as_of_raw
      # We essentially want to `group_modify` the archive, but
      # haven't implemented this method yet. Next best would be
      # `group_modify` on its `$DT`, but that has different
      # behavior based on whether or not `dtplyr` < 1.3.0 is loaded.
      # Instead, go through an ordinary data frame, trying to avoid
      # copies.
      if (address(as_of_archive$DT) == address(.x$private$ungrouped$DT)) {
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
      group_map_fn <- function(.data_group, .group_key,
                               .slide_comp, ...,
                               .version,
                               .new_col_name) {
        # .data_group is coming from as_of_df as a tibble, but we
        # want to feed `comp_one_grp` an `epi_archive` backed by a
        # DT; convert and wrap:
        data.table::setattr(.data_group, "sorted", dt_key)
        data.table::setDT(.data_group, key = dt_key)
        .data_group_archive <- as_of_archive
        .data_group_archive$DT <- .data_group
        comp_one_grp(.data_group_archive, .group_key,
          .slide_comp = .slide_comp, ...,
          .version = .version,
          .new_col_name = .new_col_name
        )
      }
    }

    return(
      dplyr::bind_rows(dplyr::group_map(
        dplyr::group_by(as_of_df, !!!syms(.x$private$vars), .drop = .x$private$drop),
        group_map_fn,
        .slide_comp = .slide_comp, ...,
        .version = .version,
        .new_col_name = .new_col_name,
        .keep = TRUE
      ))
    )
  })
  # Combine output into a single tibble (allowing for packed columns)
  out <- vctrs::vec_rbind(!!!out)
  # Reconstruct groups
  out <- group_by(out, !!!syms(.x$private$vars), .drop = .x$private$drop)

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
