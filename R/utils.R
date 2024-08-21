# Note: update `wrap_symbolics` and `wrap_varnames` (parameters, parameter
# defaults, bodies) together.

#' Line wrap list holding [symbolic][rlang::is_symbolic], with prefix&indent
#'
#' Helps pretty-print these objects. Adds backticks, commas, prefixes, and
#' indentation. Wraps lines, but won't insert line breaks in the middle of any
#' name while doing so.
#'
#' @param symbolics List of [symbolic][rlang::is_symbolic] objects: the variable
#'   names (potentially empty)
#' @param initial Optional; single string: a prefix for the initial line in the
#'   result; e.g., "Variable names: ". Defaults to "". Any non-initial lines
#'   will be indented with whitespace matching the (estimated) visual width of
#'   `initial`.
#' @param common_prefix Optional; single string: a prefix for every line (will
#'   appear before `initial`); e.g., "# ". Defaults to "".
#' @param none_str Optional; single string: what to display when given
#'   `length`-0 input. Will be combined with `common_prefix` and `initial`.
#' @param width Optional; single integer: desired maximum formatted line width.
#'   The formatted output may not obey this setting if `common_prefix` plus
#'   `initial` is long or the printing width is very narrow.
#' @return `chr`; to print, use [`base::writeLines`].
#'
#' @noRd
wrap_symbolics <- function(symbolics,
                           initial = "", common_prefix = "", none_str = "<none>",
                           width = getOption("width", 80L)) {
  if (!all(purrr::map_lgl(symbolics, rlang::is_symbolic))) {
    cli_abort("`symbolics` must be a list of symbolic objects")
  }
  assert_character(initial, len = 1L)
  assert_character(common_prefix, len = 1L)
  assert_character(none_str, len = 1L)
  assert_int(width, lower = 1L)

  prefix <- strrep(" ", nchar(initial, type = "width"))
  full_initial <- paste0(common_prefix, initial)
  full_prefix <- paste0(common_prefix, prefix)
  full_initial_width <- nchar(full_initial, type = "width")
  minimum_reasonable_line_width_for_syms <- 20L
  line_width_for_syms <- max(
    width - full_initial_width,
    minimum_reasonable_line_width_for_syms
  )
  unprefixed_lines <-
    if (length(symbolics) == 0L) {
      none_str
    } else {
      utils::capture.output(
        withr::with_options(list("width" = line_width_for_syms), {
          # `paste0` already takes care of necessary backquotes. `cat` with
          # `fill=TRUE` takes care of spacing + line wrapping exclusively
          # between elements. We need to add commas appropriately.
          cat(paste0(symbolics, c(rep(",", times = length(symbolics) - 1L), "")), fill = TRUE)
        })
      )
    }
  lines <- paste0(
    c(full_initial, rep(full_prefix, times = length(unprefixed_lines) - 1L)),
    unprefixed_lines
  )
  lines
}

#' Line wrap `chr` holding variable/column/other names, with prefix&indent
#'
#' @param nms Character vector: the variable names (potentially empty)
#' @inheritParams wrap_symbolics
#' @return `chr`; to print, use [`base::writeLines`].
#'
#' @noRd
wrap_varnames <- function(nms,
                          initial = "", common_prefix = "", none_str = "<none>",
                          width = getOption("width", 80L)) {
  # (Repeating parameter names and default args here for better autocomplete.
  # Using `...` instead would require less upkeep, but have worse autocomplete.)
  assert_character(nms)
  wrap_symbolics(rlang::syms(nms), initial = initial, common_prefix = common_prefix, none_str = none_str, width = width)
}

#' Paste `chr` entries (lines) together with `"\n"` separators, trailing `"\n"`
#'
#' @param lines `chr`
#' @return string
#'
#' @noRd
paste_lines <- function(lines) {
  paste(paste0(lines, "\n"), collapse = "")
}


#' Assert that a sliding computation function takes enough args
#'
#' @param f Function; specifies a computation to slide over an `epi_df` or
#'  `epi_archive` in `epi_slide` or `epix_slide`.
#' @param ... Dots that will be forwarded to `f` from the dots of `epi_slide` or
#'   `epix_slide`.
#'
#' @importFrom rlang is_missing
#' @importFrom purrr map_lgl
#' @importFrom utils tail
#'
#' @noRd
assert_sufficient_f_args <- function(f, ...) {
  mandatory_f_args_labels <- c("window data", "group key", "reference time value")
  n_mandatory_f_args <- length(mandatory_f_args_labels)
  args <- formals(args(f))
  args_names <- names(args)
  # Remove named arguments forwarded from `epi[x]_slide`'s `...`:
  forwarded_dots_names <- names(rlang::call_match(dots_expand = FALSE)[["..."]])
  # positional calling args will skip over args matched by named calling args
  # extreme edge case: `epi[x]_slide(<stuff>, dot = 1, `...` = 2)`
  args_matched_in_dots <- args_names %in% forwarded_dots_names & args_names != "..."

  remaining_args <- args[!args_matched_in_dots]
  remaining_args_names <- names(remaining_args)
  # note that this doesn't include unnamed args forwarded through `...`.
  dots_i <- which(remaining_args_names == "...") # integer(0) if no match
  n_f_args_before_dots <- dots_i - 1L
  if (length(dots_i) != 0L) {
    # `f` has a dots "arg"
    # Keep all arg names before `...`
    mandatory_args_mapped_names <- remaining_args_names[seq_len(n_f_args_before_dots)] # nolint: object_usage_linter

    if (n_f_args_before_dots < n_mandatory_f_args) {
      mandatory_f_args_in_f_dots <-
        tail(mandatory_f_args_labels, n_mandatory_f_args - n_f_args_before_dots)

      cli::cli_warn(
        "`f` might not have enough positional arguments before its `...`; in
        the current `epi[x]_slide` call, the {mandatory_f_args_in_f_dots} will
        be included in `f`'s `...`; if `f` doesn't expect those arguments, it
        may produce confusing error messages",
        class = "epiprocess__assert_sufficient_f_args__mandatory_f_args_passed_to_f_dots",
        epiprocess__f = f,
        epiprocess__mandatory_f_args_in_f_dots = mandatory_f_args_in_f_dots
      )
    }
  } else { # `f` doesn't have a dots "arg"
    if (length(args_names) < n_mandatory_f_args + rlang::dots_n(...)) {
      # `f` doesn't take enough args.
      if (rlang::dots_n(...) == 0L) {
        # common case; try for friendlier error message
        cli_abort("`f` must take at least {n_mandatory_f_args} arguments",
          class = "epiprocess__assert_sufficient_f_args__f_needs_min_args",
          epiprocess__f = f
        )
      } else {
        # less common; highlight that they are (accidentally?) using dots forwarding
        cli_abort(
          "`f` must take at least {n_mandatory_f_args} arguments plus the
          {rlang::dots_n(...)} arguments forwarded through `epi[x]_slide`'s
          `...`, or a named argument to `epi[x]_slide` was misspelled",
          class = "epiprocess__assert_sufficient_f_args__f_needs_min_args_plus_forwarded",
          epiprocess__f = f
        )
      }
    }
  }
  # Check for args with defaults that are filled with mandatory positional
  # calling args. If `f` has fewer than n_mandatory_f_args before `...`, then we
  # only need to check those args for defaults. Note that `n_f_args_before_dots` is
  # length 0 if `f` doesn't accept `...`.
  n_remaining_args_for_default_check <- min(c(n_f_args_before_dots, n_mandatory_f_args))
  default_check_args <- remaining_args[seq_len(n_remaining_args_for_default_check)]
  default_check_args_names <- names(default_check_args)
  has_default_replaced_by_mandatory <- map_lgl(default_check_args, ~ !is_missing(.x))
  if (any(has_default_replaced_by_mandatory)) {
    default_check_mandatory_args_labels <-
      mandatory_f_args_labels[seq_len(n_remaining_args_for_default_check)]
    # ^ excludes any mandatory args absorbed by f's `...`'s:
    mandatory_args_replacing_defaults <- default_check_mandatory_args_labels[has_default_replaced_by_mandatory] # nolint: object_usage_linter
    args_with_default_replaced_by_mandatory <- rlang::syms(default_check_args_names[has_default_replaced_by_mandatory]) # nolint: object_usage_linter
    cli::cli_abort(
      "`epi[x]_slide` would pass the {mandatory_args_replacing_defaults} to
      `f`'s {args_with_default_replaced_by_mandatory} argument{?s}, which
      {?has a/have} default value{?s}; we suspect that `f` doesn't expect
      {?this arg/these args} at all and may produce confusing error messages.
      Please add additional arguments to `f` or remove defaults as
      appropriate.",
      class = "epiprocess__assert_sufficient_f_args__required_args_contain_defaults",
      epiprocess__f = f
    )
  }
}

#' Generate a `epi[x]_slide` computation function from a function, formula, or quosure
#'
#' @description `as_slide_computation()` transforms a one-sided formula or a
#'  quosure into a function; functions are returned as-is or with light
#'  modifications to calculate `ref_time_value`.
#'
#' This code extends `rlang::as_function` to create functions that take three
#' arguments. The arguments can be accessed via the idiomatic `.`, `.x`, and
#' `.y`, extended to include `.z`; positional references `..1` and `..2`,
#' extended to include `..3`; and also by `epi[x]_slide`-specific names
#' `.group_key` and `.ref_time_value`.
#'
#' @source This code and documentation are based on
#'  [`as_function`](https://github.com/r-lib/rlang/blob/c55f6027928d3104ed449e591e8a225fcaf55e13/R/fn.R#L343-L427)
#'  from Hadley Wickham's `rlang` package.
#'
#'  Below is the original license for the `rlang` package.
#'
#'
#'  MIT License
#'
#'  Copyright (c) 2020 rlang authors
#'
#'  Permission is hereby granted, free of charge, to any person obtaining a copy
#'  of this software and associated documentation files (the "Software"), to deal
#'  in the Software without restriction, including without limitation the rights
#'  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#'  copies of the Software, and to permit persons to whom the Software is
#'  furnished to do so, subject to the following conditions:
#'
#'  The above copyright notice and this permission notice shall be included in all
#'  copies or substantial portions of the Software.
#'
#'  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#'  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#'  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#'  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#'  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#'  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#'  SOFTWARE.
#'
#'
#'  Portions of the original code used in this adaptation:
#'    1. Much of the documentation and examples
#'    2. The general flow of the function, including branching conditions
#'    3. Error conditions and wording
#'    4. The chunk converting a formula into a function, see
#'    https://github.com/r-lib/rlang/blob/c55f6027928d3104ed449e591e8a225fcaf55e13/R/fn.R#L411-L418
#'
#'  Changes made include:
#'    1. Updates to documentation due to new functionality
#'    2. The removal of function-as-string processing logic and helper arg
#'    `env`
#'    3. The addition of an output function wrapper that defines a data mask
#'    for evaluating quosures
#'    4. Calling an argument-checking function
#'    5. Replacing rlang error functions with internal error functions
#'
#' @param f A function, one-sided formula, or quosure.
#'
#'   If a **function**, the function is returned as-is, with no
#'   modifications.
#'
#'   If a **formula**, e.g. `~ mean(.x$cases)`, it is converted to a function
#'   with up to three arguments: `.x` (single argument), or `.x` and `.y`
#'   (two arguments), or `.x`, `.y`, and `.z` (three arguments). The `.`
#'   placeholder can be used instead of `.x`, `.group_key` can be used in
#'   place of `.y`, and `.ref_time_value` can be used in place of `.z`. This
#'   allows you to create very compact anonymous functions (lambdas) with up
#'   to three inputs. Functions created from formulas have a special class.
#'   Use `inherits(fn, "epiprocess_slide_computation")` to test for it.
#'
#'   If a **quosure**, in the case that `f` was not provided to the parent
#'   `epi[x]_slide` call and the `...` is interpreted as an expression for
#'   tidy evaluation, it is evaluated within a wrapper function. The wrapper
#'   sets up object access via a data mask.
#'
#' @param ... Additional arguments to pass to the function or formula
#'  specified via `x`. If `x` is a quosure, any arguments passed via `...`
#'  will be ignored.
#' @examples
#' f <- as_slide_computation(~ .x + 1)
#' f(10)
#'
#' g <- as_slide_computation(~ -1 * .)
#' g(4)
#'
#' h <- as_slide_computation(~ .x - .group_key)
#' h(6, 3)
#'
#' @importFrom rlang is_function new_function f_env is_environment missing_arg
#'  f_rhs is_formula caller_arg caller_env
#'
#' @noRd
as_slide_computation <- function(f, ...) {
  arg <- caller_arg(f)
  call <- caller_env()

  if (rlang::is_quosures(f)) {
    quosures <- rlang::quos_auto_name(f) # resolves := among other things
    nms <- names(quosures)
    manually_named <- rlang::names2(f) != "" | vapply(f, function(quosure) {
      expression <- rlang::quo_get_expr(quosure)
      is.call(expression) && expression[[1L]] == rlang::sym(":=")
    }, FUN.VALUE = logical(1L))
    fn <- function(.x, .group_key, .ref_time_value) {
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
      data_mask$.ref_time_value <- .ref_time_value
      common_size <- NULL
      # The data mask is an environment; it doesn't track the binding order.
      # We'll track that separately. For efficiency, we'll use `c` to add to
      # this order, and deal with binding redefinitions at the end. We'll
      # reflect deletions immediately (current implementation of `new_tibble`
      # seems like it would exclude `NULL` bindings for us but `?new_tibble`
      # doesn't reflect this behavior).
      results_multiorder <- character(0L)
      for (quosure_i in seq_along(f)) {
        # XXX could capture and improve error messages here at cost of recover()ability
        quosure_result_raw <- rlang::eval_tidy(quosures[[quosure_i]], data_mask)
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
            {rlang::expr_deparse(rlang::quo_get_expr(f[[quosure_i]]))}}; it
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

    return(fn)
  }

  if (is_function(f)) {
    # Check that `f` takes enough args
    assert_sufficient_f_args(f, ...)
    return(f)
  }

  if (is_formula(f)) {
    if (is_quosure(f)) {
      cli_abort("`f` argument to `as_slide_computation()` cannot be a `quosure`; it should probably be a `quosures`.  This is likely an internal bug in `{{epiprocess}}`.") # nolint: line_length_linter
    }

    if (length(f) > 2) {
      cli_abort("{.code {arg}} must be a one-sided formula",
        class = "epiprocess__as_slide_computation__formula_is_twosided",
        epiprocess__f = f,
        call = call
      )
    }
    if (rlang::dots_n(...) > 0L) {
      cli_abort(
        "No arguments can be passed via `...` when `f` is a formula, or there
        are unrecognized/misspelled parameter names, or there is a trailing
        comma in the `epi[x]_slide()` call.",
        class = "epiprocess__as_slide_computation__formula_with_dots",
        epiprocess__f = f,
        epiprocess__enquos_dots = enquos(...)
      )
    }

    env <- f_env(f)
    if (!is_environment(env)) {
      cli_abort("Formula must carry an environment.",
        class = "epiprocess__as_slide_computation__formula_has_no_env",
        epiprocess__f = f,
        epiprocess__f_env = env,
        arg = arg, call = call
      )
    }

    args <- list(
      ... = missing_arg(),
      .x = quote(..1), .y = quote(..2), .z = quote(..3),
      . = quote(..1), .group_key = quote(..2), .ref_time_value = quote(..3)
    )
    fn <- new_function(args, f_rhs(f), env)
    fn <- structure(fn, class = c("epiprocess_slide_computation", "function"))

    return(fn)
  }

  cli_abort(
    "Can't convert an object of class {paste(collapse = ' ', deparse(class(f)))}
      to a slide computation",
    class = "epiprocess__as_slide_computation__cant_convert_catchall",
    epiprocess__f = f,
    epiprocess__f_class = class(f),
    arg = arg,
    call = call
  )
}


guess_geo_type <- function(geo_value) {
  if (is.character(geo_value)) {
    # Convert geo values to lowercase
    geo_value <- tolower(geo_value)

    # If all geo values are state abbreviations, then use "state"
    state_values <- c(
      tolower(datasets::state.abb),
      "as", "dc", "gu", "mp", "pr", "vi"
    )
    if (all(geo_value %in% state_values)) {
      return("state")
    } else if (all(grepl("[a-z]{2}", geo_value)) && !any(grepl("[a-z]{3}", geo_value))) {
      # Else if all geo values are 2 letters, then use "nation"
      return("nation")
    } else if (all(grepl("[0-9]{5}", geo_value)) && !any(grepl("[0-9]{6}", geo_value))) {
      # Else if all geo values are 5 numbers, then use "county"
      return("county")
    }
  } else if (is.numeric(geo_value)) {
    # Convert geo values to integers
    geo_value <- as.integer(geo_value)

    # If the max geo value is at most 10, then use "hhs"
    if (max(geo_value) <= 10) {
      return("hhs")
    }

    # Else if the max geo value is at most 457, then use "hrr"
    if (max(geo_value) <= 457) {
      return("hrr")
    }
  }

  return("custom")
}


guess_time_type <- function(time_value, time_value_arg = rlang::caller_arg(time_value)) {
  if (inherits(time_value, "Date")) {
    unique_time_gaps <- as.numeric(diff(sort(unique(time_value))))
    # Gaps in a weekly date sequence will cause some diffs to be larger than 7
    # days, so check modulo 7 equality, rather than equality with 7.
    if (all(unique_time_gaps %% 7 == 0)) {
      return("week")
    }
    if (all(unique_time_gaps >= 28)) {
      cli_abort(
        "Found a monthly or longer cadence in the time column `{time_value_arg}`.
        Consider using tsibble::yearmonth for monthly data and 'YYYY' integers for year data."
      )
    }
    return("day")
  } else if (inherits(time_value, "yearmonth")) {
    return("yearmonth")
  } else if (rlang::is_integerish(time_value)) {
    return("integer")
  }

  cli_warn(
    "Unsupported time type in column `{time_value_arg}`, with class {.code {class(time_value)}}.
    Time-related functionality may have unexpected behavior.
    ",
    class = "epiprocess__guess_time_type__unknown_time_type",
    epiprocess__time_value = time_value
  )
  return("custom")
}

#'  given a vector of characters, add the same values, but upcased, e.g.
#'   "date" -> c("date", "Date")
#'   "target_date" -> c("target_date", "Target_Date")
#' @keywords internal
upcase_snake_case <- function(vec) {
  upper_vec <- strsplit(vec, "_") %>%
    map(function(name) paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), collapse = "_")) %>%
    unlist()
  c(vec, upper_vec)
}

#' potential time_value columns
#' @description
#' the full list of potential substitutions for the `time_value` column name:
#' `r time_column_names()`
#' @export
time_column_names <- function() {
  substitutions <- c(
    "time_value", "date", "time", "datetime", "dateTime", "date_time", "target_date",
    "week", "epiweek", "month", "mon", "year", "yearmon", "yearmonth",
    "yearMon", "yearMonth", "dates", "time_values", "target_dates", "time_Value"
  )
  substitutions <- upcase_snake_case(substitutions)
  names(substitutions) <- rep("time_value", length(substitutions))
  return(substitutions)
}
#
#' potential geo_value columns
#' @description
#' the full list of potential substitutions for the `geo_value` column name:
#' `r geo_column_names()`
#' @export
geo_column_names <- function() {
  substitutions <- c(
    "geo_value", "geo_values", "geo_id", "geos", "location", "jurisdiction", "fips", "zip",
    "county", "hrr", "msa", "state", "province", "nation", "states",
    "provinces", "counties", "geo_Value"
  )
  substitutions <- upcase_snake_case(substitutions)
  names(substitutions) <- rep("geo_value", length(substitutions))
  return(substitutions)
}

#' potential version columns
#' @description
#' the full list of potential substitutions for the `version` column name:
#' `r version_column_names()`
#' @export
version_column_names <- function() {
  substitutions <- c(
    "version", "issue", "release"
  )
  substitutions <- upcase_snake_case(substitutions)
  names(substitutions) <- rep("version", length(substitutions))
  return(substitutions)
}

#' rename potential time_value columns
#'
#' @description
#' potentially renames
#' @param x the tibble to potentially rename
#' @param substitutions a named vector. the potential substitions, with every name `time_value`
#' @keywords internal
#' @importFrom cli cli_inform cli_abort
#' @importFrom dplyr rename
guess_column_name <- function(x, column_name, substitutions) {
  if (!(column_name %in% names(x))) {
    # if none of the names are in substitutions, and `column_name` isn't a column, we're missing a relevant column
    if (!any(names(x) %in% substitutions)) {
      cli_abort(
        "There is no {column_name} column or similar name.
         See e.g. [`time_column_name()`] for a complete list",
        class = "epiprocess__guess_column__multiple_substitution_error"
      )
    }

    tryCatch(
      {
        x <- x %>% rename(any_of(substitutions))
        cli_inform(
          "inferring {column_name} column.",
          class = "epiprocess__guess_column_inferring_inform"
        )
        return(x)
      },
      error = function(cond) {
        cli_abort(
          "{intersect(names(x), substitutions)}
           are both/all valid substitutions for {column_name}.
           Either `rename` some yourself or drop some.",
          class = "epiprocess__guess_column__multiple_substitution_error"
        )
      }
    )
  }
  return(x)
}

##########


quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

##########

# Create an auto-named list
enlist <- function(...) {
  # converted to thin wrapper around
  rlang::dots_list(
    ...,
    .homonyms = "error",
    .named = TRUE,
    .check_assign = TRUE
  )
}

# Variable assignment from a list. NOT USED. Something is broken, this doesn't
# seem to work completely as expected: the variables it define don't propogate
# down to child environments
list2var <- function(x) {
  list2env(x, envir = parent.frame())
}

##########

#' [`lifecycle::is_present`] for enquosed deprecated NSE arg
#'
#' [`lifecycle::is_present`] is designed for use with args that undergo standard
#' evaluation, rather than non-standard evaluation (NSE). This function is
#' designed to fulfill a similar purpose, but for args we have
#' [enquosed][rlang::enquo] in preparation for NSE.
#'
#' @param quo [enquosed][rlang::enquo] arg
#' @return bool; was `quo` "present", or did it look like a missing quosure or
#'   have an expr that looked like `deprecated()` or `lifecycle::deprecated()`?
#'
#' @examples
#'
#' fn <- function(x = deprecated()) {
#'   deprecated_quo_is_present(rlang::enquo(x))
#' }
#'
#' fn() # FALSE
#' fn(.data$something) # TRUE
#'
#' # Functions that wrap `fn` should forward the NSE arg to `fn` using
#' # [`{{ arg }}`][rlang::embrace-operator] (or, if they are working from an
#' # argument that has already been defused into a quosure, `!!quo`). (This is
#' # already how NSE arguments that will be enquosed should be forwarded.)
#'
#' wrapper1 <- function(x = deprecated()) fn({{ x }})
#' wrapper2 <- function(x = lifecycle::deprecated()) fn({{ x }})
#' wrapper3 <- function(x) fn({{ x }})
#' wrapper4 <- function(x) fn(!!rlang::enquo(x))
#'
#' wrapper1() # FALSE
#' wrapper2() # FALSE
#' wrapper3() # FALSE
#' wrapper4() # FALSE
#'
#' # More advanced: wrapper that receives an already-enquosed arg:
#'
#' inner_wrapper <- function(quo) fn(!!quo)
#' outer_wrapper1 <- function(x = deprecated()) inner_wrapper(rlang::enquo(x))
#'
#' outer_wrapper1() # FALSE
#'
#' # Improper argument forwarding from a wrapper function will cause this
#' # function to produce incorrect results.
#' bad_wrapper1 <- function(x) fn(x)
#' bad_wrapper1() # TRUE, bad
#'
#' @importFrom lifecycle deprecated
#'
#' @noRd
deprecated_quo_is_present <- function(quo) {
  if (!rlang::is_quosure(quo)) {
    cli_abort("`quo` must be a quosure; `enquo` the arg first",
      internal = TRUE
    )
  } else if (rlang::quo_is_missing(quo)) {
    FALSE
  } else {
    quo_expr <- rlang::get_expr(quo)
    if (identical(quo_expr, rlang::expr(deprecated())) || identical(quo_expr, rlang::expr(lifecycle::deprecated()))) { # nolint: object_usage_linter
      FALSE
    } else {
      TRUE
    }
  }
}

##########

#' Find the greatest common divisor of two numeric scalars
#'
#' Not expected to be used directly; output class isn't precise, and checks
#' could be moved away into [`gcd_num`].
#'
#' An implementation of a least absolute remainder Euclidean algorithm (See,
#' e.g., Moore, Thomas. "On the least absolute remainder Euclidean algorithm."
#' The Fibonacci Quarterly (1992).)
#'
#' Notes on this implementation:
#' * We allow positive or negative inputs, and don't require |a| > |b|.
#' * `round` combines the job of truncating division and deciding between
#'    positive and negative remainders.
#' * We use some tolerance parameters and different checks to allow this to work
#'   on floating-point numbers. Perhaps they could be altered or removed if we
#'   are passed integers, but for simplicity, we always perform these checks.
#'
#' @param a Length 1, `is.numeric`; the first number
#' @param b Length 1, `is.numeric`; the second number
#' @param rrtol Optional, length 1, `is.numeric`, non-negative; the remainder
#'   relative tolerance: consider a remainder from a division operation to be
#'   zero if it is `abs(remainder/divisor) <= rrtol`. Could also be described as
#'   a tolerance on the fractional part of the proper quotient. Default is 1e-6.
#' @param pqlim Optional, length 1, `is.numeric`, non-negative; the proper
#'   quotient limit: consider a divisor to be zero if `abs(dividend/divisor) >=
#'   pqlim`.
#' @param irtol Optional, length 1, `is.numeric`, non-negative; the iterand
#'   relative tolerance: consider `a` and `b` to have no gcd if the absolute
#'   value of an iterand (and consequently also any result that might be
#'   subsequently produced, as absolute values of iterands are decreasing) is
#'   `<= irtol * a` or `<= irtol * b`. Also can be seen as the reciprocal of a
#'   limit on the number `k` needed to achieve `k * gcd_result ==
#'   max(abs(a),abs(b))`.
#' @return Length 1, `is.numeric`: the gcd. (Or an error.) Expected to be a
#'   double unless `b` is the GCD and an integer, in which case it is expected
#'   be an integer.
#'
#' @noRd
gcd2num <- function(a, b, rrtol = 1e-6, pqlim = 1e6, irtol = 1e-6) {
  assert_numeric(a, len = 1L)
  assert_numeric(b, len = 1L)
  assert_numeric(rrtol, len = 1L, lower = 0)
  assert_numeric(pqlim, len = 1L, lower = 0)
  assert_numeric(irtol, len = 1L, lower = 0)
  if (is.na(a) || is.na(b) || a == 0 || b == 0 || abs(a / b) >= pqlim || abs(b / a) >= pqlim) {
    cli_abort(
      "`a` and/or `b` is either `NA` or exactly zero, or one is so much
      smaller than the other that it looks like it's supposed to be zero; see `pqlim` setting."
    )
  }
  iatol <- irtol * max(a, b)
  a_curr <- a
  b_curr <- b
  while (TRUE) {
    # `b_curr` is the candidate GCD / iterand; check first if it seems too small:
    if (abs(b_curr) <= iatol) {
      cli_abort(
        "No GCD found; remaining potential GCDs are all too small relative
        to one/both of the original inputs; see `irtol` setting."
      )
    }
    remainder <- a_curr - round(a_curr / b_curr) * b_curr
    if (abs(remainder / b_curr) <= rrtol) {
      # We consider `a_curr` divisible by `b_curr`; `b_curr` is the GCD or its negation
      return(abs(b_curr))
    }
    a_curr <- b_curr
    b_curr <- remainder
  }
}

#' Find the greatest common divisor of all entries in a numeric vector
#'
#' @param dividends `is.numeric`, `length` > 0; the dividends for which to find
#'   the greatest common divisor.
#' @param ... should be empty; forces the following parameters to be passed by
#'   name
#' @inheritParams gcd2num
#' @return Same [`vctrs::vec_ptype`] as `dividends`, `length` 1: the gcd. (Or an
#'   error.)
#'
#' @noRd
gcd_num <- function(dividends, ..., rrtol = 1e-6, pqlim = 1e6, irtol = 1e-6) {
  if (!is.numeric(dividends) || length(dividends) == 0L) {
    cli_abort("`dividends` must satisfy `is.numeric`, and have `length` > 0")
  }
  if (rlang::dots_n(...) != 0L) {
    cli_abort(
      "`...` should be empty; all dividends should go in a single `dividends`
      vector, and all tolerance&limit settings should be passed by name."
    )
  }
  # We expect a bunch of duplicate `dividends` for some applications.
  # De-duplicate to reduce work. Sort by absolute value to attempt to reduce
  # workload. Also take `abs` early on as another form of deduplication and to
  # make the sort simpler. Use `na.last=FALSE` in the sort to preserve presence
  # of `NA`s in order to get a better error message in this case.
  optimized_dividends <- sort(unique(abs(dividends)), na.last = FALSE)
  # Note that taking the prime factorizations of a set of integers, and
  # calculating the minimum power for each prime across all these
  # factorizations, yields the prime factorization of the GCD of the set of
  # integers. We could carry these parallel minimum operations out using
  # `reduce`, so we see that calculating the GCD of a set of integers can be
  # done via `reduce`. Note that we should always have "gcd_real"(reals) =
  # gcd_int(reals / integerizing_divisor) * integerizing_divisor for *every*
  # integerizing divisor that would make "gcd_int" applicable. There is a
  # greatest integerizing divisor if there is a GCD at all, and this is the
  # "gcd_real" itself, for which the "gcd_int" in the previous equation is 1;
  # the set of valid integerizing divisors is the set of nonzero integer
  # multiples of the greatest integerizing divisor. The gcd_real of X U Y is an
  # integerizing factor for X U Y as well as X and Y individually, and we can
  # see gcd_real(X U Y) = gcd_int(XUY / gcd(XUY)) * gcd(XUY) =
  # gcd2int(gcd_int(X/gcd_real(XUY)), gcd_int(Y/gcd_real(XUY))) * gcd(XUY) =
  # gcd2real(gcd_int(X/gcd_real(XUY))*gcd_real(XUY),
  # gcd_int(Y/gcd_real(XUY))*gcd_real(XUY)) = gcd2real(gcd_real(X),
  # gcd_real(Y)). So "gcd_real" should also be `reduce`-compatible.
  numeric_gcd <- purrr::reduce(optimized_dividends, gcd2num,
    rrtol = rrtol, pqlim = pqlim, irtol = irtol
  )
  vctrs::vec_cast(numeric_gcd, dividends)
}

#' Use max valid period as guess for `period` of `time_values`
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param time_values Vector containing time-interval-like or time-point-like
#'   data, with at least two distinct values.
#' @param time_values_arg Optional, string; name to give `time_values` in error
#'   messages. Defaults to quoting the expression the caller fed into the
#'   `time_values` argument.
#' @param ... Should be empty, there to satisfy the S3 generic.
#' @return length-1 vector; `r lifecycle::badge("experimental")` class will
#'   either be the same class as [`base::diff()`] on such time values, an
#'   integer, or a double, such that all `time_values` can be exactly obtained
#'   by adding `k * result` for an integer k, and such that there is no smaller
#'   `result` that can achieve this.
#'
#' @export
guess_period <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  UseMethod("guess_period")
}

#' @export
guess_period.default <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  rlang::check_dots_empty()
  sorted_distinct_time_values <- sort(unique(time_values))
  if (length(sorted_distinct_time_values) < 2L) {
    cli_abort("Not enough distinct values in {.code {time_values_arg}} to guess the period.",
      class = "epiprocess__guess_period__not_enough_times",
      time_values = time_values
    )
  }
  skips <- diff(sorted_distinct_time_values)
  # Certain diff results have special classes or attributes; use vctrs to try to
  # appropriately destructure for gcd_num, then restore to their original class
  # & attributes.
  skips_data <- vctrs::vec_data(skips)
  period_data <- gcd_num(skips_data, rrtol = 0)
  vctrs::vec_restore(period_data, skips)
}

# `full_seq()` doesn't like difftimes, so convert to the natural units of some time types:

#' @export
guess_period.Date <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  as.numeric(NextMethod(), units = "days")
}

#' @export
guess_period.POSIXt <- function(time_values, time_values_arg = rlang::caller_arg(time_values), ...) {
  as.numeric(NextMethod(), units = "secs")
}

validate_slide_window_arg <- function(arg, time_type, allow_inf = TRUE, arg_name = rlang::caller_arg(arg)) {
  if (is.null(arg)) {
    cli_abort("`{arg_name}` is a required argument for slide functions.", class = "epiprocess__validate_slide_window_arg")
  }

  if (!checkmate::test_scalar(arg)) {
    cli_abort("Slide function expected `{arg_name}` to be a scalar value.", class = "epiprocess__validate_slide_window_arg")
  }

  if (time_type == "custom") {
    cli_abort(
      "Unsure how to interpret slide units with a custom time type. Consider converting your time
      column to a Date, yearmonth, or integer type.",
      class = "epiprocess__validate_slide_window_arg"
    )
  }

  msg <- ""
  if (!identical(arg, Inf)) {
    if (time_type == "day") {
      if (!test_int(arg, lower = 0L) && !(inherits(arg, "difftime") && units(arg) == "days")) {
        msg <- glue::glue_collapse(c("difftime with units in days", "non-negative integer", "Inf"), " or ")
      }
    } else if (time_type == "week") {
      if (!(inherits(arg, "difftime") && units(arg) == "weeks")) {
        msg <- glue::glue_collapse(c("difftime with units in weeks", "Inf"), " or ")
      }
    } else if (time_type == "yearmonth") {
      if (!test_int(arg, lower = 0L) || inherits(arg, "difftime")) {
        msg <- glue::glue_collapse(c("non-negative integer", "Inf"), " or ")
      }
    } else if (time_type == "integer") {
      if (!test_int(arg, lower = 0L) || inherits(arg, "difftime")) {
        msg <- glue::glue_collapse(c("non-negative integer", "Inf"), " or ")
      }
    } else {
      msg <- glue::glue_collapse(c("difftime", "non-negative integer", "Inf"), " or ")
    }
  } else {
    if (!allow_inf) {
      msg <- glue::glue_collapse(c("a difftime", "a non-negative integer"), " or ")
    }
  }
  if (msg != "") {
    cli_abort(
      "Slide function expected `{arg_name}` to be a {msg}.",
      class = "epiprocess__validate_slide_window_arg"
    )
  }
}
