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

  # A quosure is a type of formula, so be careful with the order and contents
  # of the conditional logic here.
  if (is_quosure(f)) {
    fn <- function(.x, .group_key, .ref_time_value) {
      # Convert to environment to standardize between tibble and R6
      # based inputs. In both cases, we should get a simple
      # environment with the empty environment as its parent.
      data_env <- rlang::as_environment(.x)
      data_mask <- rlang::new_data_mask(bottom = data_env, top = data_env)
      data_mask$.data <- rlang::as_data_pronoun(data_mask)
      # We'll also install `.x` directly, not as an `rlang_data_pronoun`, so
      # that we can, e.g., use more dplyr and epiprocess operations.
      data_mask$.x <- .x
      data_mask$.group_key <- .group_key
      data_mask$.ref_time_value <- .ref_time_value
      rlang::eval_tidy(f, data_mask)
    }

    return(fn)
  }

  if (is_function(f)) {
    # Check that `f` takes enough args
    assert_sufficient_f_args(f, ...)
    return(f)
  }

  if (is_formula(f)) {
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
        are unrecognized/misspelled parameter names.",
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

  # If we got here then we failed
  return("custom")
}

guess_time_type <- function(time_value) {
  # Convert character time values to Date or POSIXct
  if (is.character(time_value)) {
    if (nchar(time_value[1]) <= 10L) {
      new_time_value <- tryCatch(
        {
          as.Date(time_value)
        },
        error = function(e) NULL
      )
    } else {
      new_time_value <- tryCatch(
        {
          as.POSIXct(time_value)
        },
        error = function(e) NULL
      )
    }
    if (!is.null(new_time_value)) time_value <- new_time_value
  }

  # Now, if a POSIXct class, then use "day-time"
  if (inherits(time_value, "POSIXct")) {
    return("day-time")
  } else if (inherits(time_value, "Date")) {
    # Else, if a Date class, then use "week" or "day" depending on gaps
    # Convert to numeric so we can use the modulo operator.
    unique_time_gaps <- as.numeric(diff(sort(unique(time_value))))
    # We need to check the modulus of `unique_time_gaps` in case there are
    # missing dates. Gaps in a weekly date sequence will cause some diffs to
    # be larger than 7 days. If we just check if `diffs == 7`, it will fail
    # unless the weekly date sequence is already complete.
    return(ifelse(all(unique_time_gaps %% 7 == 0), "week", "day"))
  } else if (inherits(time_value, "yearweek")) {
    # Else, check whether it's one of the tsibble classes
    return("yearweek")
  } else if (inherits(time_value, "yearmonth")) {
    return("yearmonth")
  } else if (inherits(time_value, "yearquarter")) {
    return("yearquarter")
  } else if (rlang::is_integerish(time_value) &&
    all(nchar(as.character(time_value)) == 4L)) { # nolint: indentation_linter
    return("year")
  }

  # If we got here then we failed
  return("custom")
}

#'  given a vector of characters, add the same values, but upcased, e.g.
#'   "date" -> c("date", "Date")
#'   "target_date" -> c("target_date", "Target_Date")
#' @keywords internal
upcase_snake_case <- function(x) {
  X <- strsplit(x, "_") %>%
    map(function(name) paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), collapse = "_")) %>%
    unlist()
  c(x, X)
}

#' given an arbitrary
#' @keywords internal
guess_time_column_name <- function(x, substitutions = NULL) {
  if (!("time_value" %in% names(x))) {
    if (is.null(substitutions)) {
      substitutions <- c(
        time_value = "date",
        time_value = "time",
        time_value = "datetime",
        time_value = "dateTime",
        tmie_value = "date_time",
        time_value = "forecast_date",
        time_value = "target_date",
        time_value = "week",
        time_value = "day",
        time_value = "epiweek",
        time_value = "month",
        time_value = "year",
        time_value = "yearmon",
        time_value = "yearMon",
        time_value = "dates",
        time_value = "time_values",
        time_value = "forecast_dates",
        time_value = "target_dates"
      )
      substitutions <- upcase_snake_case(substitutions)
    }
    strsplit(name_substitutions, "_") %>%
      map(function(name) paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), collapse = "_")) %>%
      unlist()
    x <- tryCatch(x %>% rename(any_of(name_substitutions)),
      error = function(cond) {
        cli_abort("There are multiple `time_value` candidate columns.
Either `rename` some yourself or drop some.")
      }
    )
    cli_inform("inferring `time_value` column.")
  }
  return(x)
}


#' @keywords internal
guess_geo_column_name <- function(x, substitutions = NULL) {
  if (!("time_value" %in% names(x))) {
    substitutions <- substitutions %||% c(
      geo_value = "geo_values",
      geo_value = "geo_id",
      geo_value = "geos",
      geo_value = "location",
      geo_value = "jurisdiction",
      geo_value = "fips",
      geo_value = "zip",
      geo_value = "county",
      geo_value = "hrr",
      geo_value = "msa",
      geo_value = "state",
      geo_value = "province",
      geo_value = "nation",
      geo_value = "states",
      geo_value = "provinces",
      geo_value = "counties"
    )
    substitutions <- upcase_snake_case(substitutions)
    x <- tryCatch(x %>% rename(any_of(substitutions)),
      error = function(cond) {
        cli_abort("There are multiple `geo_value` candidate columns.
Either `rename` some yourself or drop some.")
      }
    )
    cli_inform("inferring `time_value` column.")
  }
  return(x)
}

guess_version_column_name <- function(x, substitutions = NULL) {
  if (!("version" %in% names(x))) {
    if (is.null(substitutions)) {
      substitutions <- c(
        version = "issue",
        version = "release"
      )
      substitutions <- upcase_snake_case(substitutions)
    }
    x <- tryCatch(x %>% rename(any_of(substitutions)),
      error = function(cond) {
        cli_abort("There are multiple `geo_value` candidate columns.
Either `rename` some yourself or drop some.")
      }
    )
    cli_inform("inferring `time_value` column.")
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

#' Use max valid period as guess for `period` of `ref_time_values`
#'
#' @param ref_time_values Vector containing time-interval-like or time-like
#'   data, with at least two distinct values, [`diff`]-able (e.g., a
#'   `time_value` or `version` column), and should have a sensible result from
#'   adding `is.numeric` versions of its `diff` result (via `as.integer` if its
#'   `typeof` is `"integer"`, otherwise via `as.numeric`).
#' @param ref_time_values_arg Optional, string; name to give `ref_time_values`
#'   in error messages. Defaults to quoting the expression the caller fed into
#'   the `ref_time_values` argument.
#' @return `is.numeric`, length 1; attempts to match `typeof(ref_time_values)`
guess_period <- function(ref_time_values, ref_time_values_arg = rlang::caller_arg(ref_time_values)) {
  sorted_distinct_ref_time_values <- sort(unique(ref_time_values))
  if (length(sorted_distinct_ref_time_values) < 2L) {
    cli_abort("Not enough distinct values in {.code {ref_time_values_arg}} to guess the period.", ref_time_values_arg)
  }
  skips <- diff(sorted_distinct_ref_time_values)
  decayed_skips <-
    if (typeof(skips) == "integer") {
      as.integer(skips)
    } else {
      as.numeric(skips)
    }
  gcd_num(decayed_skips)
}
