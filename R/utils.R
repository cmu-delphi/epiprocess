break_str = function(str, nchar = 79, init = "") {
  str = paste(strwrap(str, nchar, initial = init), collapse = "\n")
  str[1] = substring(str, nchar(init)+1)
  return(str)
}

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
wrap_symbolics = function(symbolics,
                          initial = "", common_prefix = "", none_str = "<none>",
                          width = getOption("width", 80L)) {
  if (!all(purrr::map_lgl(symbolics, rlang::is_symbolic))) {
    Abort("`symbolics` must be a list of symbolic objects")
  }
  if (!rlang::is_string(initial)) {
    Abort("`initial` must be a string")
  }
  if (!rlang::is_string(common_prefix)) {
    Abort("`common_prefix` must be a string")
  }
  if (!rlang::is_string(none_str)) {
    Abort("`none_str` must be a string")
  }
  prefix = strrep(" ", nchar(initial, type="width"))
  full_initial = paste0(common_prefix, initial)
  full_prefix = paste0(common_prefix, prefix)
  full_initial_width = nchar(full_initial, type="width")
  minimum_reasonable_line_width_for_syms = 20L
  line_width_for_syms = max(width - full_initial_width,
                            minimum_reasonable_line_width_for_syms)
  unprefixed_lines =
    if (length(symbolics) == 0L) {
      none_str
    } else {
      utils::capture.output(
        withr::with_options(list("width" = line_width_for_syms), {
          # `paste0` already takes care of necessary backquotes. `cat` with
          # `fill=TRUE` takes care of spacing + line wrapping exclusively
          # between elements. We need to add commas appropriately.
          cat(paste0(symbolics, c(rep(",", times=length(symbolics)-1L), "")), fill=TRUE)
        })
      )
    }
  lines = paste0(c(full_initial, rep(full_prefix, times=length(unprefixed_lines)-1L)),
                 unprefixed_lines)
  lines
}

#' Line wrap `chr` holding variable/column/other names, with prefix&indent
#'
#' @param nms Character vector: the variable names (potentially empty)
#' @inheritParams wrap_symbolics
#' @return `chr`; to print, use [`base::writeLines`].
#'
#' @noRd
wrap_varnames = function(nms,
                         initial = "", common_prefix = "", none_str = "<none>",
                         width = getOption("width", 80L)) {
  # (Repeating parameter names and default args here for better autocomplete.
  # Using `...` instead would require less upkeep, but have worse autocomplete.)
  if (!rlang::is_character(nms)) {
    Abort("`nms` must be a character vector")
  }
  wrap_symbolics(rlang::syms(nms), initial=initial, common_prefix=common_prefix, none_str=none_str, width=width)
}

#' Paste `chr` entries (lines) together with `"\n"` separators, trailing `"\n"`
#'
#' @param lines `chr`
#' @return string
#'
#' @noRd
paste_lines = function(lines) {
  paste(paste0(lines,"\n"), collapse="")
}

Abort = function(msg, ...) rlang::abort(break_str(msg, init = "Error: "), ...)
Warn = function(msg, ...) rlang::warn(break_str(msg, init = "Warning: "), ...)

#' Check that a sliding computation function takes enough args
#'
#' @param f Function; specifies a computation to slide over an `epi_df` or
#'  `epi_archive` in `epi_slide` or `epix_slide`.
#'
#' @noRd
check_sufficient_f_args <- function(f) {
  n_mandatory_f_args <- 2
  arg_names = names(formals(args(f)))
  if ("..." %in% arg_names) {
    # Keep all arg names before `...`
    dots_i <- which(arg_names == "...")
    arg_names <- arg_names[seq_len(dots_i - 1)]

    if (length(arg_names) < n_mandatory_f_args) {
      Warn(sprintf("`f` only takes %s positional arguments before the `...` args, but %s were expected; this can lead to obtuse errors downstream", length(arg_names), n_mandatory_f_args),
          class="check_sufficient_f_args__f_needs_min_args_before_dots",
          epiprocess__f = f,
          epiprocess__arg_names = arg_names)
    }
  } else {
    if (length(arg_names) < n_mandatory_f_args) {
      Abort(sprintf("`f` must take at least %s arguments", n_mandatory_f_args),
          class="check_sufficient_f_args__f_needs_min_args",
          epiprocess__f = f,
          epiprocess__arg_names = arg_names)
    }
  }
}

##########

in_range = function(x, rng) pmin(pmax(x, rng[1]), rng[2])

##########

Min = function(x) min(x, na.rm = TRUE)
Max = function(x) max(x, na.rm = TRUE)
Sum = function(x) sum(x, na.rm = TRUE)
Mean = function(x) mean(x, na.rm = TRUE)
Median = function(x) median(x, na.rm = TRUE)

##########

Start = function(x) x[1]
End = function(x) x[length(x)]
MiddleL = function(x) x[floor((length(x)+1)/2)]
MiddleR = function(x) x[ceiling((length(x)+1)/2)]
ExtendL = function(x) c(Start(x), x)
ExtendR = function(x) c(x, End(x))

guess_geo_type = function(geo_value) {
  if (is.character(geo_value)) {
    # Convert geo values to lowercase
    geo_value = tolower(geo_value)
      
    # If all geo values are state abbreviations, then use "state" 
    state_values = c(tolower(datasets::state.abb), 
                     "as", "dc", "gu", "mp", "pr", "vi")
    if (all(geo_value %in% state_values)) return("state")

    # Else if all geo values are 2 letters, then use "nation"
    else if (all(grepl("[a-z]{2}", geo_value))
             & !any(grepl("[a-z]{3}", geo_value))) return("nation")

    # Else if all geo values are 5 numbers, then use "county"
    else if (all(grepl("[0-9]{5}", geo_value)) &
             !any(grepl("[0-9]{6}", geo_value))) return("county")
  }

  else if (is.numeric(geo_value)) {
    # Convert geo values to integers
    geo_value = as.integer(geo_value)

    # If the max geo value is at most 10, then use "hhs"
    if (max(geo_value) <= 10) return("hhs")
      
    # Else if the max geo value is at most 457, then use "hrr"
    if (max(geo_value) <= 457) return("hrr")
  }

  # If we got here then we failed
  return("custom")
}

guess_time_type = function(time_value) {
  # Convert character time values to Date or POSIXct
  if (is.character(time_value)) {
    if (nchar(time_value[1]) <= "10") {
      new_time_value = tryCatch({ as.Date(time_value) },
                                error = function(e) NULL)
    }
    else {
      new_time_value = tryCatch({ as.POSIXct(time_value) },
                                error = function(e) NULL)
    }
    if (!is.null(new_time_value)) time_value = new_time_value
  }
    
  # Now, if a POSIXct class, then use "day-time"
  if (inherits(time_value, "POSIXct")) return("day-time")

  # Else, if a Date class, then use "week" or "day" depending on gaps 
  else if (inherits(time_value, "Date")) {
    return(ifelse(all(diff(sort(time_value)) == 7), "week", "day"))
  }

  # Else, check whether it's one of the tsibble classes
  else if (inherits(time_value, "yearweek")) return("yearweek")
  else if (inherits(time_value, "yearmonth")) return("yearmonth")
  else if (inherits(time_value, "yearquarter")) return("yearquarter")

  # Else, if it's an integer that's at least 1582, then use "year"
  if (is.numeric(time_value) &&
      all(time_value == as.integer(time_value)) &&
      all(time_value >= 1582)) {
    return("year")
  }
      
  # If we got here then we failed
  return("custom")
}

##########


quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

##########

# Create an auto-named list
enlist = function(...) {
  x = list(...)
  n = as.character(sys.call())[-1]
  if (!is.null(n0 <- names(x))) {
    n[n0 != ""] = n0[n0 != ""]
  }
  names(x) = n
  return(x) 
}

# Variable assignment from a list. NOT USED. Something is broken, this doesn't 
# seem to work completely as expected: the variables it define don't propogate
# down to child environments  
list2var = function(x) {
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
#' fn = function(x = deprecated()) {
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
#' wrapper1 = function(x=deprecated()) fn({{x}})
#' wrapper2 = function(x=lifecycle::deprecated()) fn({{x}})
#' wrapper3 = function(x) fn({{x}})
#' wrapper4 = function(x) fn(!!rlang::enquo(x))
#'
#' wrapper1() # FALSE
#' wrapper2() # FALSE
#' wrapper3() # FALSE
#' wrapper4() # FALSE
#'
#' # More advanced: wrapper that receives an already-enquosed arg:
#'
#' inner_wrapper = function(quo) fn(!!quo)
#' outer_wrapper1 = function(x=deprecated()) inner_wrapper(rlang::enquo(x))
#'
#' outer_wrapper1() # FALSE
#'
#' # Improper argument forwarding from a wrapper function will cause this
#' # function to produce incorrect results.
#' bad_wrapper1 = function(x) fn(x)
#' bad_wrapper1() # TRUE, bad
#'
#' @noRd
deprecated_quo_is_present = function(quo) {
  if (!rlang::is_quosure(quo)) {
    Abort("`quo` must be a quosure; `enquo` the arg first",
          internal=TRUE)
  } else if (rlang::quo_is_missing(quo)) {
    FALSE
  } else {
    quo_expr = rlang::get_expr(quo)
    if (identical(quo_expr, rlang::expr(deprecated())) ||
          identical(quo_expr, rlang::expr(lifecycle::deprecated()))) {
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
gcd2num = function(a, b, rrtol=1e-6, pqlim=1e6, irtol=1e-6) {
  if (!is.numeric(a) || length(a) != 1L) {
    Abort("`a` must satisfy `is.numeric`, have `length` 1.")
  }
  if (!is.numeric(b) || length(b) != 1L) {
    Abort("`b` must satisfy `is.numeric`, have `length` 1.")
  }
  if (!is.numeric(rrtol) || length(rrtol) != 1L || rrtol < 0) {
    Abort("`rrtol` must satisfy `is.numeric`, have `length` 1, and be non-negative.")
  }
  if (!is.numeric(pqlim) || length(pqlim) != 1L || pqlim < 0) {
    Abort("`pqlim` must satisfy `is.numeric`, have `length` 1, and be non-negative.")
  }
  if (!is.numeric(irtol) || length(irtol) != 1L || irtol < 0) {
    Abort("`irtol` must satisfy `is.numeric`, have `length` 1, and be non-negative.")
  }
  if (is.na(a) || is.na(b) || a == 0 || b == 0 || abs(a/b) >= pqlim || abs(b/a) >= pqlim) {
    Abort("`a` and/or `b` is either `NA` or exactly zero, or one is so much smaller than the other that it looks like it's supposed to be zero; see `pqlim` setting.")
  }
  iatol = irtol * max(a,b)
  a_curr = a
  b_curr = b
  while (TRUE) {
    # `b_curr` is the candidate GCD / iterand; check first if it seems too small:
    if (abs(b_curr) <= iatol) {
      Abort('No GCD found; remaining potential Gads are all too small relative to one/both of the original inputs; see `irtol` setting.')
    }
    remainder = a_curr - round(a_curr / b_curr) * b_curr
    if (abs(remainder / b_curr) <= rrtol) {
      # We consider `a_curr` divisible by `b_curr`; `b_curr` is the GCD or its negation
      return (abs(b_curr))
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
gcd_num = function(dividends, ..., rrtol=1e-6, pqlim=1e6, irtol=1e-6) {
  if (!is.numeric(dividends) || length(dividends) == 0L) {
    Abort("`dividends` must satisfy `is.numeric`, and have `length` > 0")
  }
  if (rlang::dots_n(...) != 0L) {
    Abort("`...` should be empty; all dividends should go in a single `dividends` vector, and all tolerance&limit settings should be passed by name.")
  }
  # We expect a bunch of duplicate `dividends` for some applications.
  # De-duplicate to reduce work. Sort by absolute value to attempt to reduce
  # workload. Also take `abs` early on as another form of deduplication and to
  # make the sort simpler. Use `na.last=FALSE` in the sort to preserve presence
  # of `NA`s in order to get a better error message in this case.
  optimized_dividends = sort(unique(abs(dividends)), na.last=FALSE)
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
  numeric_gcd = purrr::reduce(optimized_dividends, gcd2num,
                              rrtol=rrtol, pqlim=pqlim, irtol=irtol)
  vctrs::vec_cast(numeric_gcd, dividends)
}

#' Use max valid period as guess for `period` of `ref_time_values`
#'
#' @param `ref_time_values` Vector containing time-interval-like or time-like
#'   data, with at least two distinct values, [`diff`]-able (e.g., a
#'   `time_value` or `version` column), and should have a sensible result from
#'   adding `is.numeric` versions of its `diff` result (via `as.integer` if its
#'   `typeof` is `"integer"`, otherwise via `as.numeric`).
#' @param `ref_time_values_arg` Optional, string; name to give `ref_time_values`
#'   in error messages. Defaults to quoting the expression the caller fed into
#'   the `ref_time_values` argument.
#' @return `is.numeric`, length 1; attempts to match `typeof(ref_time_values)`
guess_period = function(ref_time_values, ref_time_values_arg = rlang::caller_arg(ref_time_values)) {
  sorted_distinct_ref_time_values = sort(unique(ref_time_values))
  if (length(sorted_distinct_ref_time_values) < 2L) {
    Abort(sprintf("Not enough distinct values in `%s` to guess the period.", ref_time_values_arg))
  }
  skips = diff(sorted_distinct_ref_time_values)
  decayed_skips =
    if (typeof(skips) == "integer") {
      as.integer(skips)
    } else {
      as.numeric(skips)
    }
  gcd_num(decayed_skips)
}
