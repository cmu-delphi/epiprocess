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
