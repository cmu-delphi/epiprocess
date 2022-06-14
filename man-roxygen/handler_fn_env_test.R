# See notes on the use of `r ...` dynamic R code in roxygen2 comments
# `utils_conditions.R`. We have to use a bit different code to get this to work
# inside this template tag file, leading to long `r ...` chunks; don't
# wrap/fill/format these, or else current roxygen2 at time of writing will not
# properly process them. Keep the blank lines around them to help prevent
# accidental auto-formatting.

#' @param .fn_env_test optional; a single string, selected from the choices
#'   listed in the nominal default value, with the effective default being the
#'   first choice listed; indicates how [`rlang::fn_env`]s of handler functions
#'   should be tested for equality when comparing handler functions for
#'   equality. Here, `fn_env` is the closure environment for closure functions,
#'   or the base namespace environment for primitive functions. Choosing
#'   `"label"` means to test the [`rlang::env_label`]s of these environments,
#'   intended to be somewhat strict while allowing a package function to remain
#'   equal to "itself" if its formals and body haven't changed; `"identical"`
#'   means to use [`base::identical`] with its defaults (ignoring bytecode and
#'   srcref); `"ignore"` means to ignore the `fn_env`s when comparing functions.
#'   The more lenient comparisons provide some flexibility when working with
#'   handler functions that aren't carefully crafted to work with functions
#'   based on stricter equality tests that

#' `r paste0(if(getRversion()>="4")"["else"","\x60base::globalCallingHandlers\x60",if(getRversion()>="4")"]"else"")`

#'   uses.
