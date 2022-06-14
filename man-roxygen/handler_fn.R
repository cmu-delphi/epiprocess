# See notes on the use of `r ...` dynamic R code in roxygen2 comments
# `utils_conditions.R`. We have to use a bit different code to get this to work
# inside this template tag file, leading to long `r ...` chunks; don't
# wrap/fill/format these, or else current roxygen2 at time of writing will not
# properly process them. Keep the blank lines around them to help prevent
# accidental auto-formatting.

#' @description
#'
#' This is a very simple function, but simply repeating its definition
#' everywhere of interest prevents

#' `r paste0(if(getRversion()>="4")"["else"","\x60base::globalCallingHandlers\x60",if(getRversion()>="4")"]"else"")`

#' and straightforward handler-related functions from recognizing these repeated
#' redefinitions as the same things (due to differing bytecode, srcrefs, closure
#' environments, and/or other things). This function provides a consistent
#' identity for this type of handler, and attempts to maintain this identity
#' even across package reloads (by setting its closure environment to a
#' something that should always be available and never reloaded; to match
#' `rlang:::hnd_entrace`, this is [`base::baseenv()`]; this means that things
#' not available from `base` require use of `<pkg>::` to access).
#'
#' @param cnd a condition object (e.g., an error, warning, or message)
#'
#' @seealso [`base::withCallingHandlers`] to tie this handler to a condition
#'   subclass for a block of code, or

#' `r paste0(if(getRversion()>="4")"["else"","\x60base::globalCallingHandlers\x60",if(getRversion()>="4")"]"else"")`,

#'   [`global_handlers_push`], [`global_handlers_remove`],
#'   [`global_handlers_list`] to enable/disable/list these associations globally
