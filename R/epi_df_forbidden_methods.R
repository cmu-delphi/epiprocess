# Methods in this file are used to
# * Disable problematic inherited behavior (e.g., mean on epi_dfs)
# * Provide better error messaging if possible for things that already abort
#   when they should (e.g., sum on epi_dfs)


# Disable mean on epi_dfs, to prevent `epi_slide(~ mean(.x), ....)` bad output:

#' @export
mean.epi_df <- function(x, ...) {
  cli_abort(c(
    "`mean` shouldn't be used on entire `epi_df`s",
    "x" = "{rlang::caller_arg(x)} was an `epi_df`",
    "i" = "If you encountered this while trying to take a rolling mean
           of a column using `epi_slide`, you probably forgot to
           specify the column name (e.g., ~ mean(.x$colname)). You may
           also prefer to use the specialized `epi_slide_mean` method."
  ), class = "epiprocess__summarizer_on_entire_epi_df")
}

# Similarly, provide better error messages for some other potentially-common
# slide operations (sum, prod, min, max, all, any, range):

#' @export
Summary.epi_df <- function(..., na.rm = FALSE) {
  # cli uses dot prefixes for special purpose; use alias to avoid confusion during interpolation
  generic <- .Generic
  opt_pointer <- switch(.Generic,
    sum = "You may also prefer to use the specialized `epi_slide_sum` method.",
    prod = ,
    min = ,
    max = ,
    all = ,
    any = "You may also prefer to use the specialized `epi_slide_opt` method.",
    range = "",
    cli_abort("Unrecognized .Generic: {generic}")
  )
  cli_abort(c(
    "`{generic}` shouldn't be used on entire `epi_df`s",
    # We'd like to quote user input in the error message, but `caller_arg(..1)` is
    # just "..1" and (eagerness/S4/unnamedness?) issues thwart some alternatives; just
    # use something generic:
    "x" = "`{generic}`'s first argument was an `epi_df`",
    "i" = "If you encountered this while trying to take a rolling {generic}
           of a column using `epi_slide`, you probably forgot to
           specify the column name (e.g., ~ {generic}(.x$colname)). {opt_pointer}"
  ), class = "epiprocess__summarizer_on_entire_epi_df")
}
