# Helpers here are meant to be used inside inline R expressions within roxygen2
# documentation when @template is inappropriate.

#' Description of a single arg that tidyselects value variables
#'
#' Not meant for when describing tidyselect `...`.
#'
#' @keywords internal
tidyselect_arg_roxygen <- '
  <[`tidy-select`][dplyr_tidy_select]> An unquoted column
  name (e.g., `cases`), multiple column names (e.g., `c(cases, deaths)`),
  [other tidy-select expression][tidyselect::language], or a vector of
  characters (e.g. `c("cases", "deaths")`). Variable names can be used as if
  they were positions in the data frame, so expressions like `x:y` can be
  used to select a range of variables.
'
