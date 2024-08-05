#' @param col_names <[`tidy-select`][dplyr_tidy_select]> An unquoted column
#'   name(e.g., `cases`), multiple column names (e.g., `c(cases, deaths)`),
#'   [other tidy-select expression][tidyselect::language], or a vector of
#'   characters (e.g. `c("cases", "deaths")`). Variable names can be used as if
#'   they were positions in the data frame, so expressions like `x:y` can be
#'   used to select a range of variables. If you have the desired column names
#'   stored in a vector `vars`, use `col_names = all_of(vars)`.
#'
#'   The tidy-selection renaming interface is not supported, and cannot be used
#'   to provide output column names; if you want to customize the output column
#'   names, use [`dplyr::rename`] after the slide.
#' @param as_list_col Not supported. Included to match `epi_slide` interface.
#' @param new_col_name Character vector indicating the name(s) of the new
#'  column(s) that will contain the derivative values. Default
#'  is "slide_value"; note that setting `new_col_name` equal to any existing
#'  column names will overwrite those columns. If `names_sep` is `NULL`,
#'  `new_col_name` must be the same length as `col_names`.
