#' @param col_names A character vector OR a
#'  <[`tidy-select`][dplyr_tidy_select]> of the names of one or more columns
#'  for which to calculate a rolling computation. If a tidy-selection, one
#'  or more unquoted expressions separated by commas. Variable names can be
#'  used as if they were positions in the data frame, so expressions like
#'  `x:y` can be used to select a range of variables. The tidy-selection
#'  cannot be used to provide output column names.
#' @param as_list_col Not supported. Included to match `epi_slide` interface.
#' @param new_col_name Character vector indicating the name(s) of the new
#'  column(s) that will contain the derivative values. Default
#'  is "slide_value"; note that setting `new_col_name` equal to any existing
#'  column names will overwrite those columns. If `names_sep` is `NULL`,
#'  `new_col_name` must be the same length as `col_names`.
