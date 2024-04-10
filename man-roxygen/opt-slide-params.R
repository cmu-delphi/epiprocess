#' @param col_names A single tidyselection or a tidyselection vector of the
#'  names of one or more columns for which to calculate the rolling mean.
#' @param as_list_col Not supported. Included to match `epi_slide` interface.
#' @param new_col_name Character vector indicating the name(s) of the new
#'  column(s) that will contain the derivative values. Default
#'  is "slide_value"; note that setting `new_col_name` equal to any existing
#'  column names will overwrite those columns. If `names_sep` is `NULL`,
#'  `new_col_name` must be the same length as `col_names`.
