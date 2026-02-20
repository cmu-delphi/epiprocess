#' Is `obj` a (subclass of) "data.frame", "tbl", or anything similar?
#'
#' @keywords internal
obj_is_tblish <- function(x) UseMethod("obj_is_tblish")
#' @export
obj_is_tblish.data.frame <- function(x) TRUE
#' @export
obj_is_tblish.tbl <- function(x) TRUE
#' @export
obj_is_tblish.dtplyr_step <- function(x) TRUE
#' @export
obj_is_tblish.default <- function(x) FALSE



tblish_vars <- dplyr::tbl_vars



tblish_head <- function(x, n) UseMethod("tblish_head")
#' @export
tblish_head.default <- function(x, n) {
  head(x, n)
}



# XXX or tblish_cols? don't want to confuse with tblish_vars but if
# want inset-ish then it'd seem most natural.  Or maybe use var*name*
# & col(s)?
tblish_select <- function(x, names) UseMethod("tblish_select")
#' @export
tblish_select.data.frame <- function(x, names) x[, names]
#' @export
tblish_select.default <- function(x, names) select(x, all_of(names))



tblish_col <- function(x, name) UseMethod("tblish_col")
#' @export
tblish_col.data.frame <- function(x, name) x[[name]]
#' @export
tblish_col.default <- function(x, name) pull(x, !!name)



# XXX also provide a tblish_inset form?
`tblish_col<-` <- function(x, name, value) UseMethod("tblish_col<-")
#' @export
`tblish_col<-.data.frame` <- function(x, name, value) {
  x[[name]] <- value
  x
}
#' @export
`tblish_col<-.default` <- function(x, name, value) {
  dplyr_col_modify(x, `names<-`(list(value), name))
}



#' Cast (via [`vec_cast_patched`]) (a subset of) columns of `x` to required ptypes
#'
#' @param x `tbl`-ish object; see [`obj_is_tblish`]
#' @param cols_to named list of [`vec_ptype`]s; can be list subclass,
#'   such as `vec_ptype` of a tibble
#'
#' @return `x`, with columns
#'
#' @keywords internal
tblish_cast_cols <- function(x, cols_to) UseMethod("tblish_cast_cols")

#' @export
tblish_cast_cols.default <- function(x, cols_to) {
  assert_true(obj_is_tblish(x))
  x_head <- tblish_head(x, 0L) # avoid collecting huge results into memory
  for (colname in names(cols_to)) {
    col_to <- cols_to[[colname]]
    x <- tblish_inset2(x, colname, vec_cast_patched(x[[colname]], col_to))
  }
  x
}
