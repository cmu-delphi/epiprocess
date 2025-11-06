
#' List that requires `$`, `$<-`, etc., to provide exact matches to names
#'
#' Does not validate or maintain invariant that field names are unique.
#'
#' @keywords internal
new_struct <- function(fields) {
  assert_list(fields, names = "named")
  vctrs::new_vctr(fields, class = "epiprocess_struct")
}

#' @export
`[.epiprocess_struct` <- function(x, i) {
  vec_slice(x, i, error_call = rlang::current_env())
}

#' @export
`[[.epiprocess_struct` <- function(x, i) {
  .subset2(vec_slice(x, i, error_call = rlang::current_env()), 1L)
}

#' @export
`$.epiprocess_struct` <- function(x, name) {
  .subset2(vec_slice(x, name, error_call = rlang::current_env()), 1L)
}

#' @export
`[<-.epiprocess_struct` <- function(x, i, ..., value) {
  vec_slice(x, i) <- value
  invisible(x)
}

#' @export
`[[<-.epiprocess_struct` <- function(x, i, ..., value) {
  i <- vec_as_location2(i, vec_size(x), names = names(x))
  NextMethod()
}

#' @export
`$<-.epiprocess_struct` <- function(x, name, value) {
  x[[name]] <- value
  invisible(x)
}
