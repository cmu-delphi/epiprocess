
#' List that requires `$`, `$<-`, etc., to provide exact matches to names
#'
#' Does not validate or maintain invariant that field names are unique.
#'
#' @keywords internal
new_struct <- function(fields) {
  assert_list(fields, names = "named")
  vctrs::new_vctr(fields, class = "epiprocess_struct")
}

# NOTE: currently implemented atop vctrs_vctr for convenience and
# its fast native checks.

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

# XXX `vctrs` methods below also block assigning to new names, but
# with suboptimal error messages about "extract"ing.

#' @export
`[<-.epiprocess_struct` <- function(x, i, ..., value) {
  vec_slice(x, i) <- value
  invisible(x)
}

#' @export
`[[<-.epiprocess_struct` <- function(x, i, ..., value) {
  i <- vec_as_location2(i, vec_size(x), names = names(x))
  # XXX ^ provides check for field existence, but with suboptimal
  # error message about "extract"ing rather than assigning
  NextMethod()
}

#' @export
`$<-.epiprocess_struct` <- function(x, name, value) {
  x[[name]] <- value
  invisible(x)
}

# XXX `vec_c`, `c(epiprocess_struct, *)` allow field duplication;
# check for in `vec_restore` and balk?

# XXX todo self-self vctrs impls

# XXX consider subclasses and vec_ptype2 etc. logic.  Defaults may
# actually be fine, allowing adding fields for base class but balking
# at attempts to do this on subclasses, which are more likely to be
# closed.  Auto-decaying to base struct class would be another option,
# but probably not the preferred one, plus would be messy to implement
# with vctrs non-S3 native internal dispatch methods.

# XXX `print` header including length is a bit weird.  May also want a
# vertical format
