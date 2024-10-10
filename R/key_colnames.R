#' Get names of columns that form a (unique) key associated with an object
#'
#' This is entirely based on metadata and arguments passed; there are no
#' explicit checks that the key actually is unique in any associated data
#' structures.
#'
#' @param x an object, such as an [`epi_df`]
#' @param ... additional arguments passed on to methods
#' @param other_keys character vector; what besides `geo_value` and `time_value`
#'   (if present) should we consider to be key columns?  Used, e.g., if we 
#' @param exclude an optional character vector of key column names to exclude
#'   from the result
#' @return character vector
#' @keywords internal
#' @export
key_colnames <- function(x, ..., exclude = character(0L)) {
  UseMethod("key_colnames")
}

#' @rdname key_colnames
#' @importFrom rlang check_dots_empty0
#' @method key_colnames data.frame
#' @export
key_colnames.data.frame <- function(x, other_keys, ..., exclude = character(0L)) {
  assert_character(other_keys)
  check_dots_empty0(...)
  assert_character(exclude)
  nm <- setdiff(c("geo_value", other_keys, "time_value"), exclude)
  intersect(nm, colnames(x))
}

#' @rdname key_colnames
#' @method key_colnames epi_df
#' @export
key_colnames.epi_df <- function(x, ..., exclude = character(0L)) {
  check_dots_empty0(...)
  assert_character(exclude)
  other_keys <- attr(x, "metadata")$other_keys
  setdiff(c("geo_value", other_keys, "time_value"), exclude)
}

#' @rdname key_colnames
#' @method key_colnames epi_archive
#' @export
key_colnames.epi_archive <- function(x, ..., exclude = character(0L)) {
  check_dots_empty0(...)
  assert_character(exclude)
  setdiff(c("geo_value", x$other_keys, "time_value", "version"), exclude)
}
