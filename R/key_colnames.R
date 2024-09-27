#' Grab any keys associated to an epi_df
#'
#' @param x a data.frame, tibble, or epi_df
#' @param ... additional arguments passed on to methods
#' @param other_keys an optional character vector of other keys to include
#' @param exclude an optional character vector of keys to exclude
#' @return If an `epi_df`, this returns all "keys". Otherwise `NULL`.
#' @keywords internal
#' @export
key_colnames <- function(x, ...) {
  UseMethod("key_colnames")
}

#' @rdname key_colnames
#' @method key_colnames default
#' @export
key_colnames.default <- function(x, ...) {
  character(0L)
}

#' @rdname key_colnames
#' @method key_colnames data.frame
#' @export
key_colnames.data.frame <- function(x, other_keys = character(0L), exclude = character(0L), ...) {
  assert_character(other_keys)
  assert_character(exclude)
  nm <- setdiff(c("geo_value", other_keys, "time_value"), exclude)
  intersect(nm, colnames(x))
}

#' @rdname key_colnames
#' @method key_colnames epi_df
#' @export
key_colnames.epi_df <- function(x, exclude = character(0L), ...) {
  assert_character(exclude)
  other_keys <- attr(x, "metadata")$other_keys
  setdiff(c("geo_value", other_keys, "time_value"), exclude)
}

#' @rdname key_colnames
#' @method key_colnames epi_archive
#' @export
key_colnames.epi_archive <- function(x, exclude = character(0L), ...) {
  assert_character(exclude)
  other_keys <- attr(x, "metadata")$other_keys
  setdiff(c("geo_value", other_keys, "time_value"), exclude)
}
