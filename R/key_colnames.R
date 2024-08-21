#' Grab any keys associated to an epi_df
#'
#' @param x a data.frame, tibble, or epi_df
#' @param ... additional arguments passed on to methods
#'
#' @return If an `epi_df`, this returns all "keys". Otherwise `NULL`
#' @keywords internal
#' @export
key_colnames <- function(x, ...) {
  UseMethod("key_colnames")
}

#' @export
key_colnames.default <- function(x, ...) {
  character(0L)
}

#' @export
key_colnames.data.frame <- function(x, other_keys = character(0L), ...) {
  assert_character(other_keys)
  nm <- c("geo_value", "time_value", other_keys)
  intersect(nm, colnames(x))
}

#' @export
key_colnames.epi_df <- function(x, ...) {
  other_keys <- attr(x, "metadata")$other_keys
  c("geo_value", "time_value", other_keys)
}

#' @export
key_colnames.epi_archive <- function(x, ...) {
  other_keys <- attr(x, "metadata")$other_keys
  c("geo_value", "time_value", other_keys)
}

kill_time_value <- function(v) {
  assert_character(v)
  v[v != "time_value"]
}
