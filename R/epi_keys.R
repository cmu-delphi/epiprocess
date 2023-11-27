#' Grab any keys associated to an epi_df
#'
#' @param x a data.frame, tibble, or epi_df
#' @param ... additional arguments passed on to methods
#'
#' @return If an `epi_df`, this returns all "keys". Otherwise `NULL`
#' @keywords internal
#' @export
epi_keys <- function(x, ...) {
  UseMethod("epi_keys")
}

#' @export
epi_keys.default <- function(x, ...) {
  character(0L)
}

#' @export
epi_keys.data.frame <- function(x, other_keys = character(0L), ...) {
  arg_is_chr(other_keys, allow_empty = TRUE)
  nm <- c("time_value", "geo_value", other_keys)
  intersect(nm, names(x))
}

#' @export
epi_keys.epi_df <- function(x, ...) {
  c("time_value", "geo_value", attr(x, "metadata")$other_keys)
}

#' @export
epi_keys.epi_archive <- function(x, ...) {
  c("time_value", "geo_value", attr(x, "metadata")$other_keys)
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(epi_keys(x, ...))
}
