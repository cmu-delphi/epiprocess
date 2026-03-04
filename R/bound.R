
# Experimental class for representing one-sided bounds, marking them
# inclusive/exclusive.  This uses a list wrapper class to try to
# prevent auto-inheritance of numerical methods which may give invalid
# threshold results or drop the class.

# XXX make all bound parameters throughout the package accept these
# classes before exporting.

# XXX if we want to implement some mathematical operations on this,
# we'll need to constrain it to be either an upper bound or a lower
# bound.

#' Make a new epiprocess_bound wrapper object, with no input validation
#'
#' @keywords internal
new_bound0 <- function(threshold, inclusive) {
  result <- new_struct(list(threshold = threshold, inclusive = inclusive))
  class(result) <- c("epiprocess_bound", class(result))
  result
}

#' Mark a threshold as an inclusive bound using a wrapper class
#'
#' @keywords internal
inclusive <- function(x) {
  if (inherits(x, "epiprocess_bound")) {
    cli_abort("`x` must not already be marked inclusive/exclusive")
  }
  new_bound0(x, TRUE)
}

#' Mark a threshold as an exclusive bound using a wrapper class
#'
#' @keywords internal
exclusive <- function(x) {
  if (inherits(x, "epiprocess_bound")) {
    cli_abort("`x` must not already be marked inclusive/exclusive")
  }
  new_bound0(x, FALSE)
}

as_inclusive_if_not_bound <- function(x) {
  if (inherits(x, "epiprocess_bound")) {
    x
  } else {
    new_bound0(x, TRUE)
  }
}

as_exclusive_if_not_bound <- function(x) {
  if (inherits(x, "epiprocess_bound")) {
    x
  } else {
    new_bound0(x, FALSE)
  }
}
