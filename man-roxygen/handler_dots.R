#' @param ... named calling handlers: each a function that takes a condition
#'   object as an argument and performs some handling (such as printing or
#'   recording a stack trace), passed by name, where the name associates the
#'   handler with a class of conditions it should be called on (e.g.,
#'   `"condition"` as a catch-all, `"error"`, `"warning"`, `"message"`, or more
#'   specific classes provided by some packages)
