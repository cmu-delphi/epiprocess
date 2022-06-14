#' @param message character vector; an `rlang` non-`cli`-format message; this
#'   will be line-wrapped and augmented with information about
#'   `display_subfields` and how to access any `more_subfields`
#' @param class_suffix string or NULL; if non-NULL, and we are in a package,
#'   then `"<pkgname>__<class_suffix>"` will be tacked on as an S3 class of the
#'   condition object; if we are not in a package, then the suffix (without any
#'   prefix) will be added as an S3 class. Additionally, regardless of whether
#'   `class_suffix` is `NULL`, if we are in a package, two additional S3 classes
#'   will be added as well: `"<pkgname>__<conditiontype>"` and
#'   `"<pkgname>__condition"`
#' @param display_subfields list of objects to display as part of the condition
#'   message using [`utils::str`] and save in the condition object
#' @param more_subfields list of objects to save in the condition object but
#'   only point to, not display, in the condition message
#' @param call environment; the calling environment from which the trace should
#'   be produced
NULL
