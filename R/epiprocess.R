#' epiprocess: Tools for basic signal processing in epidemiology
#'
#' This package introduces a common data structure for epidemiological data sets
#' measured over space and time, and offers associated utilities to perform
#' basic signal processing tasks.
#'
#' @importFrom checkmate assert assert_scalar assert_data_frame anyMissing
#'             assert_logical assert_list assert_character assert_class
#'             assert_int assert_numeric check_data_frame vname check_atomic
#'             anyInfinite test_subset test_set_equal checkInt
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom rlang %||%
#' @name epiprocess
"_PACKAGE"
utils::globalVariables(c(".x", ".group_key", ".ref_time_value"))
