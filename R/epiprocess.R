#' epiprocess: Tools for basic signal processing in epidemiology
#'
#' This package introduces a common data structure for epidemiological data sets
#' measured over space and time, and offers associated utilities to perform
#' basic signal processing tasks.
#'
#' @importFrom checkmate assert assert_character assert_int anyInfinite
#' @name epiprocess
"_PACKAGE"
utils::globalVariables(c(".x", ".group_key", ".ref_time_value"))
