#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import epidatasets
#' @importFrom checkmate anyInfinite anyMissing assert assert_character
#' @importFrom checkmate assert_class assert_data_frame assert_int assert_list
#' @importFrom checkmate assert_logical assert_numeric assert_scalar checkInt
#' @importFrom checkmate check_atomic check_data_frame expect_class test_int
#' @importFrom checkmate check_names
#' @importFrom checkmate test_subset test_set_equal vname
#' @importFrom cli cli_abort cli_warn
#' @importFrom data.table as.data.table
#' @importFrom data.table key
#' @importFrom data.table setkeyv
#' @importFrom dplyr select
#' @importFrom lifecycle deprecated
#' @importFrom rlang %||%
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".x", ".group_key", ".ref_time_value", "resid",
  "fitted", ".response", "geo_value", "time_value",
  "value", ".real", "lag", "max_value", "min_value",
  "median_value", "spread", "rel_spread", "time_to",
  "time_near_latest", "n_revisions"
))
