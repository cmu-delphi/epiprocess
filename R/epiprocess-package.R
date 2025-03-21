#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import epidatasets
#' @importFrom checkmate anyInfinite anyMissing assert assert_character
#' @importFrom checkmate assert_class assert_data_frame assert_int assert_list
#' @importFrom checkmate assert_false
#' @importFrom checkmate assert_logical assert_numeric assert_scalar checkInt
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @importFrom checkmate assert_tibble
#' @importFrom checkmate check_atomic check_data_frame expect_class test_int
#' @importFrom checkmate check_names
#' @importFrom checkmate test_subset test_set_equal vname
#' @importFrom cli cli_abort cli_warn
#' @importFrom cli pluralize
#' @importFrom cli qty
#' @importFrom data.table as.data.table
#' @importFrom data.table key
#' @importFrom data.table setkeyv
#' @importFrom dplyr arrange
#' @importFrom dplyr grouped_df
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr select
#' @importFrom lifecycle deprecated
#' @importFrom purrr list_rbind
#' @importFrom rlang %||%
#' @importFrom rlang is_bare_integerish
#' @importFrom tools toTitleCase
#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_data
#' @importFrom vctrs vec_equal
#' @importFrom vctrs vec_in
#' @importFrom vctrs vec_order
#' @importFrom vctrs vec_rbind
#' @importFrom vctrs vec_recycle_common
#' @importFrom vctrs vec_rep
#' @importFrom vctrs vec_slice
#' @importFrom vctrs vec_slice<-
#' @importFrom vctrs vec_sort
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", ".x", ".group_key", ".ref_time_value", "resid",
  "fitted", ".response", "geo_value", "time_value",
  "value", ".real", "lag", "max_value", "min_value",
  "median_value", "spread", "rel_spread", "lag_to",
  "lag_near_latest", "n_revisions", "min_lag", "max_lag"
))
