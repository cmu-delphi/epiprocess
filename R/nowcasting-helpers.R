#' Get predictor lag train&test data, factoring in data maturity (revisioning)
#'
#' Background: in many data sources, measurements for the most recent
#'  time values are systematically noisy or even biased.  Bias is
#'  particularly common when working with counts or population rates,
#'  as the first versions of a measurement are often partial counts
#'  that will be revised upward as data for more reporters or
#'  individuals is received.
#'
#' Motivation: simply using [`epix_as_of_latest`] and fitting a model
#' in these cases may produce erratic or biased forecasts.  We'd like
#' for the model to rely less on systematically noisy bleeding-edge
#' data, and to correct for systematically biased data.  So our
#' predictor training data shouldn't be the latest version, but
#' rather, versions that exhibit the same sorts of behavior.
#'
#' For each `version` (a.k.a. forecast date/time), this extracts the
#' value of `varname` where `time_value == version -
#' processed_time_lag` for each `geo_value` (or geo-age combinations,
#' etc. if you have `other_keys`), *as it was reported in version
#' `version`*.  Here, `processed_time_lag` factors in the time step
#' size based on the archive's time type; see `time_lag` below.
#'
#' @param archive An `epi_archive`
#' @param varname String; name of the predictor column you'd like to
#'   lag
#' @param time_lag Length-1 "time delta"; either an integer number of
#'   time steps (with size determined by `archive$time_type`), or a
#'   `difftime` object.
#' @param versions Optional vector of versions (a.k.a. forecast
#'   dates/times) for which you'd like the predictor lags; defaults to
#'   all versions with updates in `archive$DT` plus any versions that
#'   look like they would be part of the normal reporting schedule but
#'   didn't change any values.
#' @param predictor_name Optional string containing no curly braces,
#'   or a [`glue::glue_data`] pattern string; what/how should we name
#'   the lagged version of the predictor in the output?
#' @param nomatch `NA` or `NULL`; what do we do when there is no
#'   measurement available due to reporting latency or being at edges
#'   or gaps of the time series?  `NA` means to include a row with an
#'   `NA` as the lagged predictor value; `NULL` means to not output a
#'   row in these cases.  (Forwarded to [`[.data.table`].)
#' @param drop_time_value Optional Boolean; should we drop the
#'   `time_value` column from the result?  The default, `TRUE`, is
#'   makes merging results for multiple `time_lag`s cleaner (the join
#'   key columns should be based on the geo, other keys, and version
#'   (forecast date/time), not the time value).  You can use `FALSE`
#'   to keep the `time_value` column instead, to check that your
#'   `time_lag` is interpreted properly, or to make certain types of
#'   plots easier.
#' @return tibble with columns `"geo_value"`, any
#'   `archive$other_keys`, `"time_value"` if requested,`"version"`,
#'   and `processed_predictor_name`.
#'
#' @export
epix_realtime_predictor_lag <- function(archive, varname, time_lag,
                                        versions = epix_slide_versions_default(archive),
                                        predictor_name = "{.col}_{.dir}_{.amt}_realtime",
                                        nomatch = NA,
                                        drop_time_value = TRUE) {
  assert_class(archive, "epi_archive")
  time_type <- archive$time_type
  assert_string(varname)
  assert_subset(varname, vctrs::vec_set_difference(colnames(archive$DT), key_colnames(archive)))
  assert_scalar(time_lag)
  time_lag <- time_delta_standardize(time_lag, time_type, "fast")
  versions <- vec_cast_patched(versions, archive$DT$version)
  assert_string(predictor_name)
  predictor_name <- glue::glue_data(list(
    .col = varname,
    .dir = if (time_delta_to_n_steps(time_lag, time_type) >= 0) "lag" else "lead",
    .amt = paste0(time_delta_to_n_steps(time_lag, time_type), time_type_unit_abbr(time_type))
  ), predictor_name)
  assert_logical(drop_time_value, any.missing = FALSE, len = 1L)
  #
  epikey_names <- key_colnames(archive, exclude = c("time_value", "version"))
  epikeys <- unique(archive$DT, by = epikey_names, cols = character())
  requests <- epikeys[, list(version = versions), by = names(epikeys)][
    , time_value := version - ..time_lag
  ]
  result <- archive$DT[
    requests,
    c(key(archive$DT), varname),
    with = FALSE,
    on = key(archive$DT),
    roll = TRUE, nomatch = nomatch,
    allow.cartesian = TRUE # skip unnecessary & misleading check
  ]
  nms <- names(result)
  nms[[match(varname, nms)]] <- predictor_name
  setnames(result, nms)
  setDF(result)
  result <- as_tibble(result)
  if (drop_time_value) {
    result$time_value <- NULL
  }
  result
}
# TODO tidyselect?
# TODO indexing based on a reference_date / reference_time? as in Hub?
# TODO allow for version lag?
