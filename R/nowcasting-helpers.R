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
#' processed_relative_time` for each `geo_value` (or geo-age combinations,
#' etc. if you have `other_keys`), *as it was reported in version
#' `version`*.  Here, `processed_relative_time` factors in the time step
#' size based on the archive's time type; see `relative_time` below.
#'
#' @param archive An `epi_archive`
#' @param varname String; name of the predictor column you'd like to
#'   lag
#' @param relative_time Length-1 "time delta"; either an integer
#'   number of time periods (with size determined by
#'   `archive$time_type`), or a `difftime` object.  How do we move
#'   from each "anchor version" (i.e., hypothetical forecast
#'   date/etc.) to the desired `time_value`.  Typically this will be
#'   negative; e.g., in daily data, to examine each report's
#'   measurements for the previous day, it'd be -1.
#' @param anchor_versions Optional vector of versions (a.k.a. forecast
#'   dates/times) for which you'd like the predictor lags; defaults to
#'   all versions with updates in `archive$DT` plus any versions that
#'   look like they would be part of the normal reporting schedule but
#'   didn't change any values.
#' @param out_name Optional [`glue::glue_data`] pattern string;
#'   what/how should we name the lagged predictor in the output?  It
#'   can just be the output column name desired, provided it contains
#'   no curly braces. Or you can use `glue` features and the following
#'   shorthand: "\{.col\}" for `varname`, "\{.dir\}" for "lag" or
#'   "lead", and "\{.amt\}" for a formatted absolute value of
#'   `relative_time`.  Default is
#'   "`r gsub("([{}])", "\\\\\\1", rlang::fn_fmls(epix_realtime_predictor_lag)$out_name)`".
#' @param nomatch `NA` or `NULL`; what do we do when there is no
#'   measurement available due to reporting latency or being at edges
#'   or gaps of the time series?  `NA` means to include a row with an
#'   `NA` as the lagged predictor value; `NULL` means to not output a
#'   row in these cases.  (Forwarded to [`[.data.table`].)
#' @param drop_time_value Optional Boolean; should we drop the
#'   `time_value` column from the result?  The default, `TRUE`, is
#'   makes merging results for multiple `relative_time`s cleaner (the
#'   join key columns should be based on the geo, other keys, and
#'   version (forecast date/time), not the time value).  You can use
#'   `FALSE` to keep the `time_value` column instead, to check that
#'   your `relative_time` is interpreted properly, or to make certain
#'   types of plots easier.
#' @return tibble with columns `"geo_value"`, any
#'   `archive$other_keys`, `"time_value"` if requested,`"version"`,
#'   and `processed_out_name`.
#'
#' @examples
#' # Basic usage: for each report, what did it say about the day 3
#' # days prior?:
#' archive_cases_dv_subset %>%
#'   epix_realtime_predictor_lag("percent_cli", -3)
#'
#' # Compare against our best idea of what the truth was for those
#' # days by combining with `epix_evaluation_target_data`:
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' inner_join(
#'   archive_cases_dv_subset %>%
#'     epix_realtime_predictor_lag("percent_cli", -3),
#'   archive_cases_dv_subset %>%
#'     epix_evaluation_target_data("percent_cli", -3, time_until_semistable = 60),
#'   by = c("geo_value", "anchor_version")
#' ) %>%
#'   pivot_longer(!c(geo_value, anchor_version)) %>%
#'   ggplot(aes(anchor_version, value, colour = name)) +
#'   geom_line() +
#'   facet_wrap(~ geo_value)
#' # From this plot, we see that (i) we don't always have a
#' # `percent_cli` measurement available for "3 days ago", and (ii)
#' # when we do, it's somewhat close to the finalized values, but
#' # since it's not negligible compared to the day-to-day variations
#' # in the target, we should consider revision-aware forecasting
#' # methods when using it.
#'
#' full_join(
#'   archive_cases_dv_subset %>%
#'     epix_realtime_predictor_lag("percent_cli", -7, drop_time_value = FALSE),
#'   archive_cases_dv_subset %>%
#'     epix_as_of_latest(),
#'   by = c("geo_value", "time_value")
#' ) %>%
#'   ggplot(aes(time_value, percent_cli)) +
#'   geom_line(na.rm = TRUE) +
#'   geom_line(aes(y = percent_cli_lag_7d_realtime), colour = "blue", na.rm = TRUE) +
#'   facet_wrap(~ geo_value)
#'
#' library(purrr)
#' map(-7*(1:8), function(relative_time) {
#'   archive_cases_dv_subset %>%
#'     epix_realtime_predictor_lag("percent_cli", relative_time, out_name = "value") %>%
#'     mutate(relative_time = .env$relative_time)
#' }) %>%
#'   bind_rows() %>%
#'   inner_join(
#'     archive_cases_dv_subset %>%
#'       epix_as_of_latest() %>%
#'       mutate(anchor_version = time_value - 4, .keep = "unused"),
#'     by = c("geo_value", "anchor_version")
#'   ) %>%
#'   # TODO via binding instead? suggesting a long format option?
#'   drop_na(value, percent_cli) %>%
#'   filter(.by = c(geo_value, anchor_version), n() == 8) %>%
#'   summarize(.by = relative_time, MAE = mean(abs(value - percent_cli))) %>%
#'   ggplot(aes(relative_time, MAE)) +
#'   geom_line()
#'
#' @export
epix_realtime_predictor_lag <- function(archive, varname, relative_time,
                                        anchor_versions = epix_slide_versions_default(archive),
                                        out_name = "{.col}_{.dir}_{.amt}_realtime",
                                        nomatch = NA,
                                        drop_time_value = TRUE) {
  assert_class(archive, "epi_archive")
  time_type <- archive$time_type
  assert_string(varname)
  assert_subset(varname, vctrs::vec_set_difference(colnames(archive$DT), key_colnames(archive)))
  assert_scalar(relative_time)
  relative_time <- time_delta_standardize(relative_time, time_type, "fast")
  anchor_versions <- vec_cast_patched(anchor_versions, archive$DT$version)
  assert_string(out_name)
  out_name <- glue::glue_data(list(
    .col = varname,
    .dir = if (time_delta_to_n_steps(relative_time, time_type) <= 0) "lag" else "lead",
    .amt = paste0(abs(time_delta_to_n_steps(relative_time, time_type)), time_type_unit_abbr(time_type))
  ), out_name)
  assert_logical(drop_time_value, any.missing = FALSE, len = 1L)
  #
  epikey_names <- key_colnames(archive, exclude = c("time_value", "version"))
  epikeys <- unique(archive$DT, by = epikey_names, cols = character())
  requests <- epikeys[, list(version = anchor_versions), by = names(epikeys)][
  , time_value := version + ..relative_time
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
  nms[[match(varname, nms)]] <- out_name
  setnames(result, nms)
  setDF(result)
  result <- as_tibble(result)
  if (drop_time_value) {
    result$time_value <- NULL
  }
  result <- rename(result, anchor_version = version)
  result
}
# TODO tidyselect?
# TODO indexing based on a reference_date / reference_time? as in Hub?
# TODO allow for version lag?
# TODO vs. label_cols subset of time, ver, lag
# TODO vs. long and wide formats?
# TODO standardize to a relative_time arg to make target fetching and predictor fetching match?

epix_evaluation_target_data <- function(archive, varname, relative_time,
                                        time_until_semistable,
                                        anchor_versions = epix_slide_versions_default(archive),
                                        out_name = "{.col}_{.dir}_{.amt}_evaluation",
                                        nomatch = NA) {
  assert_class(archive, "epi_archive")
  time_type <- archive$time_type
  assert_string(varname)
  assert_subset(varname, vctrs::vec_set_difference(colnames(archive$DT), key_colnames(archive)))
  assert_scalar(relative_time)
  relative_time <- time_delta_standardize(relative_time, time_type, "fast")
  # TODO default time_until_semistable
  assert_scalar(time_until_semistable)
  time_until_semistable <- time_delta_standardize(time_until_semistable, time_type, "fast")
  anchor_versions <- vec_cast_patched(anchor_versions, archive$DT$version)
  assert_string(out_name)
  out_name <- glue::glue_data(list(
    .col = varname,
    .dir = if (time_delta_to_n_steps(relative_time, time_type) <= 0) "lag" else "lead",
    .amt = paste0(abs(time_delta_to_n_steps(relative_time, time_type)), time_type_unit_abbr(time_type))
  ), out_name)
  checkmate::assert_true(list(nomatch) %in% list(NA, NULL))

  epikey_names <- key_colnames(archive, exclude = c("time_value", "version"))
  epikeys <- unique(archive$DT, by = epikey_names, cols = character()) %>%
    as.data.frame() %>% as_tibble()

  anchors <- tibble(anchor_version = anchor_versions, time_value = anchor_versions + relative_time)
  needles <- dplyr::cross_join(epikeys, anchors)
  haystack <- archive %>%
    epix_as_of_latest() %>%
    select(all_of(key_colnames(.)), !!out_name := !!varname)
  by <- key_colnames(haystack)

  if (identical(nomatch, NA)) {
    matches <- dplyr::left_join(needles, haystack, by = by)
  } else if (identical(nomatch, NULL)) {
    matches <- dplyr::inner_join(needles, haystack, by = by)
  } else {
    cli_abort("Unsupported `nomatch` option {nomatch}.")
  }

  matches %>%
    mutate(!!out_name := if_else(time_value + time_until_semistable <= .env$archive$versions_end, .data[[out_name]], NA)) %>%
    select(-time_value)
}

# TODO nomatch -> better name & options

# anchor_version -> forecast_date?  can't come up with generic sub-in for "date"...

# FIXME make sure relative_time standard with epipredict

# TODO fn to get all lags / all lags up to some point / a set of lags? long vs. wide format

# TODO for purely additive revisions, function to get the additions / incremental reports by lag?

# FIXME time type != version type issues
