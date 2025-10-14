#' Get predictor lag train&test data, factoring in data maturity (revisioning)
#'
#' Pairs with [`epix_target_evaluation_data`] to facilitate
#' version-aware modeling (including backcasting, nowcasting, and
#' forecasting), or custom revision analysis (see also
#' [`revision_summary`]).
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
#'   "`r gsub("([{}])", "\\\\\\1", rlang::fn_fmls(epix_realtime_predictor_data)$out_name)`".
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
#'   epix_realtime_predictor_data("percent_cli", -3)
#'
#' # Compare against our best idea of what the truth was for those
#' # days by combining with `epix_target_evaluation_data`:
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' inner_join(
#'   archive_cases_dv_subset %>%
#'     epix_realtime_predictor_data("percent_cli", -3),
#'   archive_cases_dv_subset %>%
#'     epix_target_evaluation_data("percent_cli", -3, time_until_semistable = 60),
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
#'     epix_realtime_predictor_data("percent_cli", -7, drop_time_value = FALSE),
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
#'     epix_realtime_predictor_data("percent_cli", relative_time, out_name = "value") %>%
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
epix_realtime_predictor_data <- function(archive, varname, relative_time,
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

#' Get target evaluation/fitting data, factoring in data maturity (revisioning)
#'
#' Pairs with [`epix_realtime_predictor_data`]; see its documentation
#' for more details.
#'
#' Assumes that our goal is to predict the value of the target in its
#' final revision.  Takes a very simplistic approach to handling the
#' fact that we may not have that yet:
#'
#' 1. For most `time_value`s, accept the latest available
#'    reporting as "close enough" and use as-is.
#'
#' 2. For recent `time_value`s, deem them too unreliable to use in
#'    fitting a model and replace the target values with `NA`s.
#'    (Otherwise our model may learn to output overly wide prediction
#'    intervals and/or biased predictions.)  The definition of
#'    "recent" is controlled by `time_until_semistable`.
#'
#' Other basic alternatives that could be paired with
#' `epix_realtime_predictor_data` include assigning weights to each
#' measurement based on its degree of reliability.  More complex
#' approaches would model each revision in the revision process as a
#' separate target.
#'
#' @inheritParams epix_realtime_predictor_data
#' @param varname String; name of target variable/column
#' @param time_until_semistable Length-1 time delta; replace the
#'   target value with `NA` if it hasn't been at least
#'   `time_until_semistable` since its `time_value` that its latest
#'   available version (`archive$versions_end`) was recorded.
#' @param anchor_versions Optional vector of versions (a.k.a. forecast
#'   dates/times) for which you'd like the target evaluation data;
#'   defaults to all versions with updates in `archive$DT` plus any
#'   versions that look like they would be part of the normal
#'   reporting schedule but didn't change any values. Note that this
#'   is to help line up with the appropriate predictor data from
#'   [`epix_realtime_predictor_data`]; we'll still be using the latest
#'   available version of each target values, regardless of
#'   `anchor_versions`.
#'
#' @export
epix_target_evaluation_data <- function(archive, varname, relative_time,
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

# We can apply this separately for each nowcast_date to ensure that we consider
# the latest possible value for every signal, though whether that is advisable
# or not may depend on revision characteristics of the signals.
#
# TODO reconsider using this in the nowcaster and replacing with a
# filter_to_same_wday or version-weighting scheme.  This might be a
# useful utility, but think more about multiple signals, and the
# motivating case of running this separately day to day based on which
# lags are available in real time latest.
thin_daily_to_weekly_archive <- function(archive) {
  key_nms <- key(archive$DT)
  val_nms <- setdiff(names(archive$DT), key_nms)
  update_tbl <- as_tibble(archive$DT)
  val_nms |>
    lapply(function(val_nm) {
      update_tbl[c(key_nms, val_nm)] |>
        # thin out to weekly, making sure that we keep the max time_value with non-NA value:
        filter(as.POSIXlt(time_value)$wday == as.POSIXlt(max(time_value[!is.na(.data[[val_nm]])]))$wday) |>
        # re-align:
        mutate(
          time_value = time_value - as.POSIXlt(time_value)$wday, # Sunday of same epiweek
          old_version = version,
          version = version - as.POSIXlt(version)$wday # Sunday of same epiweek
        ) |>
        slice_max(old_version, by = all_of(key_nms)) |>
        select(-old_version) |>
        as_epi_archive(other_keys = setdiff(key_nms, c("geo_value", "time_value", "version")),
                       compactify = TRUE)
    }) |>
    reduce(epix_merge, sync = "locf")
}

chr_mapping_standardize <- function(mapping, chr_keys, mapping_arg = rlang::caller_arg(mapping), call = rlang::caller_env()) {
  if (is.null(names(mapping))) {
    `names<-`(vctrs::vec_recycle(mapping, vec_size(chr_keys)), chr_keys)
  } else {
    checkmate::assert_names(names(mapping), permutation.of = chr_keys, .var.name = mapping_arg)
    mapping[chr_keys]
  }
}

# TODO refactor out the target&predictor search stuff into a function?
regression_nowcaster2 <- function(archive,
                                  target, predictors = target,
                                  # TODO better defaults
                                  target_relative_time = 0L,
                                  search_predictor_shifts_within = as.difftime(60, units = "days"),
                                  predictor_search_offset = 0L,
                                  # TODO predictor shift spacing?
                                  min_n_predictor_shifts = dplyr::case_match(predictors, target ~ 1L, .default = 0L),
                                  max_n_predictor_shifts = 3L,
                                  min_n_training_each_predictor = 30L,
                                  max_n_training_intersection = Inf,
                                  time_until_target_semistable = as.difftime(60, units = "days"),
                                  trainer = linear_reg(),
                                  args_list = arx_args_list() # FIXME lag 0 etc.
                                  ) {
  if (!is_epi_archive(archive) && !is_grouped_epi_archive(archive)) {
    cli_abort("`archive` must be an `epi_archive` or `grouped_epi_archive` object,
               not an object of class {format_chr_deparse(class(archive))}")
  }
  if (is_epi_archive(archive)) {
    time_type <- archive$time_type
  } else {
    time_type <- archive$ungrouped$time_type
  }
  assert_string(target)
  assert_character(predictors, any.missing = FALSE)
  assert_scalar(target_relative_time)
  target_relative_time <- time_delta_standardize(target_relative_time, time_type, "fast")
  search_predictor_shifts_within <- chr_mapping_standardize(search_predictor_shifts_within, predictors)
  search_predictor_shifts_within <- time_delta_standardize(search_predictor_shifts_within, time_type, "fast")
  # Standardize chr mapping first if other standardization operations
  # might accidentally unname.  Otherwise, standardize the mapping
  # after other validation/standardization.
  predictor_search_offset <- chr_mapping_standardize(predictor_search_offset, predictors)
  predictor_search_offset <- time_delta_standardize(predictor_search_offset, time_type, "fast")
  checkmate::assert_true(is_bare_integerish(min_n_predictor_shifts))
  min_n_predictor_shifts <- chr_mapping_standardize(min_n_predictor_shifts, predictors)
  checkmate::assert_true(is_bare_integerish(max_n_predictor_shifts))
  max_n_predictor_shifts <- chr_mapping_standardize(max_n_predictor_shifts, predictors)
  checkmate::assert_true(is_bare_integerish(min_n_training_each_predictor))
  checkmate::assert_true(is_bare_integerish(max_n_training_intersection))
  checkmate::assert_scalar(time_until_target_semistable)
  time_until_target_semistable <- time_delta_standardize(time_until_target_semistable, time_type, "fast")
  # TODO finish validation

  if (is_grouped_epi_archive(archive) || length(unique(archive$DT$geo_value)) != 1L) {
    stop("FIXME TODO grouping and multikey")
  }

  nowcast_date <- archive$versions_end
  target_time_value <- nowcast_date + target_relative_time
  latest_edf <- archive %>% epix_as_of(nowcast_date)

  predictor_shift_selection_config <- tibble(
    predictor = predictors,
    # search_predictor_shifts_within,
    # predictor_search_offset,
    min_n_training_each_predictor,
    min_n_predictor_shifts,
    max_n_predictor_shifts
  )

  checkmate::assert_false("relative_time" %in% names(latest_edf))

  predictor_shifts_available <-
    latest_edf %>%
    mutate(relative_time = time_delta_standardize(time_value - .env$nowcast_date, .env$time_type, "fast")) %>%
    select(!all_of(key_colnames(latest_edf))) %>%
    pivot_longer(!relative_time, names_to = "predictor", values_to = "value") %>%
    tidyr::drop_na(value) %>%
    mutate(offset_relative_time = relative_time - predictor_search_offset[predictor]) %>%
    filter(abs(time_delta_to_n_steps(offset_relative_time, time_type)) <=
             time_delta_to_n_steps(search_predictor_shifts_within, time_type)) %>%
    arrange(
      predictor,
      abs(time_delta_to_n_steps(offset_relative_time, .env$time_type)),
      # prioritize later time_values on ties
      -time_delta_to_n_steps(offset_relative_time, .env$time_type)
    )

  # TODO non-negativity checks on some args
  predictor_search_results <-
    dplyr::nest_join(predictor_shift_selection_config, predictor_shifts_available, by = "predictor") %>%
    dplyr::rowwise() %>%
    mutate(selections = {
      selections <- list()
      debug_info <- list()
      for (i in seq_len(nrow(.data$predictor_shifts_available))) {
        # TODO in validation code: inequality check on max vs. min n predictor shifts
        if (length(selections) == .data$max_n_predictor_shifts) {
          break
        }
        candidate_selection <- epix_realtime_predictor_data(archive, .data$predictor, .data$predictor_shifts_available$relative_time[[i]]) %>%
          na.omit() %>%
          # TODO rename later?  so when this is its own utility, we can potentially avoid confusion
          rename(time_value = anchor_version) %>%
          as_epi_df()
        # FIXME `training` -> `rows`?
        debug_info_record <- list(
          relative_time = .data$predictor_shifts_available$relative_time[[i]],
          n_nonmissing_analogues = nrow(candidate_selection)
        )
        if (nrow(candidate_selection) >= .env$min_n_training_each_predictor) {
          selections <- c(selections, list(candidate_selection))
          debug_info_record$selected <- TRUE
        } else {
          debug_info_record$selected <- FALSE
        }
        # TODO selection cadence?
        debug_info <- c(debug_info, list(debug_info_record))
      }
      if (length(selections) < .data$min_n_predictor_shifts) {
        # TODO which test-time things were explicit NAs?
        # ... except we don't know that in multisignal archive
        # format
        debug_info_tbl <- debug_info %>%
          map(as_tibble) %>%
          dplyr::bind_rows()
        if (nrow(debug_info_tbl) == 0L) {
          cli_abort(c("Predictor {format_varname(predictor)} didn't have any non-NA values available within the search window, but the nowcaster `min_n_predictor_shifts` setting required us to find at least {min_n_predictor_shifts[[predictor]]}.",
                      ">" = "Consider expanding or shifting the search window with `search_predictor_shifts_within`, `predictor_search_offset`.",
                      ">" = "If {format_varname(predictor)} isn't essential to the nowcast, consider setting `min_n_predictor_shifts` to 0 so you can use it when it's available and skip it if it's not.",
                      " " = "Additionally, if you're backtesting:",
                      ">" = "Check that you're not backtesting on a version of the data before the first report recorded from this data source.  If this is the problem, you'll need to start backtesting not only after this first report, but long enough after so that there will be at least `min_n_training_each_predictor` training versions available.)"))
        } else {
          # cli_abort("TODO error message")
          # cli_abort(c("Predictor {format_varname(predictor)} didn't have enough usable time shifts in the search window.  We were required to find {min_n_predictor_shifts[[predictor]]} usable time shifts, but only found {length(selections)}.  This could be due to one or more of the following:"
          #             ">" = "Look at `epix_as_of_latest(archive)` and consider expanding or shifting the search window with `search_predictor_shifts_within`, `predictor_search_offset`.",
          #             "*" = "Maybe the ",
          #              (a) didn't have enough non-NA values available within the search range, or
          #              (b) must have at least {min_n_predictor_shifts}, but only had {length(selections)}.",
          #             c("i" = 'Relative times with non-NA values on the forecast date were:
          #                  {debug_info_tbl$relative_time}',
          #               "i" = "Number of analogous non-NA values in the history data were:
          #                  {debug_info_tbl$n_nonmissing_analogues}, respectively",
          #               "i" = 'Were these predictor shifts selected?:
          #                  {data.table::fifelse(debug_info_tbl$selected, "yes", "no")}')
          #             ))
          cli_abort(c("Predictor {format_varname(predictor)} didn't have enough usable time shifts in the search window.  We were required to find {min_n_predictor_shifts[[predictor]]} usable time shifts, but only found {length(selections)}.",
                      if (nrow(debug_info_tbl) < min_n_predictor_shifts[[predictor]]) {
                        c("x" = "There were only {nrow(debug_info_tbl)} non-NA predictor values found within the search window.")
                      } else {
                        stop("Continue working on error messaging.")
                      }
                      ))
        }
      } else {
        list(selections)
      }
    }) %>%
    ungroup() %>%
    .$selections %>%
    vctrs::list_unchop()

  predictors_edf <- predictor_search_results %>%
    purrr::reduce(dplyr::full_join, by = key_colnames(latest_edf))

  target_edf <- epix_target_evaluation_data(archive, target, target_relative_time, time_until_target_semistable) %>%
    rename(time_value = anchor_version) %>%
    as_epi_df()
  # TODO naming... maybe need to reverse back to time_value + orig target col name

  training_test <- dplyr::full_join(predictors_edf, target_edf, by = key_colnames(latest_edf))

  # training <- training_test %>%
  #   tidyr::drop_na() %>%
  #   dplyr::slice_max(time_value, n = max_n_training_intersection)

  # test <- training_test %>%
  #   filter(time_value == .env$nowcast_date)

  epipredict::arx_forecaster(training_test,
                             # FIXME not sure this is going to work or if we'll have to fake epipredict out with some fake ahead
                             vctrs::vec_set_difference(names(target_edf), key_colnames(target_edf)),
                             # FIXME TODO use arg
                             trainer = epipredict::quantile_reg(quantile_levels = 0.5),
                             # FIXME TODO use arg
                             args_list = arx_args_list(
                               # FIXME max_n_training_intersection naming when have pooling
                               lags = 0L, ahead = 0L, n_training = max_n_training_intersection,
                               forecast_date = nowcast_date, target_date = nowcast_date
                             )
                             )
}

regression_nowcaster <- function(archive, settings, return_info = FALSE) {
  if (!inherits(archive, "epi_archive")) {
    stop("`archive` isn't an `epi_archive`")
  }
  if (length(unique(archive$DT$geo_value)) != 1L) {
    # FIXME
    stop("Expected exactly one unique `geo_value`")
  }
  if (archive$time_type == "day") {
    # TODO replace with ...
    archive <- thin_daily_to_weekly_archive(archive)
  }

  nowcast_date <- archive$versions_end
  target_time_value <- nowcast_date
  latest_edf <- archive %>% epix_as_of(nowcast_date)

  predictor_descriptions <-
    latest_edf %>%
    mutate(lag_days = as.integer(nowcast_date - time_value)) %>%
    select(-c(geo_value, time_value)) %>%
    pivot_longer(-lag_days, names_to = "varname", values_to = "value") %>%
    drop_na(value) %>%
    inner_join(settings$predictors, by = "varname", unmatched = "error") %>%
    filter(abs(lag_days) <= max_abs_shift_days) %>%
    arrange(varname, abs(lag_days)) %>%
    group_by(varname) %>%
    filter(seq_len(n()) <= max_n_shifts[[1]]) %>%
    ungroup() %>%
    mutate(predictor_name = paste0(varname, "_lag", lag_days, "_realtime")) %>%
    select(varname, lag_days, predictor_name)

  predictor_edfs <- predictor_descriptions %>%
    pmap(function(varname, lag_days, predictor_name) {
      get_predictor_training_data(archive, varname, lag_days, predictor_name)
    }) %>%
    lapply(na.omit) %>%
    keep(~ nrow(.x) >= settings$min_n_training_per_predictor)

  if (length(predictor_edfs) == 0) {
    stop("Couldn't find acceptable predictors in the latest data.")
  }

  predictors <- predictor_edfs %>%
    reduce(full_join, by = c("geo_value", "time_value"))

  target <- latest_edf %>%
    filter(time_value <= max(time_value) - settings$days_until_target_semistable) %>%
    select(geo_value, time_value, mortality_semistable = mortality)

  training_test <- full_join(predictors, target, by = c("geo_value", "time_value"))

  training <- training_test %>%
    drop_na() %>%
    slice_max(time_value, n = settings$max_n_training_intersection)

  test <- training_test %>%
    filter(time_value == nowcast_date)

  if (isTRUE(settings$median)) {
    fit <- training %>%
      select(any_of(predictor_descriptions$predictor_name), mortality_semistable) %>%
      quantreg::rq(formula = mortality_semistable ~ ., tau = 0.5)
  } else {
    fit <- training %>%
      select(any_of(predictor_descriptions$predictor_name), mortality_semistable) %>%
      lm(formula = mortality_semistable ~ .)
  }

  pred <- tibble(
    geo_value = "ca",
    nowcast_date = nowcast_date,
    target_date = target_time_value,
    prediction = unname(predict(fit, test))
  )

  if (return_info) {
    return(tibble(
      coefficients = list(coef(fit)),
      predictions = list(pred)
    ))
  } else {
    return(pred)
  }
}




# === TODOs for variable extractors: ===

# TODO tidyselect?
# TODO indexing based on a reference_date / reference_time? as in Hub?
# TODO allow for version lag?
# TODO vs. label_cols subset of time, ver, lag
# TODO vs. long and wide formats?

# TODO nomatch -> better name & options? though dplyr's "equivalent"
# `unmatched` doesn't cover the NULL case; that's controlled by the
# join function selection, and maybe `nomatch` is better than
# homespun?

# TODO anchor_version -> forecast_date?  but can't come up with generic
# sub-in for "date" that isn't confusingly saying "time"...

# TODO fn to get all lags / all lags up to some point / a set of lags? long vs. wide format

# TODO for purely additive revisions, function to get the additions / incremental reports by lag?

# FIXME time type != version type issues


# Consider
#
# v - 0  miss
# v - 1  miss
# v - 2  NA
# v - 3  val,  not enough training data
# v - 4  val,  enough training data,            use
# v - 5  val,  not far enough from used above
# v - 6  val,  not far enough from used above
# v - 7  val,  not enough training data
# v - 8  miss
# v - 9  val,  not enough training data
# v - 10 val,  not enough training data
# v - 11 val,  enough training data,            use
