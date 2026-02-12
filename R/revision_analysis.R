#' A function to describe revision behavior for an archive.
#'
#' @description
#' `revision_summary` removes all missing values (if requested), and
#'   then computes some basic statistics about the revision behavior
#'   of an archive, returning a tibble summarizing the revisions per
#'   time_value+epi_key features (limited to those that have data
#'   available past the min waiting period to compare against, and are
#'   not detected as added in a bulk report). The columns returned
#'   are:
#'  1. `n_revisions`: the total number of revisions for that entry
#'  2. `min_lag`: the minimum time to any value (if `drop_nas=FALSE`, this
#'   includes `NA`'s)
#'  3. `max_lag`: the amount of time until the final (new) version (same caveat
#'   for `drop_nas=FALSE`, though it is far less likely to matter)
#'  4. `min_value`: the minimum value across revisions
#'  5. `max_value`: the maximum value across revisions
#'  6. `median_value`: the median value across revisions
#'  7. `spread`: the difference between the smallest and largest values (this
#'   always excludes `NA` values)
#'  8. `rel_spread`: `spread` divided by the largest value (so it will
#'   always be less than 1). Note that this need not be the final value. It will
#'   be `NA` whenever `spread` is 0.
#'  9. `lag_near_latest`: the time taken for the revisions to settle to within
#'   `within_latest` (default 20%) of the final value and stay there. For
#'   example, consider the series (0, 20, 99, 150, 102, 100); then
#'   `lag_near_latest` is 5, since even though 99 is within 20%, it is outside
#'   the window afterwards at 150.
#'
#' @param epi_arch an epi_archive to be analyzed
#' @param ... <[`tidyselect`][dplyr::dplyr_tidy_select]>, used to
#'   choose the column to summarize. If empty and there is only one
#'   value/measurement column (i.e., not in [`key_colnames`]) in the
#'   archive, it will automatically select it.  If supplied, `...`
#'   must select exactly one column.
#' @param min_waiting_period `difftime`, integer or `NULL`. Sets a
#'   cutoff: any time_values that have not had at least
#'   `min_waiting_period` to stabilize as of the `versions_end` are
#'   removed. `min_waiting_period` should characterize the typical
#'   time during which most significant revisions occur. The default
#'   of 60 days corresponds to a typical near-final value for case
#'   counts as reported in the context of insurance. To avoid this
#'   filtering, either set to `NULL` or 0. A `difftime` will be
#'   rounded up to the appropriate `time_type` if necessary (that is 5
#'   days will be rounded to 1 week if the data is weekly).
#' @param within_latest double between 0 and 1. Determines the
#'   threshold used for the `lag_to`
#' @param bulk_reporting_level,bulk_reporting_multiplier numeric; the
#'   former between 0 and 1, typically close to but less than one, and
#'   the latter `>= 1`; defaults of 0.8 and 1.2,
#'   respectively. Determines how to detect bulk reporting.  Consider
#'   the distribution of "max initial lags" across geodemographic
#'   group x version pairs that add initial observations for new time
#'   values; a bulk reporting lag threshold is determined by taking
#'   the `bulk_reporting_level`-th quantile of this distribution,
#'   multiplying by `bulk_reporting_multiplier`, and rounding to an
#'   integer number of time intervals.  To avoid flagging anything as
#'   bulk reporting, set `bulk_reporting_level = 1`.
#' @param compactify bool. If `TRUE`, we will compactify after the
#'   signal requested in `...` has been selected on its own and the
#'   `drop_nas` step.  This helps, for example, to give similar
#'   results when called on [merged][epix_merge] and single-signal
#'   archives, since merged archives record an update when any of the
#'   other signals change, not just the requested signal. The default
#'   is `TRUE`.
#' @param compactify_abs_tol length-1 double, used if `compactify` is
#'   `TRUE`, it determines the threshold for when two doubles are
#'   considered identical.
#' @param compactify_drop_initial_nas bool; should we drop initial
#'   estimates of NA during the compactification step?  Default is
#'   TRUE, because these NAs are likely present due to
#'   [`epix_merge()`]ing with a more timely indicator, and the
#'   upstream source probably didn't actually report explicit NAs as
#'   provisional estimates.
#' @param drop_nas bool, do we drop all `NA` values (not just initial
#'   estimates of `NA`) from the archive data structure prior to
#'   (optional) compactification?  This both strips (i) initial
#'   estimates of NA and (ii) explicit revisions from a non-NA
#'   estimate to NA.  Equivalent to `compactify_drop_initial_nas` if
#'   `compactify` is `TRUE`. Default is FALSE, favoring `compactify =
#'   TRUE, compactify_drop_initial_nas = TRUE`.
#' @param return_only_tibble boolean to return only the simple
#'   `tibble` of computational results rather than the complete S3
#'   object.
#'
#' @details Applies to `epi_archive`s with `time_type`s of `"day"`, `"week"`,
#'   and `"yearmonth"`. It can also work with a `time_type` of `"integer"` if
#'   the possible `time_values` are all consecutive integers; you will need to
#'   manually specify the `min_waiting_period` and `quick_revision`, though.
#'   Using a `time_type` of `"integer"` with week numbers like 202501 will
#'   produce incorrect results for some calculations, since week numbering
#'   contains jumps at year boundaries.
#'
#' @return An S3 object with class `revision_analysis`. This function is typically
#'   called for the purposes of inspecting the printed output. The
#'   results of the computations are available in
#'   `revision_analysis(...)$revision_behavior`. If you only want to access
#'   the internal computations, use `return_only_tibble = TRUE`.
#'
#' @examples
#'
#' # Print revision summary:
#' revision_analysis(archive_cases_dv_subset, percent_cli)
#'
#' # Print some underlying data:
#' revision_example <- revision_analysis(archive_cases_dv_subset, percent_cli)
#' revision_example$revision_behavior %>% arrange(desc(spread))
#'
#' @export
#' @importFrom cli cli_inform cli_abort cli_li
#' @importFrom rlang list2 syms dots_n
#' @importFrom vctrs vec_cast
#' @importFrom dplyr mutate group_by arrange filter if_any all_of across pull pick c_across
#'   everything ungroup summarize if_else %>%
revision_analysis <- function(epi_arch,
                              ...,
                              min_waiting_period = as.difftime(60, units = "days"),
                              within_latest = 0.2,
                              bulk_reporting_level = 0.8,
                              bulk_reporting_multiplier = 1.2,
                              compactify = TRUE,
                              compactify_abs_tol = 0,
                              compactify_drop_initial_nas = TRUE,
                              drop_nas = FALSE,
                              return_only_tibble = FALSE) {
  assert_class(epi_arch, "epi_archive")
  if (methods::is(min_waiting_period, "difftime")) {
    min_waiting_period <- min_waiting_period %>%
      difftime_approx_ceiling_time_delta(epi_arch$time_type)
  }
  # if the column to summarize isn't specified, use the only one if there is only one
  if (dots_n(...) == 0) {
    # Choose the first column that's not a key:
    value_colnames <- setdiff(archive_colnames(epi_arch), key_colnames(epi_arch))
    if (length(value_colnames) == 1) {
      arg <- value_colnames
    } else {
      cli_abort(c(
        "Cannot determine which column to summarize.",
        "i" = "Value/measurement columns appear to be: {format_varnames(value_colnames)}",
        ">" = "Please specify which column to summarize in `...` (with tidyselect syntax)."
      ), class = "epiprocess__revision_summary_cannot_determine_default_selection")
    }
  } else {
    # get the names of columns matching any tidyselect used in `...`
    arg <- names(eval_select(rlang::expr(c(...)), allow_rename = FALSE, data = archive_colmask(epi_arch)))
    if (length(arg) == 0) {
      cli_abort("Could not find any columns matching the selection in `...`.",
        class = "epiprocess__revision_summary__selected_zero_columns"
      )
    }
    if (length(arg) > 1) {
      cli_abort("Not currently implementing more than one column at a time. Run each separately.")
    }
  }
  # for each time_value, get
  #   the number of revisions
  #   the maximum spread in value (both absolute and relative)
  #   the min lag
  #   the max lag
  #
  # revision_tibble
  epikey_names <- key_colnames(epi_arch, exclude = c("time_value", "version"))
  epikeytime_names <- c(epikey_names, "time_value")
  ukey_names <- c(epikeytime_names, "version")
  time_type <- epi_arch$time_type

  # Stay on the backend handle so select/filter push down on lazy backends;
  # `apply_compactify` is eager (data-frame row indexing), so collect first.
  revision_behavior <- archive_data(epi_arch) %>%
    select(all_of(unique(c(ukey_names, arg))))
  if (!is.null(min_waiting_period)) {
    last_semistable_time_value <- time_minus_n_steps(
      epi_arch$versions_end,
      time_delta_to_n_steps(min_waiting_period, time_type),
      time_type
    )
    revision_behavior <- revision_behavior %>%
      filter(time_value <= last_semistable_time_value)
  }
  if (drop_nas) {
    # if we're dropping NA's, we should recompactify
    revision_behavior <-
      revision_behavior %>%
      filter(!is.na(.data[[arg]]))
  }
  revision_behavior <- revision_behavior %>% dplyr::collect() %>% tibble::as_tibble()
  if (compactify) {
    revision_behavior <- revision_behavior %>%
      apply_compactify(ukey_names, compactify_abs_tol, init_nas_are_locf = compactify_drop_initial_nas)
  }
  revision_behavior <- revision_behavior %>%
    mutate(lag = time_minus_time_in_n_steps(version, time_value, time_type))

  total_na <- revision_behavior %>%
    filter(is.na(c_across(!!arg))) %>% # nolint: object_usage_linter
    nrow()
  n_obs <- nrow(revision_behavior)

  # Note: `setDT()` modifies `revision_behavior` in-place, changing
  # the object it points to from a tibble to a data.table for the
  # remainder of this function, though we eventually point it to
  # something that's a tibble later on.
  setDT(revision_behavior)

  initial_reporting <- revision_behavior %>%
    #   We extract the initial report
    # for each epikey-time combination using `.SD[1]` (relying on
    # chronological sorting by version), then calculate the min/max lag
    # for all first-time data introduced in each version dump.
    .[, .SD[1], by = c(epikeytime_names)] %>%
    .[, list(
      min_initial_lag = min(lag),
      max_initial_lag = max(lag)
    ),
    by = c(epikey_names, "version")
    ] %>%
    setDF() %>%
    as_tibble()
  assert_numeric(bulk_reporting_level, lower = 0, upper = 1, any.missing = FALSE, len = 1L)
  assert_numeric(bulk_reporting_multiplier, lower = 1, any.missing = FALSE, len = 1L)
  bulk_reporting_quantile <- unname(quantile(initial_reporting$max_initial_lag, bulk_reporting_level))
  max_nonbulk_initial_lag <- round(bulk_reporting_multiplier * bulk_reporting_quantile)
  bulk_reporting_versions <- initial_reporting %>%
    filter(.data$max_initial_lag > .env$max_nonbulk_initial_lag) %>%
    pull(version) %>%
    vec_unique() %>%
    vec_sort()
  nonbulk_expanding_versions <-
    vec_set_difference(initial_reporting$version, bulk_reporting_versions) %>%
    vec_sort()
  revision_only_versions <-
    vec_set_difference(revision_behavior$version, c(bulk_reporting_versions, nonbulk_expanding_versions)) %>%
    vec_sort()

  data.table::setnames(revision_behavior, arg, ".VAL") # faster than .SD[[arg]] later on

  revision_behavior <-
    revision_behavior %>%
    {
      with_missing <- .[, list(
        n_revisions = .N - 1L,
        min_lag = min(lag), # nolint: object_usage_linter
        max_lag = max(lag), # nolint: object_usage_linter
        lag_to = lag_within_x_latest(lag, .VAL, prop = ..within_latest) # nolint: object_usage_linter
      ), by = c(epikeytime_names)]
      without_missing <- .[!is.na(.VAL)][, list(
        min_value = min(.VAL), # nolint: object_usage_linter
        max_value = max(.VAL), # nolint: object_usage_linter
        median_value = median(.VAL) # nolint: object_usage_linter
      ), by = c(epikeytime_names)]
      merge(with_missing, without_missing, by = epikeytime_names, all = TRUE)
    } %>%
    setDF() %>%
    as_tibble()
  n_epikeytimes <- nrow(revision_behavior)
  revision_behavior <- revision_behavior %>%
    filter(.data$min_lag <= .env$max_nonbulk_initial_lag) %>%
    mutate(
      spread = max_value - min_value, # nolint: object_usage_linter
      rel_spread = spread / max_value, # nolint: object_usage_linter
      min_lag = n_steps_to_time_delta(min_lag, time_type), # nolint: object_usage_linter
      max_lag = n_steps_to_time_delta(max_lag, time_type), # nolint: object_usage_linter
      lag_near_latest = n_steps_to_time_delta(lag_to, time_type) # nolint: object_usage_linter
    ) %>%
    select(-lag_to) %>%
    relocate(
      time_value, geo_value, all_of(epikey_names), n_revisions, min_lag, max_lag, # nolint: object_usage_linter
      lag_near_latest, spread, rel_spread, min_value, max_value, median_value # nolint: object_usage_linter
    )
  n_nonbulk_epikeytimes <- nrow(revision_behavior)
  if (!return_only_tibble) {
    revision_behavior <- structure(list(
      revision_behavior = revision_behavior,
      initial_reporting = initial_reporting,
      max_nonbulk_initial_lag = n_steps_to_time_delta(max_nonbulk_initial_lag, time_type),
      bulk_reporting_versions = bulk_reporting_versions,
      nonbulk_expanding_versions = nonbulk_expanding_versions,
      revision_only_versions = revision_only_versions,
      range_time_values = range(archive_col(epi_arch, "time_value")),
      signal_variable = arg,
      drop_nas = drop_nas,
      time_type = time_type,
      total_na = total_na,
      max_val = max(archive_col(epi_arch, arg), na.rm = TRUE),
      n_obs = n_obs,
      n_epikeytimes = n_epikeytimes,
      n_nonbulk_epikeytimes = n_nonbulk_epikeytimes,
      within_latest = within_latest
    ), class = "revision_analysis")
  }
  return(revision_behavior)
}


#' Print a `revision_analysis` object
#'
#' @param x a `revision_analysis` object
#' @param quick_revision Difftime or integer (integer is treated as days).
#'   The amount of time between the final revision and the
#'   actual time_value to consider the revision quickly resolved. Default of 3
#'   days. This will be rounded up to the appropriate `time_type` if
#'   necessary (that is 5 days will be rounded to 1 week if the data is weekly).
#' @param few_revisions Integer. The upper bound on the
#'   number of revisions to consider "few". Default is 3.
#' @param abs_spread_threshold Scalar numeric. The
#'   maximum spread used to characterize revisions which don't actually change
#'   very much. Default is 5% of the maximum value in the dataset, but this is
#'   the most unit dependent of values, and likely needs to be chosen
#'   appropriate for the scale of the dataset.
#' @param rel_spread_threshold Scalar between 0 and 1. The relative spread fraction used to characterize revisions which
#'   don't actually change very much. Default is .1, or 10% of the final value
#'
#' @rdname revision_analysis
#' @export
print.revision_analysis <- function(x,
                                    quick_revision = as.difftime(3, units = "days"),
                                    few_revisions = 3,
                                    abs_spread_threshold = NULL,
                                    rel_spread_threshold = 0.1,
                                    ...) {
  if (methods::is(quick_revision, "difftime")) {
    quick_revision <- quick_revision %>%
      difftime_approx_ceiling_time_delta(x$time_type)
  }
  if (is.null(abs_spread_threshold)) abs_spread_threshold <- .05 * x$max_val
  rev_beh <- x$revision_behavior
  cli::cli_h2("Revision analysis for archive spanning time values {.val {x$range_time_values[1]}} to {.val {x$range_time_values[2]}}.") # nolint: line_length_linter
  cli::cli_h3("Across epi_key + versions that add new time values:")
  cli::cli_inform("Freshest new time value's lag/latency:")
  time_delta_summary(x$initial_reporting$min_initial_lag, x$time_type) %>% print()
  cli::cli_inform("Farthest-back new time value's lag/latency:")
  time_delta_summary(x$initial_reporting$max_initial_lag, x$time_type) %>% print()
  if (!x$drop_nas) {
    cli::cli_h3("Across epi_key + time_value + versions:")
    cli_inform("Fraction of all versions that are `NA`:")
    cli_li(num_percent(x$total_na, x$n_obs, ""))
  }

  if (vec_size(x$bulk_reporting_versions) > 0L) {
    cli::cli_h3("Bulk reporting adding initial observations for older epikey + time values:")
    cli_inform("Initial lags above {format_time_delta(x$max_nonbulk_initial_lag, x$time_type)}
                were counted as bulk reporting.")
    cli_inform("Fraction of epi_key + time_values initially added by bulk reporting:")
    cli_li(num_percent(x$n_epikeytimes - x$n_nonbulk_epikeytimes, x$n_epikeytimes, ""))
    cli_inform("Versions containing bulk reporting: {vec_size(x$bulk_reporting_versions)}")
    cli_li("({x$bulk_reporting_versions})")
    cli_inform("Versions adding epikey + time values but no bulk reporting: {vec_size(x$nonbulk_expanding_versions)}")
    cli_inform("Revision-only versions: {vec_size(x$revision_only_versions)}")
  } else {
    cli::cli_h3("Bulk reporting of older epikey + time values: none detected")
    cli_inform("Initial lags above {format_time_delta(x$max_nonbulk_initial_lag, x$time_type)}
                would have been counted as bulk reporting.")
  }

  # \u00a0 is non-breaking space to align text across lines here:
  cli::cli_h3("Remaining information is for non-bulk-reported epikey + time values
               \u00a0\u00a0\u00a0with semi-stable versions past the waiting period available.")

  cli::cli_h3("Fraction of epi_key + time_values with")
  total_num <- nrow(rev_beh) # nolint: object_usage_linter
  total_num_unrevised <- sum(rev_beh$n_revisions == 0) # nolint: object_usage_linter
  cli_inform("No revisions:")
  cli_li(num_percent(total_num_unrevised, total_num, ""))
  total_quickly_revised <- sum( # nolint: object_usage_linter
    time_delta_to_n_steps(rev_beh$max_lag, x$time_type) <=
      time_delta_to_n_steps(quick_revision, x$time_type)
  )
  cli_inform("Quick revisions (last revision within {format_time_delta(quick_revision, x$time_type)}
                of the `time_value`):")
  cli_li(num_percent(total_quickly_revised, total_num, ""))
  total_barely_revised <- sum(rev_beh$n_revisions <= few_revisions)
  cli_inform("Few revisions (At most {.val {few_revisions}} revisions for that `time_value`):")
  cli_li(num_percent(total_barely_revised, total_num, ""))

  cli::cli_h3("Fraction of revised epi_key + time_values which have:")

  real_revisions <- rev_beh %>% filter(n_revisions > 0) # nolint: object_usage_linter
  n_real_revised <- nrow(real_revisions) # nolint: object_usage_linter
  rel_spread <- sum( # nolint: object_usage_linter
    real_revisions$rel_spread < rel_spread_threshold,
    na.rm = TRUE
  ) + sum(is.na(real_revisions$rel_spread))
  cli_inform("Less than {.val {rel_spread_threshold}} spread in relative value:")
  cli_li(num_percent(rel_spread, n_real_revised, ""))
  abs_spread <- sum( # nolint: object_usage_linter
    real_revisions$spread > abs_spread_threshold
  ) # nolint: object_usage_linter
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli_inform("Spread of more than {.val {abs_spread_threshold}} in actual value (when revised):")
  cli::cli_end(divid)
  cli_li(num_percent(abs_spread, n_real_revised, ""))

  # time_type_unit_pluralizer[[time_type]] is a format string controlled by us
  # and/or downstream devs, so we can paste it onto our format string safely:
  units_plural <- pluralize(paste0("{qty(2)}", time_type_unit_pluralizer[[x$time_type]])) # nolint: object_usage_linter
  cli::cli_h3("{toTitleCase(units_plural)} until within {.val {x$within_latest*100}}% of the latest value:")
  time_delta_summary(rev_beh[["lag_near_latest"]], x$time_type) %>% print()

  cli::cli_h3("{toTitleCase(units_plural)} until at the latest lag:")
  time_delta_summary(rev_beh[["max_lag"]], x$time_type) %>% print()
}

#' @export
#' @rdname revision_analysis
revision_summary <- revision_analysis

#' pull the value from lags when values starts indefinitely being within prop of its latest value.
#' @param lags vector of lags; should be sorted
#' @param values this should be a vector (e.g., a column) with length matching that of `lags`
#' @param prop optional length-1 double; proportion
#' @keywords internal
lag_within_x_latest <- function(lags, values, prop = .2) {
  latest_value <- values[[length(values)]]
  close_enough <- abs(values - latest_value) < prop * latest_value
  # we want to ignore any stretches where it's close, but goes farther away later
  return(get_last_run(close_enough, lags))
}

#' return the first value in values_from from the last string of trues in bool_vec
#' @description
#' the point of this operation is to get the value in values_from which occurs
#'   at the same index as the start of the last run of true values in bool_vec.
#'   for example, in c(1,1,0,1,1), we want the 4th entry, since there's a 0
#'   breaking the run
#' @keywords internal
get_last_run <- function(bool_vec, values_from) {
  runs <- rle(bool_vec)
  values_from[[length(bool_vec) - tail(runs$lengths, n = 1) + 1]]
}

#' use when the default behavior returns a warning on empty vectors, which we do
#' not want, and there is no super clean way of preventing this
#' @keywords internal
f_no_na <- function(f, x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    Inf
  } else {
    f(x)
  }
}


#' simple util for printing a fraction and it's percent
#' @keywords internal
num_percent <- function(a, b, b_description) {
  glue::glue("{prettyNum(a, big.mark=',')} out of {prettyNum(b, big.mark=',')} {b_description}
({round(a/b*100,digits=2)}%)")
}

#' Like `summary` but working across all "time deltas", including difftimes
#'
#' Also standardizes units of difftimes to the natural unit for the given
#' `time_type` (via conversion to and from a corresponding number of time
#' steps).
#'
#' @keywords internal
time_delta_summary <- function(time_delta, time_type) {
  if (length(time_delta) > 0) {
    n_steps <- time_delta_to_n_steps(time_delta, time_type)
    res <- data.frame(
      min = min(n_steps),
      median = median(n_steps),
      mean = round(mean(n_steps), 1),
      max = max(n_steps),
      row.names = " ",
      check.names = FALSE
    ) %>%
      mutate(across(c(min, median, mean, max), ~ .x * unit_time_delta(time_type)))
    res
  } else {
    data.frame()
  }
}
