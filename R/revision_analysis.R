#' A function to describe revision behavior for an archive
#' @description
#' `revision_summary` removes all missing values, and then computes some basic
#'   statistics about the revision behavior of an archive, returning a tibble of
#'   a per-epi-key (so time_value, geo_value pair, possibly others based on the
#'   metadata). If `print_inform` is true, it prints a concise summary. The
#'   columns returned are:
#'  1. `min_lag`: the minimum time to any value (if `drop_nas=FALSE`, this
#'   includes `NA`'s)
#'  2. `max_lag`: the amount of time until the final (new) version (same caveat
#'   for `drop_nas=FALSE`, though it is far less likely to matter)
#'  3. `max_change`: the difference between the smallest and largest values (this
#'   always excludes `NA` values)
#'  4. `max_rel_change`: `max_change` divided by the largest value (so it will
#'   always be less than 1). Note that this need not be the final value. It will
#'   be `NA` whenever `max_change` is 0.
#'  5. `time_to_x_final`: where `x` is the `percent_final_value` (default). This
#'   gives the lag when the value is within `1-x` of the value at the final
#'   time. For example, consider the series (0,20, 99, 150, 102, 100); then
#'   `time_to_x_final` is the 5th index, since even though 99 is within 20%, it
#'   is outside the window afterwards at 150.
#' @param epi_arch an epi_archive to be analyzed
#' @param ... tidyselect, used to choose the column to summarize. If empty, it
#'   chooses the first. Currently only implemented for one column at a time
#' @param drop_nas bool, drop any `NA` values from the archive? After dropping
#'   `NA`'s compactify is run again to make sure there are no duplicate values.
#' @param print_inform bool, determines whether to print summary information, or
#'   only return the full summary tibble
#' @param percent_final_value double between 0 and 1. Determines the threshold
#'   used for the `time_to`
#' @param quick_revision difftime or integer (integer is treated as days), for
#'   the printed summary, the amount of time between the final revision and the
#'   actual time_value to consider the revision quickly resolved. Default of 3
#'   days
#' @param few_revisions integer, for the printed summary, the upper bound on the
#'   number of revisions to consider "few". Default is 3.
#' @param abs_change_threshold numeric, for the printed summary, the maximum change
#'   used to characterize revisions which don't actually change very much.
#'   Default is 5% of the maximum value in the dataset, but this is the most
#'   unit dependent of values, and likely needs to be chosen appropriate for the
#'   scale of the dataset
#' @param rel_change_threshold float between 0 and 1, for the printed summary, the
#'   relative change fraction used to characterize revisions which don't
#'   actually change very much. Default is .1, or 10% of the final value
#' @examples
#'
#' revision_example <- revision_summary(archive_cases_dv_subset, percent_cli)
#'
#' revision_example %>% arrange(desc(max_change))
#' @export
#' @importFrom cli cli_inform cli_abort cli_li
#' @importFrom rlang list2 syms
#' @importFrom dplyr mutate group_by arrange filter if_any all_of across pull
#'   everything ungroup summarize if_else %>%
revision_summary <- function(epi_arch,
                             ...,
                             drop_nas = TRUE,
                             print_inform = TRUE,
                             percent_final_value = 0.8,
                             quick_revision = as.difftime(3, units = "days"),
                             few_revisions = 3,
                             rel_change_threshold = 0.1,
                             abs_change_threshold = NULL) {
  arg <- enquos(...)
  if (length(arg) == 0) {
    first_non_key <- !(names(epi_arch$DT) %in% c(key_colnames(epi_arch), "version"))
    arg <- syms(names(epi_arch$DT)[first_non_key][1])
  } else if (length(arg) > 1) {
    cli_abort("Not currently implementing more than one column at a time. Run each separately")
  }
  if (is.null(abs_change_threshold)) {
    abs_change_threshold <- .05 * epi_arch$DT %>%
      pull(...) %>%
      max(na.rm = TRUE)
  }
  # for each time_value, get
  #   the number of revisions
  #   the maximum change in value (both absolute and relative)
  #   the min lag
  #   the max lag
  #
  # revision_tibble
  keys <- key_colnames(epi_arch)
  names(epi_arch$DT)

  if (drop_nas) {
    # if we're dropping NA's, we should recompactify
    revision_behavior <-
      epi_arch$DT %>%
      filter(!is.na(!!arg[[1]])) %>%
      arrange(across(c(geo_value, time_value, all_of(keys), version))) %>% # need to sort before compactifying
      compactify_tibble(c(keys, version))
  } else {
    revision_behavior <- epi_arch$DT
  }
  revision_behavior <- revision_behavior %>%
    mutate(across(c(version, time_value), list(int = as.integer))) %>%
    mutate(lag = version_int - time_value_int) %>% # nolint: object_usage_linter
    group_by(across(all_of(keys))) %>% # group by all the keys
    select(-time_value_int, -version_int) %>%
    summarize(
      n_revisions = dplyr::n() - 1,
      min_lag = min(lag), # nolint: object_usage_linter
      max_lag = max(lag), # nolint: object_usage_linter
      max_change = suppressWarnings(max(!!arg[[1]], na.rm = TRUE) - min(!!arg[[1]], na.rm = TRUE)),
      max_rel_change = (max_change) / suppressWarnings(max(!!arg[[1]], na.rm = TRUE)), # nolint: object_usage_linter
      time_to = time_to_x_percent(lag, !!arg[[1]], percent = percent_final_value)
    ) %>%
    ungroup() %>%
    mutate(
      # TODO the units here may be a problem
      min_lag = as.difftime(min_lag, units = "days"), # nolint: object_usage_linter
      max_lag = as.difftime(max_lag, units = "days"), # nolint: object_usage_linter
      time_to_pct_final = as.difftime(time_to, units = "days") # nolint: object_usage_linter
    ) %>%
    select(-time_to)
  if (print_inform) {
    total_num <- nrow(revision_behavior) # nolint: object_usage_linter
    cli_inform("Number of revisions:")
    cli_inform("Min lag (time to first version):")
    difftime_summary(revision_behavior$min_lag)
    if (!drop_nas) {
      total_na <- nrow(epi_arch$DT %>% filter(is.na(!!arg[[1]]))) # nolint: object_usage_linter
      cli_inform("Fraction of all versions that are `NA`:")
      cli_li(num_percent(total_na, nrow(epi_arch$DT)))
    }
    total_num_unrevised <- sum(revision_behavior$n_revisions == 0) # nolint: object_usage_linter
    cli_inform("No revisions:")
    cli_li(num_percent(total_num_unrevised, total_num))
    total_quickly_revised <- sum( # nolint: object_usage_linter
      revision_behavior$max_lag <=
        as.difftime(quick_revision, units = "days")
    )
    cli_inform("Quick revisions (last revision within {quick_revision}
{units(quick_revision)} of the `time_value`):")
    cli_li(num_percent(total_quickly_revised, total_num))
    total_barely_revised <- sum( # nolint: object_usage_linter
      revision_behavior$n_revisions <=
        few_revisions
    )
    cli_inform("Few revisions (At most {few_revisions} revisions for that `time_value`):")
    cli_li(num_percent(total_barely_revised, total_num))
    cli_inform("")
    cli_inform("Changes in Value:")

    real_revisions <- revision_behavior %>% filter(n_revisions > 0) # nolint: object_usage_linter
    n_real_revised <- nrow(real_revisions) # nolint: object_usage_linter
    rel_change <- sum( # nolint: object_usage_linter
      real_revisions$max_rel_change <
        rel_change_threshold,
      na.rm = TRUE
    ) + sum(is.na(real_revisions$max_rel_change))
    cli_inform("Less than {rel_change_threshold} change in relative value (only from the revised subset):")
    cli_li(num_percent(rel_change, n_real_revised))
    na_rel_change <- sum(is.na(real_revisions$max_rel_change)) # nolint: object_usage_linter
    cli_inform("{units(quick_revision)} until over {percent_final_value} percent of the final value:")
    difftime_summary(revision_behavior[[glue::glue("time_to_pct_final")]])
    abs_change <- sum( # nolint: object_usage_linter
      real_revisions$max_change >
        abs_change_threshold
    ) # nolint: object_usage_linter
    cli_inform("Change by more than {abs_change_threshold} in actual value (when revised):")
    cli_li(num_percent(abs_change, n_real_revised))
  }
  return(revision_behavior)
}

#' @keywords internal
time_to_x_percent <- function(lags, values, percent = .9) {
  final_value <- values[which.max(lags)]
  close_enough <- abs(values - final_value) < (1 - percent) * final_value
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
  length(bool_vec) - tail(runs$lengths, n = 1)
  values_from[length(bool_vec) - tail(runs$lengths, n = 1) + 1]
}



#' simple util for printing a fraction and it's percent
#' @keywords internal
num_percent <- function(a, b) {
  glue::glue("{prettyNum(a, big.mark=',')} out of {prettyNum(b, big.mark=',')}
({round(a/b*100,digits=2)}%)")
}

#' summary doesn't work on difftimes
#' @keywords internal
difftime_summary <- function(diff_time_val) {
  data.frame(
    min = min(diff_time_val),
    median = median(diff_time_val),
    mean = round(mean(diff_time_val), 0),
    max = max(diff_time_val),
    row.names = " ",
    check.names = FALSE
  ) %>% print()
}
