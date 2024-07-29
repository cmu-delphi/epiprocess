#' A function to describe revision behavior for an archive
#' @description
#' `revision_summary` removes all missing values (if requested), and then
#'   computes some basic statistics about the revision behavior of an archive,
#'   returning a tibble of a per-epi-key (so time_value, geo_value pair,
#'   possibly others based on the metadata). If `print_inform` is true, it
#'   prints a concise summary. The columns returned are:
#'  1. `min_lag`: the minimum time to any value (if `drop_nas=FALSE`, this
#'   includes `NA`'s)
#'  2. `max_lag`: the amount of time until the final (new) version (same caveat
#'   for `drop_nas=FALSE`, though it is far less likely to matter)
#'  3. `spread`: the difference between the smallest and largest values (this
#'   always excludes `NA` values)
#'  4. `rel_spread`: `spread` divided by the largest value (so it will
#'   always be less than 1). Note that this need not be the final value. It will
#'   be `NA` whenever `spread` is 0.
#'  5. `time_near_latest`: This gives the lag when the value is within
#'   `within_latest` (default 20%) of the value at the latest time. For example,
#'   consider the series (0,20, 99, 150, 102, 100); then `time_near_latest` is
#'   the 5th index, since even though 99 is within 20%, it is outside the window
#'   afterwards at 150.
#' @param epi_arch an epi_archive to be analyzed
#' @param ... <[`tidyselect`][dplyr_tidy_select]>, used to choose the column to summarize. If empty, it
#'   chooses the first. Currently only implemented for one column at a time
#' @param drop_nas bool, drop any `NA` values from the archive? After dropping
#'   `NA`'s compactify is run again to make sure there are no duplicate values
#'   from occasions when the signal is revised to be NA, and then back to its
#'   immediately-preceding value.
#' @param print_inform bool, determines whether to print summary information, or
#'   only return the full summary tibble
#' @param within_latest double between 0 and 1. Determines the threshold
#'   used for the `time_to`
#' @param quick_revision difftime or integer (integer is treated as days), for
#'   the printed summary, the amount of time between the final revision and the
#'   actual time_value to consider the revision quickly resolved. Default of 3
#'   days
#' @param few_revisions integer, for the printed summary, the upper bound on the
#'   number of revisions to consider "few". Default is 3.
#' @param abs_spread_threshold numeric, for the printed summary, the maximum
#'   spread used to characterize revisions which don't actually change very
#'   much. Default is 5% of the maximum value in the dataset, but this is the
#'   most unit dependent of values, and likely needs to be chosen appropriate
#'   for the scale of the dataset.
#' @param rel_spread_threshold float between 0 and 1, for the printed summary,
#'   the relative spread fraction used to characterize revisions which don't
#'   actually change very much. Default is .1, or 10% of the final value
#' @param compactify_tol float, used if `drop_nas=TRUE`, it determines the
#'   threshold for when two floats are considered identical.
#' @examples
#'
#' revision_example <- revision_summary(archive_cases_dv_subset, percent_cli)
#'
#' revision_example %>% arrange(desc(spread))
#' @export
#' @importFrom cli cli_inform cli_abort cli_li
#' @importFrom rlang list2 syms
#' @importFrom dplyr mutate group_by arrange filter if_any all_of across pull pick
#'   everything ungroup summarize if_else %>%
revision_summary <- function(epi_arch,
                             ...,
                             drop_nas = TRUE,
                             print_inform = TRUE,
                             within_latest = 0.2,
                             quick_revision = as.difftime(3, units = "days"),
                             few_revisions = 3,
                             rel_spread_threshold = 0.1,
                             abs_spread_threshold = NULL,
                             compactify_tol = .Machine$double.eps^0.5) {
  arg <- names(eval_select(rlang::expr(c(...)), allow_rename = FALSE, data = epi_arch$DT))
  if (length(arg) == 0) {
    first_non_key <- !(names(epi_arch$DT) %in% c(key_colnames(epi_arch), "version"))
    arg <- names(epi_arch$DT)[first_non_key][1]
  } else if (length(arg) > 1) {
    cli_abort("Not currently implementing more than one column at a time. Run each separately")
  }
  if (is.null(abs_spread_threshold)) {
    abs_spread_threshold <- .05 * epi_arch$DT %>%
      pull(...) %>%
      max(na.rm = TRUE)
  }
  # for each time_value, get
  #   the number of revisions
  #   the maximum spread in value (both absolute and relative)
  #   the min lag
  #   the max lag
  #
  # revision_tibble
  keys <- key_colnames(epi_arch)
  names(epi_arch$DT)

  revision_behavior <-
    epi_arch$DT %>%
    select(c(geo_value, time_value, all_of(keys), version, !!arg))
  if (drop_nas) {
    # if we're dropping NA's, we should recompactify
    revision_behavior <-
      revision_behavior %>%
      filter(!is.na(c_across(!!arg))) %>%
      arrange(across(c(geo_value, time_value, all_of(keys), version))) %>% # need to sort before compactifying
      compactify_tibble(c(keys, version), compactify_tol)
  } else {
    revision_behavior <- epi_arch$DT
  }
  revision_behavior <-
    revision_behavior %>%
    mutate(lag = as.integer(version) - as.integer(time_value)) %>% # nolint: object_usage_linter
    group_by(across(all_of(keys))) %>% # group by all the keys
    summarize(
      n_revisions = dplyr::n() - 1,
      min_lag = min(lag), # nolint: object_usage_linter
      max_lag = max(lag), # nolint: object_usage_linter
      spread = spread_vec(pick(!!arg)),
      rel_spread = spread / max_no_na(pick(!!arg)), # nolint: object_usage_linter
      time_to = time_within_x_latest(lag, pick(!!arg), prop = within_latest), # nolint: object_usage_linter
      .groups = "drop"
    ) %>%
    mutate(
      # TODO the units here may be a problem
      min_lag = as.difftime(min_lag, units = "days"), # nolint: object_usage_linter
      max_lag = as.difftime(max_lag, units = "days"), # nolint: object_usage_linter
      time_near_latest = as.difftime(time_to, units = "days") # nolint: object_usage_linter
    ) %>%
    select(-time_to)
  if (print_inform) {
    cli_inform("Number of revisions:")
    cli_inform("Min lag (time to first version):")
    difftime_summary(revision_behavior$min_lag) %>% print()
    if (!drop_nas) {
      total_na <- epi_arch$DT %>%
        filter(is.na(c_across(!!arg))) %>% # nolint: object_usage_linter
        nrow()
      cli_inform("Fraction of all versions that are `NA`:")
      cli_li(num_percent(total_na, nrow(epi_arch$DT)))
    }
    total_num <- nrow(revision_behavior) # nolint: object_usage_linter
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
    rel_spread <- sum( # nolint: object_usage_linter
      real_revisions$rel_spread <
        rel_spread_threshold,
      na.rm = TRUE
    ) + sum(is.na(real_revisions$rel_spread))
    cli_inform("Less than {rel_spread_threshold} spread in relative value (only from the revised subset):")
    cli_li(num_percent(rel_spread, n_real_revised))
    na_rel_spread <- sum(is.na(real_revisions$rel_spread)) # nolint: object_usage_linter
    cli_inform("{units(quick_revision)} until within {within_latest*100}% of the latest value:")
    difftime_summary(revision_behavior[["time_near_latest"]]) %>% print()
    abs_spread <- sum( # nolint: object_usage_linter
      real_revisions$spread >
        abs_spread_threshold
    ) # nolint: object_usage_linter
    cli_inform("Spread of more than {abs_spread_threshold} in actual value (when revised):")
    cli_li(num_percent(abs_spread, n_real_revised))
  }
  return(revision_behavior)
}

#' pull the value from lags when values starts indefinitely being within prop of it's last value.
#' @param values this should be a 1 column tibble. errors may occur otherwise
#' @keywords internal
time_within_x_latest <- function(lags, values, prop = .2) {
  values <- values[[1]]
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
  length(bool_vec) - tail(runs$lengths, n = 1)
  values_from[[length(bool_vec) - tail(runs$lengths, n = 1) + 1]]
}

#' the default behavior returns a warning on empty lists, which we do not want,
#' and there is no super clean way of preventing this
#' @keywords internal
max_no_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(Inf)
  } else {
    return(max(x))
  }
}
#' the default behavior returns a warning on empty lists, which we do not want
#' @keywords internal
spread_vec <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(-Inf)
  } else {
    res <- x %>%
      range(na.rm = TRUE) %>%
      diff(na.rm = TRUE)
    return(res)
  }
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
  if (length(diff_time_val) > 0) {
    res <- data.frame(
      min = min(diff_time_val),
      median = median(diff_time_val),
      mean = round(mean(diff_time_val), 1),
      max = max(diff_time_val),
      row.names = " ",
      check.names = FALSE
    )
    return(res)
  } else {
    return(data.frame())
  }
}
