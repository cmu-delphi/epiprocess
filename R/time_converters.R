#' convert an archive from daily data to weekly data, summing where appropriate
#' @details
#' this function is slow, so make sure you are calling it correctly, and
#'   consider testing it on a small portion of your archive first
#' @param day_of_week integer, day of the week, starting from Sunday, select the
#'   date to represent the week in the time_value column, based on it's
#'   corresponding day of the week. The default value represents the week using
#'   Wednesday.
#' @param day_of_week_end integer, day of the week starting on Sunday.
#'   Represents the last day, so the week consists of data summed to this day.
#'   The default value `6` means that the week is summed from Sunday through
#'   Saturday.
daily_to_weekly <- function(epi_arch,
                            agg_columns,
                            agg_method = c("total", "mean", "median"),
                            day_of_week = 4L,
                            day_of_week_end = 6L) {
  keys <- grep("time_value", key_colnames(epi_arch), invert = TRUE, value = TRUE)
  too_many_tibbles <- epix_slide(
    epi_arch,
    before = 99999999L,
    ref_time_values = ref_time_values,
    function(x, group, ref_time) {
      x %>%
        group_by(across(all_of(keys))) %>%
        epi_slide_sum(agg_columns, before = 6L) %>%
        select(-all_of(agg_columns)) %>%
        rename_with(~ gsub("slide_value_", "", .x)) %>%
        # only keep 1/week
        filter(wday(time_value) == day_of_week_end) %>%
        # switch time_value to the designated day of the week
        mutate(time_value = time_value - 7L + day_of_week) %>%
        as_tibble()
    }
  )
  too_many_tibbles %>%
    rename(version = time_value) %>%
    rename_with(~ gsub("slide_value_", "", .x)) %>%
    as_epi_archive(compactify = TRUE)
}



#' fill in values between, and divide any numeric values equally between them
#' @param to_complete epi_archive
#' @param groups to be grouped by. Should include both time_value and version, and any epi_keys
#' @param columns_to_complete any columns that need their values extended
#' @param aggregate_columns any columns which have numerical data that is a sum
#'   across days, and thus needs to be divided into equal parts distributed
#'   accross days
convert_to_period_upsample <- function(to_complete, groups, columns_to_complete,
                                       aggregate_columns, source_period = 7, target_period = 1) {
  to_complete_datatable <- to_complete$DT
  completed_time_values <-
    to_complete_datatable %>%
    group_by(across(all_of(groups))) %>%
    reframe(
      time_value = seq(from = time_value, to = time_value + source_period - 1, by = target_period)
    ) %>%
    unique()
  completed <- to_complete_datatable %>%
    full_join(
      completed_time_values,
      by = join_by(season, geo_value, version, time_value)
    ) %>%
    arrange(geo_value, version, time_value) %>%
    fill(all_of(columns_to_complete), .direction = "down") %>%
    mutate(across(all_of(aggregate_columns), \(x) x / 7))
  completed %>%
    arrange(geo_value, time_value) %>%
    as_epi_archive(compactify = TRUE)
}
