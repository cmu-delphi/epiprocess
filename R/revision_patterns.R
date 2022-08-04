### Data Preprocessing
### The raw input data should have 4/5 basic columns: 
### time_value: reference date
### issue_date: issue date/date of reporting
### geo_value: location
### lag: the number of days between issue date and the reference date
### counts: the number of counts used for estimation
### library(lubridate)
### library(stats)
### library(stats)
### library(dyplr)
### library(tidyverse)

#' Create empty obs for every sequentially missing date-lag combo
#'
#' Make sure all reference date have enough rows for updates. Re-index
#' dataframe to insert a row for each missing date between `min_refd`
#' and `max_refd` for every requested lag from 1 to `ref_lag`, filling with `NA`.
#'
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' @param min_refd the earliest reference date considered in the data
#' @param max_refd the latest reference date considered in the data
#' @param ref_lag the maximum lag value through which to complete
#' 
#' @return df_new Data Frame with filled rows for missing lags
#'
#' @importFrom tidyr crossing
#' 
#' @export
fill_rows <- function(df, refd_col, lag_col, min_refd, max_refd, ref_lag){
  lags <- min(df[[lag_col]]): ref_lag # Full list of lags
  refds <- seq(min_refd, max_refd, by="day") # Full list reference date
  row_inds_df <- as.data.frame(crossing(refds, lags)) %>%
    setNames(c(refd_col, lag_col))
  df_new = merge(x=df, y=row_inds_df,
                 by=c(refd_col, lag_col),  all.y=TRUE)
  return (df_new)
}

#' Perform LOCF to fill missing values in issues
#'
#' Perform LOCF to fill `NA`s if a group is missing from an issue but was
#' previously available. If there is no update on issue date \eqn{D} but
#' previous reports exist for issue date \eqn{D_p} < \eqn{D}, all the dates
#' between \eqn{[D_p, D]} are filled with the value reported on date \eqn{D_p}.
#' If there is no update for any previous issue date, fill in with 0.
#'
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param value_col column name for the column of counts
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' 
#' @importFrom tidyr fill pivot_wider pivot_longer
#' @importFrom dplyr everything select
#' 
#' @export
fill_missing_updates <- function(df, value_col, refd_col, lag_col) {
  pivot_df <- df[order(df[[lag_col]], decreasing=FALSE), ] %>%
    pivot_wider(id_cols=lag_col, names_from=refd_col, values_from=value_col)
  if (any(diff(pivot_df[[lag_col]])!=1)){Abort("Risk exists in forward fill")}
  pivot_df <- pivot_df %>% fill(everything(), .direction="down")
  pivot_df[is.na(pivot_df)] <- 0 # fill NAs with 0s
  backfill_df <- pivot_df %>%
    pivot_longer(-lag_col, values_to="value_raw", names_to=refd_col)
  backfill_df[[refd_col]] = as.Date(backfill_df[[refd_col]])
  return (as.data.frame(backfill_df))
}

#' Calculate 7 day moving average for each issue date
#'
#' The 7-day average for date \eqn{D} reported on issue date \eqn{D_i} uses data
#' from \eqn{D-7} to \eqn{D-1}.
#'
#' @param pivot_df Data Frame where the columns are issue dates and the rows are 
#'    reference dates
#' @param refd_col column name for the column of reference date
#' 
#' @importFrom zoo rollmeanr
#' @importFrom tidyr pivot_longer
#' 
#' @export
get_7dav <- function(pivot_df, refd_col){
  for (col in colnames(pivot_df)){
    if (col == refd_col) next
    pivot_df[, col] <- rollmeanr(pivot_df[, col], 7, align="right", fill=NA)
  }
  backfill_df <- pivot_df %>%
    pivot_longer(-refd_col, values_to="value_raw", names_to="issue_date")
  backfill_df[[refd_col]] = as.Date(backfill_df[[refd_col]])
  backfill_df[["issue_date"]] = as.Date(backfill_df[["issue_date"]])
  return (as.data.frame(backfill_df))
}

#' Shift reference dates by `n` days, keeping all other columns the same.
#' 
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param n_day number of days to be shifted. A positive value corresponds to
#'     a shift forward in time, negative shifts dates backwards in time.
#' @param refd_col column name for the column of reference date
#' 
#' @export
add_shift <- function(df, n_day, refd_col){
  df[, refd_col] <- as.Date(df[, refd_col]) + n_day
  return (df)
}

#' Add 7-day moving average and prediction target to a dataframe
#'
#' Each row must be uniquely identified by a reference date + lag combination.
#' Issue dates are not required and are regenerated from the reference date and
#' lag fields.
#'
#' Targets are updates made `ref_lag` days after the first release.
#'
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param value_col column name for the column of raw value
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' @param ref_lag target lag
#' 
#' @importFrom tidyr pivot_wider
#'
#' @export
add_7davs_and_target <- function(df, value_col, refd_col, lag_col, ref_lag){
  
  df$issue_date <- df[[refd_col]] + df[[lag_col]]
  pivot_df <- df[order(df$issue_date, decreasing=FALSE), ] %>%
    pivot_wider(id_cols=refd_col, names_from="issue_date", 
                values_from=value_col)
  
  # Add 7dav avg
  avg_df <- get_7dav(pivot_df, refd_col)
  # 7dav until yesterday
  avg_df <- add_shift(avg_df, 1, refd_col) %>%
    rename(value_7dav = value_raw)
  avg_df_prev7 <- add_shift(avg_df, 7, refd_col) %>%
    rename(value_prev_7dav = value_7dav)

  backfill_df <- Reduce(function(x, y) merge(x, y, all=TRUE),
                        list(df, avg_df, avg_df_prev7))

  # Add target
  target_df <- df[df$lag==ref_lag, ] %>%
    select(c(refd_col, value_col, "issue_date")) %>%
    rename(value_target = "value_raw", target_date = issue_date)
  
  backfill_df <- merge(backfill_df, target_df, by=refd_col, all.x=TRUE)
  
  # Remove invalid rows
  backfill_df <- backfill_df %>% drop_na(c(lag_col))
  
  return (as.data.frame(backfill_df))
}
