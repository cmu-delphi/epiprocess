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


#' Re-index, fill na, make sure all reference date have enough rows for updates
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' @param min_refd the earliest reference date considered in the data
#' @param max_refd the latest reference date considered in the data
#' 
#' @importFrom constants ref_lag
#' 
#' @return df_new Data Frame with filled rows for missing lags
#' 
#' @export
fill_rows <- function(df, refd_col, lag_col, min_refd, max_refd){
  lags <- min(df[[lag_col]]): ref_lag # Full list of lags
  refds <- seq(min_refd, max_refd, by="day") # Full list reference date
  row_inds_df <- as.data.frame(crossing(refds, lags)) %>%
    setNames(c(refd_col, lag_col))
  df_new = merge(x=df, y=row_inds_df,
                 by=c(refd_col, lag_col),  all.y=TRUE)
  return (df_new)
}

#' Get pivot table, filling NANs. If there is no update on issue date D but 
#' previous reports exist for issue date D_p < D, all the dates between
#' [D_p, D] are filled with with the reported value on date D_p. If there is 
#' no update for any previous issue date, fill in with 0.
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param value_col column name for the column of counts
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' 
#' @importFrom constants ref_lag
#' @importFrom tidyr fill
#' @importFrom dplyr everything, select
#' 
#' @export
fill_missing_updates <- function(df, value_col, refd_col, lag_col) {
  pivot_df <- df[order(df[[lag_col]], decreasing=FALSE), ] %>%
    pivot_wider(id_cols=lag_col, names_from=refd_col, values_from=value_col)
  if (any(diff(pivot_df[[lag_col]])!=1)){stop("Risk exists in forward fill")}
  pivot_df <- pivot_df %>% fill(everything(), .direction="down")
  pivot_df[is.na(pivot_df)] <- 0 # fill NAs with 0s
  backfill_df <- pivot_df %>%
    pivot_longer(-lag_col, values_to="value_raw", names_to=refd_col)
  backfill_df[[refd_col]] = as.Date(backfill_df[[refd_col]])
  return (as.data.frame(backfill_df))
}

#' Calculate 7 day moving average for each issue date
#' The 7dav for date D reported on issue date D_i is the average from D-7 to D-1
#' @param pivot_df Data Frame where the columns are issue dates and the rows are 
#'    reference dates
#' @param refd_col column name for the column of reference date
#' 
#' @importFrom zoo rollmeanr
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

#' Used for data shifting in terms of reference date
#' 
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param n_day number of days to be shifted
#' @param refd_col column name for the column of reference date
#' 
#' @export
add_shift <- function(df, n_day, refd_col){
  df[, refd_col] <- as.Date(df[, refd_col]) + n_day
  return (df)
}

#' Add 7dav and target to the data
#' Target is the updates made ref_lag days after the first release
#' @param df Data Frame of aggregated counts within a single location 
#'    reported for each reference date and issue date.
#' @param value_col column name for the column of raw value
#' @param refd_col column name for the column of reference date
#' @param lag_col column name for the column of lag
#' 
#' @export
add_7davs_and_target <- function(df, value_col, refd_col, lag_col){
  
  df$issue_date <- df[[refd_col]] + df[[lag_col]]
  pivot_df <- df[order(df$issue_date, decreasing=FALSE), ] %>%
    pivot_wider(id_cols=refd_col, names_from="issue_date", 
                values_from=value_col)
  
  # Add 7dav avg
  avg_df <- get_7dav(pivot_df, refd_col)
  avg_df <- add_shift(avg_df, 1, refd_col) # 7dav until yesterday
  names(avg_df)[names(avg_df) == 'value_raw'] <- 'value_7dav'
  avg_df_prev7 <- add_shift(avg_df, 7, refd_col)
  names(avg_df_prev7)[names(avg_df_prev7) == 'value_7dav'] <- 'value_prev_7dav'
  
  backfill_df <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                        list(df, avg_df, avg_df_prev7))
  
  # Add target
  target_df <- df[df$lag==ref_lag, ] %>% select(c(refd_col, "value_raw", "issue_date"))
  names(target_df)[names(target_df) == 'value_raw'] <- 'value_target'
  names(target_df)[names(target_df) == 'issue_date'] <- 'target_date'
  
  backfill_df <- merge(backfill_df, target_df, by=refd_col, all.x=TRUE)
  
  # Remove invalid rows
  backfill_df <- backfill_df %>% drop_na(c(lag_col))
  
  return (as.data.frame(backfill_df))
}
