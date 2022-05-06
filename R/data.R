#' California daily COVID-19 new cases.
#'
#' A dataset containing the daily COVID-19 new cases 
#' from March 1, 2020 to May 1, 2020.
#'
#' @format A tibble with 62 rows and 3 variables:
#' \describe{
#'   \item{geo_value}{location such as the state name}
#'   \item{time_value}{date}
#'   \item{cases}{number of new confirmed COVID-19 cases, daily}
#'   ...
#' }
#' @source \url{https://cmu-delphi.github.io/delphi-epidata/}
"ca_daily_cases"

#' California and Florida Daily COVID-19 incidence rate and death Rates.
#'
#' A dataset containing the daily COVID-19 incidence rates and death rates 
#' from March 1, 2020 to May 1, 2020.
#'
#' @format A tibble with 124 rows and 4 variables:
#' \describe{
#'   \item{geo_value}{location such as the state name}
#'   \item{time_value}{date}
#'   \item{case_rate}{number of new confirmed COVID-19 cases per 100,000 population, daily}
#'   \item{death_rate}{number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#'   ...
#' }
#' @source \url{https://cmu-delphi.github.io/delphi-epidata/}
"ca_fl_death_rate_and_cases"