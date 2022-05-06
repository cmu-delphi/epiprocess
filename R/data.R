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



#' California and Florida daily percentage of doctor’s visits with COVID-like illness.
#'
#' An epi_archive dataset containing the daily COVID-19 percentage of doctor’s visits with CLI (COVID-like illness) 
#' computed from medical insurance claims from June 1, 2020 to July 1, 2020.
#'
#' @format A tibble with 718 rows and 4 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{version}{the time value specifying the version for each row of measurements. For example, if in a given row the version is January 15, 2022 and time_value is January 14, 2022, then this row contains the measurements of the data for January 14, 2022 that were available one day later.}
#'   \item{percent_cli}{percentage of doctor’s visits with CLI (COVID-like illness) computed from medical insurance claims}
#'   ...
#' }
#' @source \url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html}
"epix_doctor_visits"
