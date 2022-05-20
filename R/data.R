#' Subset of JHU daily cases and deaths from California, Florida, Texas, and New York
#'
#' This data source of confirmed COVID-19 cases and deaths 
#' is based on reports made available by the Center for 
#' Systems Science and Engineering at Johns Hopkins University. 
#' This example data ranges from June 1 to June 15, 2020 and is limited to California, Florida, Texas, and New York.
#'
#' @format A tibble with 60 rows and 6 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{case_rate_7d_av}{7-day average signal of number of new confirmed COVID-19 cases per 100,000 population, daily}
#'   \item{death_rate_7d_av}{7-day average signal of number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#'   \item{cases_7d_av}{7-day average signal of number of new confirmed COVID-19 cases, daily}
#' }
#' @source This data set is licensed under the terms of the 
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license} 
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering. 
#' Copyright Johns Hopkins University 2020.
#' 
#' * \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository}
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{COVIDcast Epidata JHU Cases and Death}
#' 
#' The example data is a filtered time view of the full data, and is also limited to California, Florida, Texas, and New York.
"jhu_csse_daily_subset"


#' Subset of daily doctor visits and cases from California, Florida, Texas, and New York in archive format
#'
#' This data source is based on information about outpatient visits, 
#' provided to us by health system partners, and also contains confirmed 
#' COVID-19 cases based on reports made available by the Center for 
#' Systems Science and Engineering at Johns Hopkins University. 
#' This example data ranges from June 1 to June 15, 2020. 
#'
#' @format An `epi_archive` data format. The data table DT has 160 rows and 5 columns:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{version}{ the time value specifying the version for each row of measurements. }
#'   \item{percent_cli}{percentage of doctor’s visits with CLI (COVID-like illness) computed from medical insurance claims}
#'   \item{case_rate}{7-day average signal of number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#' }
#' @source 
#' This data set is licensed under the terms of the 
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license} 
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering. 
#' Copyright Johns Hopkins University 2020.
#' 
#' * \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository}
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html}{COVIDcast Epidata Doctor Visits}
#' 
#' The example data is a filtered time view of the full data, and is also limited to California, Florida, Texas, and New York.
"archive_cases_dv_subset"

