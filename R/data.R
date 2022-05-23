#' Subset of JHU daily cases and deaths from California, Florida, Texas, New York, Georgia, and Pennsylvania
#'
#' This data source of confirmed COVID-19 cases and deaths 
#' is based on reports made available by the Center for 
#' Systems Science and Engineering at Johns Hopkins University. 
#' This example data ranges from Mar 1, 2020 to Dec 31, 2021, and is limited to California, Florida, Texas, New York, Georgia, and Pennsylvania.
#'
#' @format A tibble with 4026 rows and 6 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{case_rate_7d_av}{7-day average signal of number of new confirmed COVID-19 cases per 100,000 population, daily}
#'   \item{death_rate_7d_av}{7-day average signal of number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#'   \item{cases_7d_av}{7-day average signal of number of new confirmed COVID-19 cases, daily}
#' }
#' @source This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#' 
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}: These signals are taken directly from the JHU CSSE \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} without changes. The 7-day average signals are computed by Delphi by calculating moving averages of the preceding 7 days, so the signal for June 7 is the average of the underlying data for June 1 through 7, inclusive.
#' * Furthermore, the data has been limited to a very small number of rows, the signal names slightly altered, and formatted into a tibble.
"jhu_csse_daily_subset"


#' Subset of daily doctor visits and cases from California, Florida, Texas, and New York in archive format
#'
#' This data source is based on information about outpatient visits, 
#' provided to us by health system partners, and also contains confirmed 
#' COVID-19 cases based on reports made available by the Center for 
#' Systems Science and Engineering at Johns Hopkins University. 
#' This example data ranges from June 1 to June 15, 2020, and is also limited to California, Florida, Texas, and New York.
#'
#' @format An `epi_archive` data format. The data table DT has 160 rows and 5 columns:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{version}{the time value specifying the version for each row of measurements. }
#'   \item{percent_cli}{percentage of doctor’s visits with CLI (COVID-like illness) computed from medical insurance claims}
#'   \item{case_rate_7d_av}{7-day average signal of number of new confirmed deaths due to COVID-19 per 100,000 population, daily}
#' }
#' @source 
#' This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#' 
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html}{From the COVIDcast Epidata Doctor Visits API}: These signals are taken directly from the JHU CSSE \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} without changes. The 7-day average signals are computed by Delphi by calculating moving averages of the preceding 7 days, so the signal for June 7 is the average of the underlying data for June 1 through 7, inclusive.
#' * Furthermore, the data has been limited to a very small number of rows, the signal names slightly altered, and formatted into a tibble.
"archive_cases_dv_subset"


#' Subset of JHU daily cases from California and Florida
#' 
#' This data source of confirmed COVID-19 cases 
#' is based on reports made available by the Center for 
#' Systems Science and Engineering at Johns Hopkins University. 
#' This example data is a snapshot as of Oct 28, 2021 and captures the cases from June 1, 2020 to May 31, 2021 
#' and is limited to California and Florida.
#' 
#' @format A tibble with 730 rows and 3 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{cases}{Number of new confirmed COVID-19 cases, daily}
#' }
#' @source This object contains a modified part of the \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University} as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}. This data set is licensed under the terms of the
#' \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#' by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering.
#' Copyright Johns Hopkins University 2020.
#' 
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}: 
#' These signals are taken directly from the JHU CSSE \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository} without changes. 
#' * Furthermore, the data has been limited to a very small number of rows, the signal names slightly altered, and formatted into a tibble.
"incidence_num_outlier_example"

#' TO-DO
"jhu_csse_county_level_subset"