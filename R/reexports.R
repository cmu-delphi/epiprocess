# NOTE: when providing a method for a generic in another package
# That generic needs to be rexported


# tsibble -----------------------------------------------------------------

#' @importFrom tsibble as_tsibble
#' @export
tsibble::as_tsibble


# dplyr -------------------------------------------------------------------

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @importFrom dplyr ungroup
#' @export
dplyr::ungroup

#' @importFrom dplyr group_modify
#' @export
dplyr::group_modify

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr relocate
#' @export
dplyr::relocate

#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @importFrom dplyr slice
#' @export
dplyr::slice


# tidyr -------------------------------------------------------------------

#' @importFrom tidyr unnest
#' @export
tidyr::unnest


# epidatasets -------------------------------------------------------------------

#' @inherit epidatasets::cases_deaths_subset
#' @examples
#'  # Since this is a re-exported dataset, it cannot be loaded using
#'  # the `data()` function. `data()` looks for a file of the same name
#'  # in the `data/` directory, which doesn't exist in this package.
#'  # works
#'  epiprocess::cases_deaths_subset
#'
#'  # works
#'  library(epiprocess)
#'  cases_deaths_subset
#'
#'  # fails
#'  data(cases_deaths_subset, package = "epiprocess")
#' @export
cases_deaths_subset <- epidatasets::cases_deaths_subset

#' @inherit epidatasets::archive_cases_dv_subset_dt
#' @examples
#'  # Since this is a re-exported dataset, it cannot be loaded using
#'  # the `data()` function. `data()` looks for a file of the same name
#'  # in the `data/` directory, which doesn't exist in this package.
#'  # works
#'  epiprocess::archive_cases_dv_subset_dt
#'
#'  # works
#'  library(epiprocess)
#'  archive_cases_dv_subset_dt
#'
#'  # fails
#'  data(archive_cases_dv_subset_dt, package = "epiprocess")
#' @export
archive_cases_dv_subset_dt <- epidatasets::archive_cases_dv_subset_dt

#' @inherit epidatasets::covid_incidence_county_subset
#' @examples
#'  # Since this is a re-exported dataset, it cannot be loaded using
#'  # the `data()` function. `data()` looks for a file of the same name
#'  # in the `data/` directory, which doesn't exist in this package.
#'  # works
#'  epiprocess::covid_incidence_county_subset
#'
#'  # works
#'  library(epiprocess)
#'  covid_incidence_county_subset
#'
#'  # fails
#'  data(covid_incidence_county_subset, package = "epiprocess")
#' @export
covid_incidence_county_subset <- epidatasets::covid_incidence_county_subset

#' @inherit epidatasets::covid_incidence_outliers
#' @examples
#'  # Since this is a re-exported dataset, it cannot be loaded using
#'  # the `data()` function. `data()` looks for a file of the same name
#'  # in the `data/` directory, which doesn't exist in this package.
#'  # works
#'  epiprocess::covid_incidence_outliers
#'
#'  # works
#'  library(epiprocess)
#'  covid_incidence_outliers
#'
#'  # fails
#'  data(covid_incidence_outliers, package = "epiprocess")
#' @export
covid_incidence_outliers <- epidatasets::covid_incidence_outliers

#' @inherit epidatasets::jhu_confirmed_cumulative_num
#' @examples
#'  # Since this is a re-exported dataset, it cannot be loaded using
#'  # the `data()` function. `data()` looks for a file of the same name
#'  # in the `data/` directory, which doesn't exist in this package.
#'  # works
#'  epiprocess::jhu_confirmed_cumulative_num
#'
#'  # works
#'  library(epiprocess)
#'  jhu_confirmed_cumulative_num
#'
#'  # fails
#'  data(jhu_confirmed_cumulative_num, package = "epiprocess")
#' @export
jhu_confirmed_cumulative_num <- epidatasets::jhu_confirmed_cumulative_num
