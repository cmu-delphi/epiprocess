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

#' @importFrom epidatasets cases_deaths_subset
#' @export
epidatasets::cases_deaths_subset

#' @importFrom epidatasets archive_cases_dv_subset_dt
#' @export
epidatasets::archive_cases_dv_subset_dt

#' @importFrom epidatasets covid_incidence_county_subset
#' @export
epidatasets::covid_incidence_county_subset

#' @importFrom epidatasets covid_incidence_outliers
#' @export
epidatasets::covid_incidence_outliers

#' @importFrom epidatasets jhu_confirmed_cumulative_num
#' @export
epidatasets::jhu_confirmed_cumulative_num
