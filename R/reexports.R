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


#' @importFrom tidyr complete
#' @export
tidyr::complete

# We don't provide a method for full_seq, but complete-ing using
# full_seq(time_value) is still needed to make some downstream things behave
# nicely.  So make that more ergonomic/discoverable with a re-export:

#' @importFrom tidyr full_seq
#' @export
tidyr::full_seq


# ggplot2 -----------------------------------------------------------------

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


# epidatasets -------------------------------------------------------------------

#' @export
delayedAssign("cases_deaths_subset", epidatasets::cases_deaths_subset)

#' @export
delayedAssign("covid_incidence_county_subset", epidatasets::covid_incidence_county_subset)

#' @export
delayedAssign("covid_incidence_outliers", epidatasets::covid_incidence_outliers)

#' @export
delayedAssign("archive_cases_dv_subset", epidatasets::archive_cases_dv_subset)

#' @export
delayedAssign("covid_case_death_rates_extended", epidatasets::covid_case_death_rates_extended)
