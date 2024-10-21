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

#' @inherit epidatasets::cases_deaths_subset description source references title
#' @inheritSection epidatasets::cases_deaths_subset Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epiprocess::cases_deaths_subset
#'
#' # works
#' library(epiprocess)
#' cases_deaths_subset
#'
#' # fails
#' \dontrun{
#' data(cases_deaths_subset, package = "epiprocess")
#' }
#' @export
delayedAssign("cases_deaths_subset", epidatasets::cases_deaths_subset)

#' @inherit epidatasets::covid_incidence_county_subset description source references title
#' @inheritSection epidatasets::covid_incidence_county_subset Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epiprocess::covid_incidence_county_subset
#'
#' # works
#' library(epiprocess)
#' covid_incidence_county_subset
#'
#' # fails
#' \dontrun{
#' data(covid_incidence_county_subset, package = "epiprocess")
#' }
#' @export
delayedAssign("covid_incidence_county_subset", epidatasets::covid_incidence_county_subset)

#' @inherit epidatasets::covid_incidence_outliers description source references title
#' @inheritSection epidatasets::covid_incidence_outliers Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epiprocess::covid_incidence_outliers
#'
#' # works
#' library(epiprocess)
#' covid_incidence_outliers
#'
#' # fails
#' \dontrun{
#' data(covid_incidence_outliers, package = "epiprocess")
#' }
#' @export
delayedAssign("covid_incidence_outliers", epidatasets::covid_incidence_outliers)

#' @inherit epidatasets::archive_cases_dv_subset description source references title
#' @inheritSection epidatasets::archive_cases_dv_subset Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epiprocess::archive_cases_dv_subset
#'
#' # works
#' library(epiprocess)
#' archive_cases_dv_subset
#'
#' # fails
#' \dontrun{
#' data(archive_cases_dv_subset, package = "epiprocess")
#' }
#'
#' @export
delayedAssign("archive_cases_dv_subset", epidatasets::archive_cases_dv_subset)

#' @inherit epidatasets::covid_case_death_rates_extended description source references title
#' @inheritSection epidatasets::covid_case_death_rates_extended Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epiprocess::covid_case_death_rates_extended
#'
#' # works
#' library(epiprocess)
#' covid_case_death_rates_extended
#'
#' # fails
#' \dontrun{
#' data(covid_case_death_rates_extended, package = "epiprocess")
#' }
#'
#' @export
delayedAssign("covid_case_death_rates_extended", epidatasets::covid_case_death_rates_extended)
