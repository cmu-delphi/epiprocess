library(epidatr)
library(epiprocess)
library(data.table)
library(dplyr)

dv_subset <- covidcast(
  data_source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211201),
  geo_values = "ca,fl,ny,tx",
  issues = epirange(20200601, 20211201)
) %>%
  fetch() %>%
  select(geo_value, time_value, version = issue, percent_cli = value) %>%
  # We're using compactify=FALSE here and below to avoid some testthat test
  # failures on tests that were based on a non-compactified version.
  as_epi_archive(compactify=FALSE)

case_rate_subset <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211201),
  geo_values = "ca,fl,ny,tx",
  issues = epirange(20200601, 20211201)
) %>%
  fetch() %>%
  select(geo_value, time_value, version = issue, case_rate_7d_av = value) %>%
  as_epi_archive(compactify=FALSE)

archive_cases_dv_subset = epix_merge(dv_subset, case_rate_subset,
                                     sync="locf",
                                     compactify=FALSE)

# If we directly store an epi_archive R6 object as data, it will store its class
# implementation there as well. To prevent mismatches between these stored
# implementations and the latest class definition, don't store them as R6
# objects; store the DT and construct the R6 object on request.
archive_cases_dv_subset_dt = archive_cases_dv_subset$DT
usethis::use_data(archive_cases_dv_subset_dt, overwrite = TRUE, internal = TRUE)
