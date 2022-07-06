library(delphi.epidata)
library(epiprocess)
library(data.table)
library(dplyr)

archive_cases_dv_subset <- covidcast(
  data_source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211201),
  geo_values = "ca,fl,ny,tx",
  issues = epirange(20200601, 20211201)
) %>% 
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, percent_cli = value) %>%
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
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, case_rate_7d_av = value) %>%
  as_epi_archive(compactify=FALSE)

epix_merge(archive_cases_dv_subset, case_rate_subset, all = TRUE)

archive_cases_dv_subset_dt = archive_cases_dv_subset$DT

usethis::use_data(archive_cases_dv_subset_dt, overwrite = TRUE, internal = TRUE)
