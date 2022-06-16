library(delphi.epidata)
library(epiprocess)
library(data.table)
library(dplyr)

archive_cases_dv <- covidcast(
  data_source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  geo_type = "state",
  time_value = epirange(20200601, 20200615),
  geo_values = "ca,fl",
  issues = epirange(20200601, 20200615)
) %>% 
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, percent_cli = value) %>%
  as_epi_archive(compactify=TRUE)

case_rate <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_value = epirange(20200601, 20200615),
  geo_values = "ca,fl",
  issues = epirange(20200601, 20200615)
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, case_rate = value) %>%
  as_epi_archive(compactify=TRUE)

epix_merge(archive_cases_dv, case_rate, all = TRUE)

usethis::use_data(archive_cases_dv, overwrite = TRUE)
