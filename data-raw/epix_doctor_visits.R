library(delphi.epidata)
library(epiprocess)
library(data.table)
library(dplyr)

epix_doctor_visits <- covidcast(
  data_source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  geo_type = "state",
  time_value = epirange(20200601, 20200701),
  geo_value = "ca,fl",
  issues = epirange(20200601, 20200701)
) %>% 
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, percent_cli = value)
  

usethis::use_data(epix_doctor_visits, overwrite = TRUE)
