library(delphi.epidata)
library(epiprocess)
library(dplyr)

ca_daily_cases <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_incidence_num",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20200501),
  geo_values = "ca"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, cases = value) %>%
  arrange(geo_value, time_value) %>%
  as_epi_df()

usethis::use_data(ca_daily_cases, overwrite = TRUE)
