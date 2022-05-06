 library(delphi.epidata)
 library(epiprocess)
 library(dplyr)
 
 # get data as an `epi_df` object 
x <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20200501),
  geo_values = "ca,fl"
  ) %>%
   fetch_tbl() %>%
   select(geo_value, time_value, case_rate = value)

  y <- covidcast(
  data_source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20200501),
  geo_values = "ca,fl"
  ) %>%
   fetch_tbl() %>%
   select(geo_value, time_value, death_rate = value)

ca_fl_death_rate_and_cases <- x %>%
  full_join(y, by = c("geo_value", "time_value")) %>%
  as_epi_df()

usethis::use_data(ca_fl_death_rate_and_cases, overwrite = TRUE)
