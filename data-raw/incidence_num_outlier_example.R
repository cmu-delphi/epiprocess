library(epidatr)
library(epiprocess)
library(dplyr)
library(tidyr)

incidence_num_outlier_example <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_incidence_num",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20210531),
  geo_values = "fl,nj",
  as_of = 20211028
) %>%
  fetch() %>%
  select(geo_value, time_value, cases = value) %>%
  as_epi_df()

usethis::use_data(incidence_num_outlier_example, overwrite = TRUE)
