library(epidatr)
library(covidcast)
library(epiprocess)
library(dplyr)

# Use covidcast::county_census to get the county and state names
y <- covidcast::county_census %>%
  filter(STNAME %in% c("Massachusetts", "Vermont"), STNAME != CTYNAME) %>%
  select(geo_value = FIPS, county_name = CTYNAME, state_name = STNAME)

# Fetch only counties from Massachusetts and Vermont, then append names columns as well
jhu_csse_county_level_subset <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_incidence_num",
  geo_type = "county",
  time_type = "day",
  geo_values = paste(y$geo_value, collapse = ","),
  time_values = epirange(20200601, 20211231),
) %>%
  select(geo_value, time_value, cases = value) %>%
  full_join(y, by = "geo_value") %>%
  as_epi_df()

usethis::use_data(jhu_csse_county_level_subset, overwrite = TRUE)
