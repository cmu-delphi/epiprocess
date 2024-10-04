library(readr)
library(epidatr)
library(epiprocess)
library(dplyr)

y <- read_csv("https://github.com/cmu-delphi/covidcast/raw/c89e4d295550ba1540d64d2cc991badf63ad04e5/Python-packages/covidcast-py/covidcast/geo_mappings/county_census.csv", # nolint: line_length_linter
  col_types = cols(
    FIPS = col_character(),
    STNAME = col_character(),
    CTYNAME = col_character()
  )
) %>%
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
  inner_join(y, by = "geo_value", relationship = "many-to-one", unmatched = "error") %>%
  as_epi_df()

usethis::use_data(jhu_csse_county_level_subset, overwrite = TRUE)
