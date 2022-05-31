library(delphi.epidata)
library(epiprocess)
library(data.table)
library(dplyr)

dv <- covidcast(
  data_source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20211101, 20211201),
  geo_values = "ca",
  issues = epirange(20211129, 20211129)
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, percent_cli = value)

dv_duplicated <- dv
for (i in 1:5) {
  dv_duplicated[i,4] <- 6
}

dv_true <- as_tibble(as_epi_archive(dv_duplicated,compactify=TRUE)$DT)
dv_false <- as_tibble(as_epi_archive(dv_duplicated,compactify=FALSE)$DT)
dv_null <- as_tibble(as_epi_archive(dv_duplicated,compactify=NULL)$DT)

test_that("LOCF values are ignored", {
  expect_identical(nrow(dv_duplicated),nrow(dv_false))
})

test_that("LOCF values are taken out", {
  dv_unique <- distinct(dv_false,percent_cli,.keep_all = TRUE)
  expect_identical(dv_true,dv_null)
  expect_identical(dv_null,dv_unique)
})