library(epiprocess)
library(data.table)
library(dplyr)

dt <- select(archive_cases_dv$DT,-case_rate)

head(dt)
arrange(dt,geo_value,time_value,version) %>% head()

dt_full <- archive_cases_dv$DT

test_that("Input for compactify must be NULL or a boolean", {
  expect_error(as_epi_archive(dv_duplicated,compactify="no"))
})
  
dt_true <- as_tibble(as_epi_archive(dt,compactify=TRUE)$DT)
dt_false <- as_tibble(as_epi_archive(dt,compactify=FALSE)$DT)
dt_null <- as_tibble(as_epi_archive(dt,compactify=NULL)$DT)

test_that("Warning for LOCF with compactify as NULL", {
  expect_warning(as_epi_archive(dt,compactify=NULL))
})

test_that("LOCF values are ignored with compactify=FALSE", {
  expect_identical(nrow(dt),nrow(dt_false))
})

test_that("LOCF values are taken out with compactify=TRUE", {
  dt_unique <- dt_false %>%
    distinct(percent_cli,.keep_all = TRUE) %>%
    filter(!is.na(percent_cli))
    
  expect_identical(dt_true,dt_null)
  expect_identical(dt_null,dt_unique)
})