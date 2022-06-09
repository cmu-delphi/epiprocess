library(epiprocess)
library(data.table)
library(dplyr)

dt <- filter(dv$DT,geo_value=="ca")
dt <- select(dt,-case_rate)

test_that("Input for compactify must be NULL or a boolean", {
  expect_error(as_epi_archive(dv_duplicated,compactify="no"))
})
  
dv_true <- as_tibble(as_epi_archive(dt,compactify=TRUE)$DT)
dv_false <- as_tibble(as_epi_archive(dt,compactify=FALSE)$DT)
dv_null <- as_tibble(as_epi_archive(dt,compactify=NULL)$DT)

test_that("Warning for LOCF with compactify as NULL", {
  expect_warning(as_epi_archive(dt,compactify=NULL))
})

test_that("LOCF values are ignored", {
  expect_identical(nrow(dt),nrow(dv_false))
})

test_that("LOCF values are taken out", {
  dv_unique <- distinct(dv_false,percent_cli,.keep_all = TRUE)
  expect_identical(dv_true,dv_null)
  expect_identical(dv_null,dv_unique)
})