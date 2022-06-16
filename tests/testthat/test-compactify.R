library(epiprocess)
library(data.table)
library(dplyr)

dt <- archive_cases_dv$DT
test_that("Input for compactify must be NULL or a boolean", {
  expect_error(as_epi_archive(dv_duplicated,compactify="no"))
})
  
dt <- filter(dt,geo_value == "ca")
dt$percent_cli <- c(1:80)
dt$case_rate <- c(1:80)

row_replace <- function(row,x,y) {
  dt[row,4] <- x
  dt[row,5] <- y
  dt
}

# Rows 1 should not be eliminated even if NA
dt <- row_replace(1,NA,NA) # Not LOCF

# Rows 11 and 12 correspond to different time_values
dt <- row_replace(12,11,11) # Not LOCF

# Rows 20 and 21 only differ in version
dt <- row_replace(21,20,20) # LOCF

# Rows 21 and 22 only differ in version
dt <- row_replace(22,20,20) # LOCF

# Row 39 comprises the first NA's
dt <-row_replace(39,NA,NA) # Not LOCF

# Row 40 has two NA's, just like its lag, row 39
dt <- row_replace(40,NA,NA) # LOCF

# Row 62's values already exist in row 15, but row 15 is not a preceding row
dt <- row_replace(62,15,15) # Not LOCF

# Row 73 only has one value carried over
dt <- row_replace(74,73,74) # Not LOCF

dt_true <- as_tibble(as_epi_archive(dt,compactify=TRUE)$DT)
dt_false <- as_tibble(as_epi_archive(dt,compactify=FALSE)$DT)
dt_null <- as_tibble(as_epi_archive(dt,compactify=NULL)$DT)

test_that("Warning for LOCF with compactify as NULL", {
  expect_warning(as_epi_archive(dt,compactify=NULL))
})

test_that("No warning when there is no LOCF", {
  expect_warning(as_epi_archive(dt[1:10,],compactify=NULL),NA)
})

test_that("LOCF values are ignored with compactify=FALSE", {
  expect_identical(nrow(dt),nrow(dt_false))
})

test_that("LOCF values are taken out with compactify=TRUE", {
  dt_test <- as_tibble(as_epi_archive(dt[-c(21,22,40),],compactify=FALSE)$DT)
  
  expect_identical(dt_true,dt_null)
  expect_identical(dt_null,dt_test)
})

dt2 <- dt
dt2$percent_cli <- 1
dt2$case_rate <- 1

as_tibble(as_epi_archive(dt2,compactify=NULL)$DT)

test_that("as_of works correctly",{
  
})
