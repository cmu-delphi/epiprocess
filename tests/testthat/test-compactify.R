library(epiprocess)
library(data.table)
library(dplyr)

dt <- select(archive_cases_dv$DT,-case_rate)
dt_unique <- dt
dt_unique$percent_cli <- 0.1 * 1:160 + 20

head(dt)
arrange(dt,geo_value,time_value,version) %>% head()

dt_full <- archive_cases_dv$DT
mutate(dt_full, in_group =
         tidyr::replace_na(
           (geo_value == lag(geo_value) &
              time_value == lag(time_value)),
           FALSE
         )
)

test_that("Input for compactify must be NULL or a boolean", {
  expect_error(as_epi_archive(dv_duplicated,compactify="no"))
})
  
dt_true <- as_tibble(as_epi_archive(dt,compactify=TRUE)$DT)
dt_false <- as_tibble(as_epi_archive(dt,compactify=FALSE)$DT)
dt_null <- as_tibble(as_epi_archive(dt,compactify=NULL)$DT)

test_that("Warning for LOCF with compactify as NULL", {
  expect_warning(as_epi_archive(dt,compactify=NULL))
})

test_that("LOCF values are ignored", {
  expect_identical(nrow(dt),nrow(dt_false))
})

test_that("LOCF values are taken out", {
  dt_unique <- distinct(dt_false,case_rate,.keep_all = TRUE)
  expect_identical(dt_true,dt_null)
  expect_identical(dt_null,dt_unique)
})