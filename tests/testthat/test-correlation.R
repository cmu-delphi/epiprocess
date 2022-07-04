library(tibble)

test_that("epi_cor throws errors as needed",{
  expect_error(epi_cor(data.frame(x=1:10),1,1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var2=1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var1=1))
})

test_that("epi_cor functions as it should",{
  expect_equal(epi_cor(x = jhu_csse_daily_subset, 
               var1 = case_rate_7d_av, 
               var2 = death_rate_7d_av, 
               cor_by = geo_value, 
               dt1 = -2)[1],
               tibble(geo_value = unique(jhu_csse_daily_subset$geo_value))
  )
})

test_that("shift works as it should",{
  expect_identical(shift(1:100,1),dplyr::lead(1:100))
  expect_identical(shift(1:100,0),1:100)
  expect_identical(shift(1:100,-1),dplyr::lag(1:100))
})