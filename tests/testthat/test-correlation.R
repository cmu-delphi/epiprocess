library(tibble)

test_that("epi_cor throws an error for a non-epi_df for its first argument",{
  expect_error(epi_cor(1:10,1,1))
  expect_error(epi_cor(data.frame(x=1:10),1,1))
})

test_that("epi_cor requires two var arguments, var1 and var2",{
  expect_error(epi_cor(archive_cases_dv_subset_dt$DT,var2=1))
  expect_error(epi_cor(archive_cases_dv_subset_dt$DT,var1=1))
})

test_that("epi_cor functions as intended",{
  expect_equal(epi_cor(x = cases_deaths_subset,
               var1 = case_rate_7d_av, 
               var2 = death_rate_7d_av, 
               cor_by = geo_value, 
               dt1 = -2)[1],
               tibble(geo_value = unique(cases_deaths_subset$geo_value))
  )
  
  edf <- as_epi_df(data.frame(geo_value=rep("asdf",20),
                              time_value=as.Date("2020-01-01") + 1:20,
                              pos=1:20,
                              neg=-(1:20)))
  expect_equal(epi_cor(edf, pos, pos)[[2]],1)
  expect_equal(epi_cor(edf, pos, neg)[[2]],-1)
})

test_that("shift works as intended",{
  expect_identical(epiprocess:::shift(1:100,1),dplyr::lead(1:100))
  expect_identical(epiprocess:::shift(1:100,0),1:100)
  expect_identical(epiprocess:::shift(1:100,-1),dplyr::lag(1:100))
})