test_that("epi_cor throws errors as needed",{
  expect_error(epi_cor(data.frame(x=1:10),1,1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var2=1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var1=1))
})