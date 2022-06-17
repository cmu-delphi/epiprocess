test_that("epi_cor throws errors as needed",{
  expect_error(epi_cor(data.frame(x=1:10),1,1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var2=1))
  expect_error(epi_cor(archive_cases_dv_subset$DT,var1=1))
})

test_that("shift works as it should",{
  expect_identical(shift(1:100,1),dplyr::lead(1:100))
  expect_identical(shift(1:100,0),1:100)
  expect_identical(shift(1:100,-1),dplyr::lag(1:100))
})