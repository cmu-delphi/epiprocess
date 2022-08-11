library(dplyr)

test_that("Test error throwing",{
  # Error cases
  expect_error(growth_rate(x=1:3,y=1:4),
               "`x` and `y` must have the same length.")
  expect_error(growth_rate(x=1:20,y=1:20,x0=21),
               "`x0` must be a subset of `x`.")
  
  # This should produce no error
  expect_error(growth_rate(x=1:20,y=1:20,x0=c(1,3)),NA)
  
  # The warning that is prompted is as follows:
  # "`x` contains duplicate values. (If being run on a column in an `epi_df`,
  # did you group by relevant key variables?)"
  # Note that putting it in the regexp of expect_warning doesn't seem to work
  jhu_csse_daily_subset %>%
    mutate(cases_gr = growth_rate(x = time_value,  y = cases, dup_rm=TRUE)) %>%
    expect_warning() %>%
    expect_error()
})
