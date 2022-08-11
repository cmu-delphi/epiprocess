library(dplyr)

test_that("Test error throwing",{
  # Error cases
  expect_error(growth_rate(x=1:3,y=1:4),
               "`x` and `y` must have the same length.")
  expect_error(growth_rate(x=1:20,y=1:20,x0=21),
               "`x0` must be a subset of `x`.")
  
  # This should produce no error
  expect_error(growth_rate(x=1:20,y=1:20,x0=c(1,3)),NA)
})

test_that("Test throwing of warning of duplicates",{
  # The warning that is prompted is as follows:
  # "`x` contains duplicate values. (If being run on a column in an `epi_df`,
  # did you group by relevant key variables?)"
  # Note that putting it in the regexp of expect_warning doesn't seem to work
  jhu_csse_daily_subset %>%
    mutate(cases_gr = growth_rate(x = time_value,  y = cases, dup_rm=TRUE)) %>%
    expect_warning() %>%
    expect_error()
})

test_that("Simple example of growth rate produces desired results",{
  expect_equal(growth_rate(x=1:20,y=2^(1:20),h=1),
               c(rep(1,19),NaN))
})

# Test each of the methods, log_scale settings, and na_rm settings
growth_rate(x=1:20,y=2^(1:20),method="rel_change",h=1)
growth_rate(x=1:20,y=2^(1:20),method="linear_reg",h=1)
growth_rate(x=1:20,y=2^(1:20),method="smooth_spline",h=1)
growth_rate(x=1:20,y=2^(1:20),method="trend_filter",h=1)

growth_rate(x=1:20,y=sin(1:20)+1:20,h=4)
growth_rate(x=1:20,y=sin(1:20)+1:20,method="rel_change",h=4)
growth_rate(x=1:20,y=sin(1:20)+1:20,method="linear_reg",h=4)
growth_rate(x=1:20,y=sin(1:20)+1:20,method="smooth_spline",h=4)
growth_rate(x=1:20,y=sin(1:20)+1:20,method="trend_filter",h=4)
