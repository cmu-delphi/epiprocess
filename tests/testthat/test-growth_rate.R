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

test_that("Running different methods won't fail",{
  expect_error(
    for (m in c("rel_change","linear_reg","smooth_spline","trend_filter")) {
      growth_rate(x=1:25,y=sin(0:24)+0:24+1,method=m,h=3)
    },
    NA
  )
})

test_that("When using trend_filter, if `cv=FALSE`, then df must be an integer",{
  expect_error(growth_rate(x=1:25,y=sin(0:24)+0:24+1,method="trend_filter",
                           cv=FALSE,df=1.5,h=3),
               "If `cv = FALSE`, then `df` must be an integer.")
})

growth_rate(x=1:20,y=exp(1:20),h=5,method="linear_reg",log_scale = TRUE)

test_that("log_scale works",{
  expect_equal(growth_rate(x=1:20,y=exp(1:20),h=5,method="linear_reg",log_scale = TRUE),
               rep(1,20))
})

test_that("na_rm works",{
  X <- c(1:10,NA,12:19,NA)
  Y <- c(1:9,NA,NA,12:20)
  growth_rate(x=X,y=Y,na_rm = TRUE)
})
