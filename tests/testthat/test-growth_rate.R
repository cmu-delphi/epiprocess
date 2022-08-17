library(dplyr)

X <- c(1:10,NA,10:20,NA)
Y <- c(2^(1:9),NA,NA,2^(10:21))

methods <- c("rel_change","linear_reg","smooth_spline","trend_filter")

gr <- function(method = "rel_change", h = 3, na_rm = TRUE, ...) {
  growth_rate(x=X,y=Y,method=method,na_rm = na_rm, h = h,...)
}

test_that("Test error throwing",{
  # Error cases
  expect_error(growth_rate(x=1:3,y=1:4),
               "`x` and `y` must have the same length.")
  expect_error(growth_rate(x=1:20,y=1:20,x0=21),
               "`x0` must be a subset of `x`.")
  # Fails only when method = `"trend_filter"`
  expect_error(gr(method = "trend_filter",cv=FALSE,df=1.5),
               "If `cv = FALSE`, then `df` must be an integer.")
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

test_that("Simple example of growth rate that produces desired results",{
  expect_equal(growth_rate(x=1:20,y=2^(1:20),h=1), c(rep(1,19),NaN))
})

test_that("log_scale works",{
  expect_equal(growth_rate(x=1:20,y=exp(1:20),h=5,
                           method="linear_reg",log_scale = TRUE),
               rep(1,20))
})

test_that("Running different methods with NA removal won't fail",{
  for (m in methods) {
    expect_false(NA %in% gr(method = m,x0=1:5))
  }
})

test_that("na_rm works and is necessary when there are NA's",{
  expect_false(NA %in% gr())
  expect_equal(length(gr()),20)
  expect_equal(gr(na_rm = FALSE),
               # 1+NA gives an NA classified as a numeric
               rep(1+NA,23))
  expect_equal(gr(h=1), c(rep(1,19),NaN))
  expect_error(gr(method = "smooth_spline"))
})
