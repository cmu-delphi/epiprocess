test_that("global param constructor errors when required", {
  # Check the tree when there is parameter dependency
  expect_true(growth_rate_global_params(df = "min", cv = FALSE)$cv)
  expect_true(growth_rate_global_params(df = "1se", cv = FALSE)$cv)
  expect_identical(growth_rate_global_params(df = "1se")$df, "1se")
  expect_false(growth_rate_global_params(df = 10, cv = FALSE)$cv)
  expect_identical(growth_rate_global_params(df = 10L)$df, 10L)
  expect_snapshot(error = TRUE, growth_rate_global_params(df = -5))

  # Make sure that assert_number is len 1
  expect_identical(growth_rate_global_params(nlambda = 5L)$nlambda, 5L)
  expect_snapshot(error = TRUE, growth_rate_global_params(nlambda = 5:8))
})

test_that("new setup args and warnings are as expected", {
  # NaN in log calculation
  expect_snapshot(growth_rate(-10:10, log_scale = TRUE))

  # NAs in x or y are removed
  expect_length(growth_rate(c(1:20, NA, 22:30)), 30L)
  expect_length(growth_rate(c(1:20, NA, 22:30), na_rm = TRUE), 29L)
  expect_length(growth_rate(1:30, x = c(1:20, NA, 22:30), na_rm = TRUE), 29L)

  # splines and trendfilter error on NAs
  expect_snapshot(error = TRUE, growth_rate(c(1:20, NA, 22:30), method = "smooth_spline"))
  expect_snapshot(error = TRUE, growth_rate(c(1:20, NA, 22:30), method = "trend_filter"))
  expect_snapshot(error = TRUE, growth_rate(c(1:20, -5, 22:30), log_scale = TRUE, method = "smooth_spline"))
  expect_snapshot(error = TRUE, growth_rate(c(1:20, -5, 22:30), log_scale = TRUE, method = "trend_filter"))

  # splines with multiple lambdas
  expect_snapshot(
    error = TRUE,
    growth_rate(
      y = c(1:20), method = "smooth_spline",
      params = growth_rate_global_params(lambda = 1:20)
    )
  )

  # other spline args give output (correctness not checked)
  z <- rnorm(30)
  expect_length(growth_rate(z, method = "smooth_spline"), 30L)
  expect_length(growth_rate(
    z,
    method = "smooth_spline", params = growth_rate_global_params(spar = .5)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "smooth_spline", params = growth_rate_global_params(lambda = 10)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "smooth_spline", params = growth_rate_global_params(df = 14)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "smooth_spline", params = growth_rate_global_params(cv = TRUE)
  ), 30L)
})


test_that("trendfilter growth_rate implementation", {
  skip_if_not_installed("trendfilter", "0.0.2")
  # tf with multiple lambdas, no df
  expect_length(
    growth_rate(
      y = 1:20, method = "trend_filter",
      params = growth_rate_global_params(lambda = 20:1)
    ),
    20L
  )
  # specifying lambda seq and df (numeric) is ok
  expect_length(
    growth_rate(
      y = 1:20, method = "trend_filter",
      params = growth_rate_global_params(lambda = 20:1, df = 4)
    ),
    20L
  )
  # single lambda and fixed df is bad
  expect_snapshot(
    error = TRUE,
    growth_rate(
      y = 1:20, method = "trend_filter",
      params = growth_rate_global_params(lambda = 1, df = 4)
    )
  )


  # other tf args give output (correctness not checked)
  z <- rnorm(30)
  expect_length(growth_rate(z, method = "trend_filter"), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(lambda = 10)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(df = 14)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(cv = TRUE)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(k = 3)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(nlambda = 10)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(lambda_max = 10)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(lambda_min = 10)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(lambda_min_ratio = .1)
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(error_measure = "mse")
  ), 30L)
  expect_length(growth_rate(
    z,
    method = "trend_filter", params = growth_rate_global_params(nfolds = 3)
  ), 30L)
})
