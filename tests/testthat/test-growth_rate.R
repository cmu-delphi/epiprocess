test_that("global param constructor errors when required", {
  # Check the tree when there is parameter dependency
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
  expect_snapshot(error = TRUE, growth_rate(y = -10:10, log_scale = TRUE))
  expect_snapshot(error = TRUE, growth_rate(y = -10:10, log_scale = TRUE, method = "smooth_spline"))

  # NAs in x or y are removed
  expect_length(growth_rate(y = c(1:20, NA, 22:30)), 30L)
  expect_length(growth_rate(y = c(1:20, NA, 22:30), na_rm = TRUE), 30L)
  expect_snapshot(error = TRUE, growth_rate(y = 1:30, x = c(1:20, NA, 22:30), na_rm = TRUE))

  # splines and trendfilter error on NAs
  expect_length(growth_rate(y = c(1:20, NA, 22:30), method = "smooth_spline"), 30L)
  expect_length(growth_rate(y = c(1:20, NA, 22:30), method = "trend_filter"), 30L)
  expect_warning(growth_rate(y = c(1:20, -5, 22:30), log_scale = TRUE, method = "smooth_spline"))
  expect_warning(growth_rate(y = c(1:20, -5, 22:30), log_scale = TRUE, method = "trend_filter"))

  # splines with multiple lambdas
  expect_snapshot(
    error = TRUE,
    growth_rate(
      y = 1:20, method = "smooth_spline",
      params = growth_rate_global_params(lambda = 1:20)
    )
  )

  # other spline args give output (correctness not checked)
  z <- rnorm(30)
  expect_length(growth_rate(y = z, method = "smooth_spline"), 30L)
  expect_length(growth_rate(
    y = z,
    method = "smooth_spline", params = growth_rate_global_params(spar = .5)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "smooth_spline", params = growth_rate_global_params(lambda = 10)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "smooth_spline", params = growth_rate_global_params(df = 14)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "smooth_spline", params = growth_rate_global_params(cv = TRUE)
  ), 30L)
})

test_that("parser sees all cases", {
  skip_if_not_installed("trendfilter", "0.0.2")
  # 18 total cases
  # lambda in {NULL, scalar, vector}
  # df in {NULL, character, numeric}
  # cv in {T/F}

  grab_l <- function(l) list(cv = l$cv, df = l$df, lambda = l$lambda)

  # CV TRUE
  l <- growth_rate_global_params(cv = TRUE)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "min", lambda = NULL)
  )
  l <- growth_rate_global_params(cv = TRUE, df = "1se")
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "1se", lambda = NULL)
  )
  l <- growth_rate_global_params(cv = TRUE, df = "min", lambda = 1:5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "min", lambda = 1:5)
  )
  l <- growth_rate_global_params(cv = TRUE, lambda = 1:5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "min", lambda = 1:5)
  )
  l <- growth_rate_global_params(cv = TRUE, lambda = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
  l <- growth_rate_global_params(cv = TRUE, df = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
  l <- growth_rate_global_params(cv = TRUE, df = 1, lambda = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
  l <- growth_rate_global_params(cv = TRUE, df = "min", lambda = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
  l <- growth_rate_global_params(cv = TRUE, df = 1, lambda = 1:5)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))

  # CV = FALSE (the default)
  # 5 Cases where we turn CV on
  l <- growth_rate_global_params(df = "1se")
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "1se", lambda = NULL)
  )
  l <- growth_rate_global_params(df = "1se", lambda = 1:5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "1se", lambda = 1:5)
  )
  l <- growth_rate_global_params(lambda = 1:5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = TRUE, df = "min", lambda = 1:5)
  )
  expect_identical(
    grab_l(parse_trendfilter_params(growth_rate_global_params())),
    list(cv = TRUE, df = "min", lambda = NULL)
  )
  # 3 cases where CV stays False
  l <- growth_rate_global_params(lambda = 1)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = FALSE, df = NULL, lambda = 1)
  )
  l <- growth_rate_global_params(df = 5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = FALSE, df = 5, lambda = NULL)
  )
  l <- growth_rate_global_params(df = 5, lambda = 1:5)
  expect_identical(
    grab_l(parse_trendfilter_params(l)),
    list(cv = FALSE, df = 5, lambda = 1:5)
  )

  # 2 error cases
  l <- growth_rate_global_params(df = "min", lambda = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
  l <- growth_rate_global_params(df = 1, lambda = 1)
  expect_snapshot(error = TRUE, parse_trendfilter_params(l))
})

test_that("trendfilter growth_rate implementation", {
  skip_if_not_installed("trendfilter", "0.0.2")

  # various tf args give output (correctness not checked)
  z <- rnorm(30)
  expect_length(growth_rate(y = z, method = "trend_filter"), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(lambda = 10)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(df = 14)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(cv = TRUE)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(k = 3)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(nlambda = 10)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(lambda_max = 10)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(lambda_min = 10)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(lambda_min_ratio = .1)
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(error_measure = "mse")
  ), 30L)
  expect_length(growth_rate(
    y = z,
    method = "trend_filter", params = growth_rate_global_params(nfolds = 3)
  ), 30L)
})
