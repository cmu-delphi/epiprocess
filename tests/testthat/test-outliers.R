test_that("detect_outlr throws error with duplicate x", {
  expect_error(detect_outlr(x = c(1, 2, 3, 3, 4), y = 1:5))
})

test_that("detect_outlr throws error with length(x) != length(y)", {
  expect_error(detect_outlr(x = 1:3, y = 1:5))
})
