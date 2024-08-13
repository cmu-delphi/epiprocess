test_that("canonical arrangement works", {
  tib <- tibble(
    x = 1:8,
    y = rep(c("b", "b", "a", "a"), times = 2),
    geo_value = rep(c("ga", "ca"), each = 4),
    time_value = rep(2:1, times = 4)
  )
  expect_warning(arrange_canonical(tib))

  tib <- tib %>% as_epi_df(additional_metadata = list(other_keys = "y"))
  expect_equal(names(tib), c("geo_value", "time_value", "x", "y"))

  tib_sorted <- arrange_canonical(tib)
  expect_equal(names(tib_sorted), c("geo_value", "time_value", "y", "x"))
  expect_equal(tib_sorted$geo_value, rep(c("ca", "ga"), each = 4))
  expect_equal(tib_sorted$time_value, c(1, 1, 2, 2, 1, 1, 2, 2))
  expect_equal(tib_sorted$y, rep(letters[1:2], times = 4))
  expect_equal(tib_sorted$x, c(8, 6, 7, 5, 4, 2, 3, 1))
})
