test_that("canonical arrangement works", {
  tib <- tibble(
    x = 1:8,
    demo_grp = rep(c("b", "b", "a", "a"), times = 2),
    geo_value = rep(c("ga", "ca"), each = 4),
    time_value = rep(2:1, times = 4)
  )
  expect_error(arrange_canonical(tib))

  tib <- tib %>% as_epi_df(other_keys = "demo_grp")
  expect_equal(names(tib), c("geo_value", "time_value", "demo_grp", "x"))

  tib_cols_shuffled <- tib %>% select(geo_value, time_value, x, demo_grp)

  tib_sorted <- arrange_canonical(tib_cols_shuffled)
  expect_equal(names(tib_sorted), c("geo_value", "time_value", "demo_grp", "x"))
  expect_equal(tib_sorted$geo_value, rep(c("ca", "ga"), each = 4))
  expect_equal(tib_sorted$time_value, c(1, 1, 2, 2, 1, 1, 2, 2))
  expect_equal(tib_sorted$demo_grp, rep(letters[1:2], times = 4))
  expect_equal(tib_sorted$x, c(8, 6, 7, 5, 4, 2, 3, 1))
})
