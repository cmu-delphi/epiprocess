test_that("canonical arrangement works", {
  tib <- tibble(
    x = 1:8,
    demo_grp = rep(c("b", "b", "a", "a"), times = 2),
    geo_value = rep(c("ga", "ca"), each = 4),
    time_value = rep(2:1, times = 4)
  )
  expect_error(arrange_canonical(tib))

  tib <- tib %>% as_epi_df(other_keys = "demo_grp")
  expect_equal(names(tib), c("geo_value", "demo_grp", "time_value", "x"))

  tib_sorted <- tib %>%
    arrange_canonical()
  expect_equal(names(tib_sorted), c("geo_value", "demo_grp", "time_value", "x"))
  expect_equal(tib_sorted$geo_value, rep(c("ca", "ga"), each = 4))
  expect_equal(tib_sorted$time_value, c(1, 2, 1, 2, 1, 2, 1, 2))
  expect_equal(tib_sorted$demo_grp, c("a", "a", "b", "b", "a", "a", "b", "b"))
  expect_equal(tib_sorted$x, c(8, 7, 6, 5, 4, 3, 2, 1))
})
