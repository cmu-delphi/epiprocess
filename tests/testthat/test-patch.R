test_that("tbl_fast_anti_join with abs_tol = 0 works", {
  x <- tibble::tibble(
    g = c("a", "a", "b", "b", "c", "c"),
    t = c(1, 2, 1, 2, 1, 2),
    v = c(1, 2, 3, NA, 5, NA),
    w = c("p", "q", "r", "s", "u", "v")
  )
  y <- tibble::tibble(
    g = c("a", "a", "b", "b", "c", "d"),
    t = c(1, 2, 1, 2, 1, 1),
    v = c(
      1, # (a, 1): exactly equal -> excluded
      2 + 1e-10, # (a, 2): tiny difference -> kept
      3, # (b, 1): equal `v`, but different `w` -> kept
      NA, # (b, 2): NA vs. NA -> equal -> excluded
      NaN, # (c, 1): non-NA vs. NaN -> kept
      9 # (d, 1): not in `x` -> not in output
    ),
    w = c("p", "q", "CHANGED", "s", "u", "z")
  )[c(4, 2, 6, 1, 3, 5), ] # shuffled
  # (c, 2) has no ukey match in `y` -> kept.
  expect_identical(
    tbl_fast_anti_join(x, y, c("g", "t"), c("v", "w"), abs_tol = 0),
    x[c(2, 3, 5, 6), ]
  )
  # Default `abs_tol` is 0:
  expect_identical(
    tbl_fast_anti_join(x, y, c("g", "t"), c("v", "w")),
    x[c(2, 3, 5, 6), ]
  )
  # With no value columns, this is a plain ukey anti-join:
  expect_identical(
    tbl_fast_anti_join(x, y, c("g", "t"), character(), abs_tol = 0),
    x[6, ]
  )
})

test_that("tbl_fast_anti_join with abs_tol > 0 works", {
  # `y` rows are in a different order than `x` rows, and `y` has an extra ukey
  # (g = "d") not in `x`, to check that ukey matches are lined up correctly.
  x <- tibble::tibble(
    g = c("a", "a", "b", "b", "c"),
    t = c(1, 2, 1, 2, 1),
    v = c(1, 2, 3, NA, 5),
    w = c("p", "q", "r", "s", "u")
  )
  y <- tibble::tibble(
    g = c("a", "a", "b", "b", "d"),
    t = c(1, 2, 1, 2, 1),
    v = c(
      1 + 1e-6, # (a, 1): outside tolerance -> kept
      2 + 1e-10, # (a, 2): within tolerance, same `w` -> excluded
      3, # (b, 1): equal `v`, but different `w` -> kept
      NA, # (b, 2): NA vs. NA -> equal -> excluded
      9 # (d, 1): not in `x` -> not in output
    ),
    w = c("p", "q", "CHANGED", "s", "z")
  )[c(4, 2, 1, 3, 5), ] # shuffled

  # (c, 1) has no ukey match in `y` -> kept.
  expect_identical(
    tbl_fast_anti_join(x, y, c("g", "t"), c("v", "w"), abs_tol = 1e-8),
    x[c(1, 3, 5), ]
  )
  # With 0 tolerance, the within-tolerance row is also kept:
  expect_identical(
    tbl_fast_anti_join(x, y, c("g", "t"), c("v", "w"), abs_tol = 0),
    x[c(1, 2, 3, 5), ]
  )
  # NA vs. non-NA is a difference:
  y2 <- y
  y2$v[y2$g == "b" & y2$t == 2] <- 4
  expect_identical(
    tbl_fast_anti_join(x, y2, c("g", "t"), c("v", "w"), abs_tol = 1e-8),
    x[c(1, 3, 4, 5), ]
  )
})

test_that("tbl_fast_anti_join with abs_tol > 0 works with no value columns", {
  x <- tibble::tibble(g = c("a", "a", "b", "c"), t = c(1, 2, 1, 1))
  y <- tibble::tibble(g = c("b", "a", "d"), t = c(1, 2, 1))
  # Only ukeys are compared, so this should act like a plain ukey anti-join:
  expect_no_warning(
    result <- tbl_fast_anti_join(x, y, c("g", "t"), character(), abs_tol = 1e-8)
  )
  expect_identical(result, x[c(1, 4), ])
  expect_identical(result, tbl_fast_anti_join(x, y, c("g", "t"), character(), abs_tol = 0))
})
