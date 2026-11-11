test_that("is_locf replacement works as expected", {
  vec <- c(1, 1, 1e-10, 1.1e-10, NA, NA, NaN, NaN)
  is_repeated <- c(0, 1, 0, 1, 0, 1, 0, 1)
  expect_equal(
    c(
      FALSE,
      vec_approx_equal(
        head(vec, -1L), tail(vec, -1L),
        na_equal = TRUE, abs_tol = .Machine$double.eps^0.5
      )
    ),
    as.logical(is_repeated)
  )
})

test_that("vec_approx_equal is compatible with vec_equal on some edge cases", {
  # Match (`==` and) `vec_equal` on NaN behavior:
  tbl <- tibble::tribble(
    ~x,   ~y,
    NaN,   5,
    NaN,  NA,
    NA,  NaN,
    NaN, NaN,
  )
  expect_identical(
    vec_approx_equal(tbl$x, tbl$y, na_equal = FALSE, abs_tol = 1e-8),
    vctrs::vec_equal(tbl$x, tbl$y, na_equal = FALSE)
  )
  expect_identical(
    vec_approx_equal(tbl$x, tbl$y, na_equal = TRUE, abs_tol = 1e-8),
    vctrs::vec_equal(tbl$x, tbl$y, na_equal = TRUE)
  )

  # Match `vec_equal` behavior on namedness, including within elements:
  unnamed_list <- list(5)
  named_list <- list(a = 5)
  seminamed_list <- `names<-`(list(5), "")
  # unnamed <-> named, and wrapped versions:
  expect_identical(
    vec_approx_equal(unnamed_list, named_list, na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(unnamed_list, named_list, na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(unnamed_list), list(named_list), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(unnamed_list), list(named_list), na_equal = TRUE)
  )
  # unnamed <-> seminamed, and wrapped versions:
  expect_identical(
    vec_approx_equal(unnamed_list, seminamed_list, na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(unnamed_list, seminamed_list, na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(unnamed_list), list(seminamed_list), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(unnamed_list), list(seminamed_list), na_equal = TRUE)
  )
  # named <-> seminamed, and wrapped versions:
  expect_identical(
    vec_approx_equal(named_list, seminamed_list, na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(named_list, seminamed_list, na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(named_list), list(seminamed_list), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(named_list), list(seminamed_list), na_equal = TRUE)
  )

  # Match `vec_equal` behavior on (p)types, including within elements:
  dbl <- 5.0
  int <- 5L
  dbl_with_name <- c(a = 5)
  dbl_with_other_attr1 <- `attr<-`(5, "myattr", 1)
  dbl_with_other_attr2 <- `attr<-`(5, "myattr", 1 + 1e-10)
  expect_identical(
    vec_approx_equal(dbl, int, na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(dbl, int, na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(dbl), list(int), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(dbl), list(int), na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(dbl), list(dbl_with_name), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(dbl), list(dbl_with_name), na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(dbl), list(dbl_with_other_attr1), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(dbl), list(dbl_with_other_attr1), na_equal = TRUE)
  )
  expect_identical(
    vec_approx_equal(list(dbl_with_other_attr1), list(dbl_with_other_attr2), na_equal = TRUE, abs_tol = 1e-8),
    vec_equal(list(dbl_with_other_attr1), list(dbl_with_other_attr2), na_equal = TRUE)
  )

  # But we do allow tol in some cases:
  nearby_dbl <- 5 + 1e-10
  expect_identical(
    vec_approx_equal(list(dbl), list(nearby_dbl), na_equal = TRUE, abs_tol = 1e-8),
    TRUE
  )
})

test_that("vec_approx_equal doesn't have issue with large integer differences", {
  large_int <- (.Machine$integer.max %/% 4L) * 3L
  expect_identical(vec_approx_equal(large_int, -large_int, na_equal = TRUE, abs_tol = 1e-8), FALSE)
  expect_identical(vec_approx_equal(large_int, -large_int, na_equal = FALSE, abs_tol = 1e-8), FALSE)
})
