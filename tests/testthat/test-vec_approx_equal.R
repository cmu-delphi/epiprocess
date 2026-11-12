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

test_that("vec_approx_equal matches vec_equal on matrices (row-wise)", {
  # Row 1 equal; row 2 has a non-NA difference and an NA; row 3 differs only
  # with NAs; row 4 is a huge integer difference.
  large_int <- (.Machine$integer.max %/% 4L) * 3L
  m1 <- matrix(c(1L, 2L,
                 1L, NA,
                 1L, NA,
                 large_int, 1L),
               4L, 2L, byrow = TRUE)
  m2 <- matrix(c(1L, 2L,
                 2L, NA, # only first entry differs, non-NA in both mats
                 1L, 3L, # only second entry differs, non-NA in one mat
                 -large_int, 1L), # only first entry differs, large int difference
               4L, 2L, byrow = TRUE)
  for (na_equal in c(TRUE, FALSE)) {
    for (abs_tol in c(0, 1e-8)) {
      expected <- vctrs::vec_equal(m1, m2, na_equal = na_equal)
      expect_identical(vec_approx_equal(m1, m2, na_equal = na_equal, abs_tol = abs_tol), expected,
                       info = glue::glue("na_equal = {na_equal}, abs_tol = {abs_tol}"))
      # with double-type matrices instead:
      expect_identical(vec_approx_equal(m1 + 0, m2 + 0, na_equal = na_equal, abs_tol = abs_tol), expected,
                       info = glue::glue("na_equal = {na_equal}, abs_tol = {abs_tol}"))
      # with matrices wrapped inside of tibble-type columns:
      expect_identical(
        vec_approx_equal(tibble::tibble(m = m1), tibble::tibble(m = m2), na_equal = na_equal, abs_tol = abs_tol),
        expected,
        info = glue::glue("na_equal = {na_equal}, abs_tol = {abs_tol}")
      )
    }
  }
})

test_that("vec_approx_equal matches vec_equal and expected results on data frames with NAs", {
  skip("`vec_equal` seems buggy here, and we inherit its behavior in certain branches")
  comparisons <- tibble(
    val1 = c(11, 11, 11, NA),
    val2 = c(11, 55, NA, NA)
  )
  comparison_pairs <- tidyr::crossing(x = comparisons, y = comparisons)
  df1 <- tibble(x = comparison_pairs$x$val1, y = comparison_pairs$y$val1)
  df2 <- tibble(x = comparison_pairs$x$val2, y = comparison_pairs$y$val2)
  for (na_equal in c(TRUE, FALSE)) {
    for (abs_tol in c(0, 1e-8)) {
      info <- tibble(
        df1 = df1,
        df2 = df2,
        xcomp = vec_equal(df1$x, df2$x, na_equal = na_equal),
        ycomp = vec_equal(df1$y, df2$y, na_equal = na_equal),
        comp = vec_equal(df1, df2, na_equal = na_equal),
        comp2 = fifelse(xcomp, ycomp, xcomp, na = xcomp), # what it's doing behind the scenes?
        ours = vec_approx_equal(df1, df2, na_equal = na_equal, abs_tol = abs_tol),
        ours2 = xcomp & ycomp # what we want
      )
      expect_identical(info$comp, info$ours)
      expect_identical(info$ours, info$ours2)
    }
  }
})
