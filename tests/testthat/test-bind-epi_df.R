test_that("bind_rows preserves epi_df if possible", {
  x <- tibble(geo_value = 1, time_value = 1, val = 1) %>% as_epi_df()
  # distinct time
  y <- tibble(geo_value = 1, time_value = 2, val = 2) %>% as_epi_df()

  res <- bind_rows(x, y)

  expect_s3_class(res, "epi_df")
  expect_equal(nrow(res), 2)
})

test_that("bind_cols preserves epi_df class", {
  x <- tibble(geo_value = 1, time_value = 1, val = 1) %>% as_epi_df()
  y <- tibble(val2 = 2)

  res <- bind_cols(x, y)

  expect_s3_class(res, "epi_df")
  expect_true("val2" %in% names(res))

  res2 <- bind_cols(y, x)

  expect_s3_class(res2, "epi_df")
  expect_true("val2" %in% names(res2))
})

test_that("bind_rows checks uniqueness", {
  x <- tibble(geo_value = 1, time_value = 1, val = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1, val = 2) %>% as_epi_df()

  # duplicate keys
  res <- bind_rows(x, y)

  # Should decay
  expect_false(inherits(res, "epi_df"))
  expect_s3_class(res, "tbl_df")
})

test_that("bind_rows merges other_keys", {
  x <- tibble(geo_value = 1, time_value = 1, k1 = 1, val = 1) %>%
    as_epi_df(other_keys = "k1")
  y <- tibble(geo_value = 1, time_value = 2, k2 = 2, val = 2) %>%
    as_epi_df(other_keys = "k2")

  # When binding, x gets NA for k2, y gets NA for k1.
  expect_warning(res <- bind_rows(x, y), "NA values found in key columns")
  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "epi_df"))
})

test_that("bind_rows fails on missing keys", {
  x <- tibble(geo_value = 1, time_value = 1, val = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, val = 2) # Missing time_value

  # time_value becomes NA for rows of y.
  expect_warning(res <- bind_rows(x, y), "NA values found")
  expect_false(inherits(res, "epi_df"))
})

test_that("bind_cols fails on duplicate keys", {
  # If we bind two epi_dfs with same keys,
  # we end up with duplicate column names,

  x <- tibble(geo_value = 1, time_value = 1, val = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1, val = 2) %>% as_epi_df()

  expect_warning(res <- bind_cols(x, y), "Key column")
  expect_false(inherits(res, "epi_df"))
})

test_that("bind_rows delegates to epi_df even if first arg is not", {
  x <- tibble(geo_value = 1, time_value = 1, val = 1)
  y <- tibble(geo_value = 1, time_value = 2, val = 2) %>% as_epi_df()

  res <- bind_rows(x, y)
  expect_s3_class(res, "epi_df")
})

test_that("bind_cols delegates to epi_df even if first arg is not", {
  x <- tibble(extra = 1)
  y <- tibble(geo_value = 1, time_value = 1, val = 2) %>% as_epi_df()

  res <- bind_cols(x, y)
  expect_s3_class(res, "epi_df")
})

test_that("bind_rows matches matching other_keys", {
  x <- tibble(geo_value = 1, time_value = 1, k1 = 1, val = 1) %>%
    as_epi_df(other_keys = "k1")
  y <- tibble(geo_value = 1, time_value = 2, k1 = 1, val = 2) %>%
    as_epi_df(other_keys = "k1")

  # Binding should be fine since k1 exists in both
  res <- bind_rows(x, y)
  expect_s3_class(res, "epi_df")
  expect_equal(attr(res, "metadata")$other_keys, "k1")
  expect_equal(res$k1, c(1, 1))
})

test_that("bind_cols merges other_keys", {
  x <- tibble(geo_value = 1, time_value = 1, k1 = 1, val = 1) %>%
    as_epi_df(other_keys = "k1")

  # tibble that has no epi_df keys
  y2 <- tibble(k2 = 2)

  res <- bind_cols(x, y2)
  expect_s3_class(res, "epi_df")
  expect_equal(attr(res, "metadata")$other_keys, "k1")
})
