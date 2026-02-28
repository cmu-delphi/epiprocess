test_that("drop_na preserves epi_df", {
  x <- tibble(
    geo_value = c("ca", "ca"),
    time_value = as.Date(c("2020-01-01", "2020-01-02")),
    val = c(1, NA)
  ) %>% as_epi_df()

  res <- drop_na(x, val)
  expect_s3_class(res, "epi_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$val, 1)

  x_all_na <- x
  x_all_na$val <- NA
  res_empty <- drop_na(x_all_na, val)

  expect_s3_class(res_empty, "epi_df")
  expect_equal(nrow(res_empty), 0)
})

test_that("pivot_wider updates other_keys", {
  x <- tibble(
    geo_value = c("ca", "ca"),
    time_value = as.Date(c("2020-01-01", "2020-01-01")),
    grp = c("A", "B"),
    val = c(1, 2)
  ) %>% as_epi_df(other_keys = "grp")

  # Pivot grp to columns. grp is a key.
  res <- pivot_wider(x, names_from = grp, values_from = val)

  expect_s3_class(res, "epi_df")
  expect_true("A" %in% names(res))
  expect_true("B" %in% names(res))

  # 'grp' should be gone from other_keys because it's no longer a column
  expect_false("grp" %in% attr(res, "metadata")$other_keys)

  # Ensure uniqueness
  expect_equal(nrow(res), 1)

  # Pivot away geo_value (should decay if it results in missing essential keys)
  # Note: If it doesn't result in missing keys, it won't warn.
  # But geo_value is required. reconstruct_light_edf checks for it.
  res_decay <- pivot_wider(x, names_from = geo_value, values_from = val)
  expect_false(inherits(res_decay, "epi_df"))
  expect_s3_class(res_decay, "tbl_df")
})

test_that("pivot_longer handles new keys", {
  x <- tibble(
    geo_value = "ca",
    time_value = as.Date("2020-01-01"),
    A = 1, B = 2
  ) %>% as_epi_df()

  # Default names_to="name"
  res <- pivot_longer(x, c(A, B))

  expect_s3_class(res, "epi_df")
  expect_true("name" %in% names(res))
  expect_true("name" %in% attr(res, "metadata")$other_keys)
  expect_equal(nrow(res), 2)
  # Validate that it is indeed a valid epi_df (keys are unique)
  expect_silent(as_epi_df(res))

  # Explicit names_to
  res2 <- pivot_longer(x, c(A, B), names_to = "metric")
  expect_true("metric" %in% attr(res2, "metadata")$other_keys)

  # names_to .value should not be added to keys
  x_complex <- tibble(
    geo_value = "ca",
    time_value = as.Date("2020-01-01"),
    x_1 = 1, x_2 = 2, y_1 = 3, y_2 = 4
  ) %>% as_epi_df()

  # pivot such that we get columns x and y, and a key column 'num'
  res3 <- pivot_longer(x_complex,
    cols = -c(geo_value, time_value),
    names_to = c(".value", "num"), names_sep = "_"
  )

  expect_s3_class(res3, "epi_df")
  expect_true("num" %in% names(res3))
  expect_true("x" %in% names(res3))
  # 'num' should be a key, 'x' and 'y' should be values (not keys)
  expect_true("num" %in% attr(res3, "metadata")$other_keys)
  expect_false(".value" %in% attr(res3, "metadata")$other_keys)

  # names_to should be added to keys even if rows are already unique without it
  x_sparse <- tibble(
    geo_value = c("ca", "ny"),
    time_value = as.Date("2020-01-01"),
    val1 = c(1, NA),
    val2 = c(NA, 2)
  ) %>% as_epi_df()

  # After pivot with drop_na, we have (ca, 2020-01-01, val1, 1) and (ny, 2020-01-01, val2, 2)
  # These are unique on geo_value alone. But 'name' should still be a key.
  res4 <- pivot_longer(x_sparse, cols = c(val1, val2), values_drop_na = TRUE)

  expect_s3_class(res4, "epi_df")
  expect_true("name" %in% names(res4))
  expect_true("name" %in% attr(res4, "metadata")$other_keys)
  expect_equal(nrow(res4), 2)
})
