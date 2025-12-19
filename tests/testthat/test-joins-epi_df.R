test_that("joins preserve epi_df class and metadata when unique", {
  x <- tibble(geo_value = 1, time_value = 1, x_val = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1, y_val = 2) %>% as_epi_df()

  # Basic left join
  res_left <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_left, "epi_df")
  expect_equal(res_left$x_val, 1)
  expect_equal(res_left$y_val, 2)

  # Basic right join
  res_right <- right_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_right, "epi_df")
  expect_equal(res_right$x_val, 1)
  expect_equal(res_right$y_val, 2)

  # inner join
  res_inner <- inner_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner, "epi_df")
  expect_equal(res_inner$x_val, 1)
  expect_equal(res_inner$y_val, 2)

  # full join
  res_full <- inner_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_full, "epi_df")
  expect_equal(res_full$x_val, 1)
  expect_equal(res_full$y_val, 2)
})

test_that("joins merge other_keys from both sides", {
  x <- tibble(geo_value = 1, time_value = 1, key_x = "A", val_x = 1) %>%
    as_epi_df(other_keys = "key_x")
  y <- tibble(geo_value = 1, time_value = 1, key_y = "B", val_y = 2) %>%
    as_epi_df(other_keys = "key_y")

  # full_join
  res_full <- full_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_full, "epi_df")
  expect_true(setequal(attr(res_full, "metadata")$other_keys, c("key_x", "key_y")))
  expect_true(check_ukey_unique(res_full, c("geo_value", "time_value", "key_x", "key_y")))

  # left_join
  res_left <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_left, "epi_df")
  expect_true(setequal(attr(res_left, "metadata")$other_keys, c("key_x", "key_y")))
  expect_true(check_ukey_unique(res_left, c("geo_value", "time_value", "key_x", "key_y")))

  # right_join
  res_right <- right_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_right, "epi_df")
  expect_true(setequal(attr(res_right, "metadata")$other_keys, c("key_x", "key_y")))
  expect_true(check_ukey_unique(res_right, c("geo_value", "time_value", "key_x", "key_y")))

  # inner_join
  res_inner <- inner_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner, "epi_df")
  expect_true(setequal(attr(res_inner, "metadata")$other_keys, c("key_x", "key_y")))
  expect_true(check_ukey_unique(res_inner, c("geo_value", "time_value", "key_x", "key_y")))
})

test_that("joins decays to tibble when uniqueness is violated", {
  # One-to-many join on essential keys without sufficient other keys
  # Note: y is NOT an epi_df here, so it has no keys.
  x <- tibble(geo_value = 1, time_value = 1, target = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1, sensor = c("A", "B"), val = 1:2)

  # left_join
  res_left <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_left, "tbl_df")
  expect_false(inherits(res_left, "epi_df"))

  # right_join
  res_right <- right_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_right, "tbl_df")
  expect_false(inherits(res_right, "epi_df"))

  # inner_join
  res_inner <- inner_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner, "tbl_df")
  expect_false(inherits(res_inner, "epi_df"))

  # full_join
  res_full <- full_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_full, "tbl_df")
  expect_false(inherits(res_full, "epi_df"))
})

test_that("joins valid if extra key in y is merged correctly", {
  # y is an epi_df with keys
  x <- tibble(geo_value = 1, time_value = 1, target = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1, sensor = c("A", "B"), val = 1:2) %>%
    as_epi_df(other_keys = "sensor")

  # left_join
  res_left <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_left, "epi_df")
  expect_equal(attr(res_left, "metadata")$other_keys, "sensor")

  # right_join
  res_right <- right_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_right, "epi_df")
  expect_equal(attr(res_right, "metadata")$other_keys, "sensor")

  # inner_join
  res_inner <- inner_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner, "epi_df")
  expect_equal(attr(res_inner, "metadata")$other_keys, "sensor")

  # full_join
  res_full <- full_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_full, "epi_df")
  expect_equal(attr(res_full, "metadata")$other_keys, "sensor")
})

test_that("cross_join always decays", {
  x <- tibble(geo_value = 1, time_value = 1) %>% as_epi_df()
  y <- tibble(x = 1:2)

  res <- cross_join(x, y)
  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "epi_df"))
})

test_that("joins decay if keys contain NA", {
  # Join creating NA in key
  x <- tibble(geo_value = c(1, 2), time_value = 1, county = "A", val = 1) %>%
    as_epi_df(other_keys = "county")
  y <- tibble(geo_value = c(1, 3), time_value = 1, age = "young", val2 = 2) %>%
    as_epi_df(other_keys = "age")

  # left_join
  expect_warning(res_left <- left_join(x, y, by = c("geo_value", "time_value")), "NA values found in key columns")

  expect_s3_class(res_left, "tbl_df")
  expect_false(inherits(res_left, "epi_df"))
  expect_true(any(is.na(res_left$age)))

  # right_join
  expect_warning(res_right <- right_join(x, y, by = c("geo_value", "time_value")), "NA values found in key columns")

  expect_s3_class(res_right, "tbl_df")
  expect_false(inherits(res_right, "epi_df"))
  expect_true(any(is.na(res_right$county)))

  # full_join
  expect_warning(res_full <- full_join(x, y, by = c("geo_value", "time_value")), "NA values found in key columns")

  expect_s3_class(res_full, "tbl_df")
  expect_false(inherits(res_full, "epi_df"))
  expect_true(any(is.na(res_full$age)))
  expect_true(any(is.na(res_full$county)))
})

test_that("semi_join and anti_join behave correctly", {
  x <- tibble(geo_value = c(1, 2), time_value = 1, val = 1) %>% as_epi_df()
  y <- tibble(geo_value = 1, time_value = 1)

  # semi_join should return epi_df
  res <- semi_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res, "epi_df")
  expect_equal(nrow(res), 1)

  # anti_join should return epi_df
  res_anti <- anti_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_anti, "epi_df")
  expect_equal(nrow(res_anti), 1)
  expect_equal(res_anti$geo_value, 2)
})

test_that("grouped epi_df joins preserve grouping and class", {
  x <- tibble(geo_value = c(1, 2), time_value = 1, k = c("a", "b"), val = 1) %>%
    as_epi_df(other_keys = "k") %>%
    group_by(k)

  y <- tibble(geo_value = 1, time_value = 1, val2 = 2) %>% as_epi_df()

  # grouped join where y adds keys
  y2 <- tibble(geo_value = c(1, 2), time_value = 1, k2 = c("z", "w")) %>%
    as_epi_df(other_keys = "k2")

  # left_join should keep grouping
  res_left <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res_left, "epi_df")
  expect_true(dplyr::is_grouped_df(res_left))
  expect_equal(dplyr::group_vars(res_left), "k")
  expect_equal(attr(res_left, "metadata")$other_keys, "k")

  res_left2 <- left_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_left2, "epi_df")
  expect_true(dplyr::is_grouped_df(res_left2))
  expect_true(setequal(attr(res_left2, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_left2), "k")

  # right_join
  res_right <- right_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_right, "epi_df")
  expect_true(setequal(attr(res_right, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_right), "k")

  res_right2 <- right_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_right2, "epi_df")
  expect_true(dplyr::is_grouped_df(res_right2))
  expect_true(setequal(attr(res_right2, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_right2), "k")

  # inner_join
  res_inner <- inner_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner, "epi_df")
  expect_true(dplyr::is_grouped_df(res_inner))
  expect_equal(dplyr::group_vars(res_inner), "k")

  res_inner2 <- inner_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner2, "epi_df")
  expect_true(dplyr::is_grouped_df(res_inner2))
  expect_true(setequal(attr(res_inner2, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_inner2), "k")

  # full_join
  res_full <- full_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_full, "epi_df")
  expect_true(setequal(attr(res_full, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_full), "k")

  res_full2 <- full_join(x, y2, by = c("geo_value", "time_value"))
  expect_s3_class(res_full2, "epi_df")
  expect_true(dplyr::is_grouped_df(res_full2))
  expect_true(setequal(attr(res_full2, "metadata")$other_keys, c("k", "k2")))
  expect_equal(dplyr::group_vars(res_full2), "k")
})

test_that("joins with superset/subset keys work as expected", {
  # x has keys k1, k2. y has k1.
  x <- tibble(geo_value = 1, time_value = 1, k1 = 1, k2 = 1, val = 1) %>%
    as_epi_df(other_keys = c("k1", "k2"))
  y <- tibble(geo_value = 1, time_value = 1, k1 = 1, val2 = 2) %>%
    as_epi_df(other_keys = "k1")
  # y has disjoint keys k3
  y3 <- tibble(geo_value = 1, time_value = 1, k3 = 3, val3 = 3) %>%
    as_epi_df(other_keys = "k3")

  # left_join
  res_left1 <- left_join(x, y, by = c("geo_value", "time_value", "k1"))
  expect_s3_class(res_left1, "epi_df")
  expect_true(setequal(attr(res_left1, "metadata")$other_keys, c("k1", "k2")))

  res_left3 <- left_join(x, y3, by = c("geo_value", "time_value"))
  expect_s3_class(res_left3, "epi_df")
  expect_true(setequal(attr(res_left3, "metadata")$other_keys, c("k1", "k2", "k3")))

  # right_join
  res_right1 <- right_join(x, y, by = c("geo_value", "time_value", "k1"))
  expect_s3_class(res_right1, "epi_df")
  expect_true(setequal(attr(res_right1, "metadata")$other_keys, c("k1", "k2")))

  res_right3 <- right_join(x, y3, by = c("geo_value", "time_value"))
  expect_s3_class(res_right3, "epi_df")
  expect_true(setequal(attr(res_right3, "metadata")$other_keys, c("k1", "k2", "k3")))

  # inner_join
  res_inner1 <- inner_join(x, y, by = c("geo_value", "time_value", "k1"))
  expect_s3_class(res_inner1, "epi_df")
  expect_true(setequal(attr(res_inner1, "metadata")$other_keys, c("k1", "k2")))

  res_inner3 <- inner_join(x, y3, by = c("geo_value", "time_value"))
  expect_s3_class(res_inner3, "epi_df")
  expect_true(setequal(attr(res_inner3, "metadata")$other_keys, c("k1", "k2", "k3")))
})

test_that("joins decay with warning if keys are lost (e.g. renamed via conflict)", {
  x <- tibble(geo_value = 1, time_value = 1, k = 1, val = 1) %>%
    as_epi_df(other_keys = "k")
  y <- tibble(geo_value = 1, time_value = 1, k = 2, val2 = 2)

  # Join without including 'k' in keys, causing it to be renamed to k.x and k.y
  expect_warning(
    res <- left_join(x, y, by = c("geo_value", "time_value")),
    "Key column"
  )
  
  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "epi_df"))
})
