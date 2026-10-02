toy_epi_df <- tibble::tibble(
  x = 1:10,
  y = 1:10,
  time_value = rep(seq(
    as.Date("2020-01-01"),
    by = 1,
    length.out = 5
  ), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5),
  indic_var1 = as.factor(rep(1:2, times = 5)),
  indic_var2 = as.factor(rep(letters[1:5], times = 2))
) %>% as_epi_df(
  other_keys = c("indic_var1", "indic_var2")
)

att_toy <- attr(toy_epi_df, "metadata")

test_that("Head and tail do not drop the epi_df class", {
  att_head <- attr(head(toy_epi_df), "metadata")
  att_tail <- attr(tail(toy_epi_df), "metadata")

  expect_true(is_epi_df(head(toy_epi_df)))
  expect_true(is_epi_df(tail(toy_epi_df)))
  expect_identical(att_head$geo_type, att_toy$geo_type)
  expect_identical(att_head$time_type, att_toy$time_type)
  expect_identical(att_head$as_of, att_toy$as_of)
  expect_identical(att_head$other_keys, att_toy$other_keys)
  expect_identical(att_tail$geo_type, att_toy$geo_type)
  expect_identical(att_tail$time_type, att_toy$time_type)
  expect_identical(att_tail$as_of, att_toy$as_of)
  expect_identical(att_tail$other_keys, att_toy$other_keys)
})


test_that("Subsetting drops & does not drop the epi_df class appropriately", {
  # Row subset - should be epi_df
  row_subset <- toy_epi_df[1:2, ]
  att_row_subset <- attr(row_subset, "metadata")

  expect_true(is_epi_df(row_subset))
  expect_equal(nrow(row_subset), 2L)
  expect_equal(ncol(row_subset), 6L)
  expect_identical(att_row_subset$geo_type, att_toy$geo_type)
  expect_identical(att_row_subset$time_type, att_toy$time_type)
  expect_identical(att_row_subset$as_of, att_toy$as_of)
  expect_identical(att_row_subset$other_keys, att_toy$other_keys)

  # Row and col single value - shouldn't be an epi_df
  row_col_subset1 <- toy_epi_df[1, 2]
  expect_false(is_epi_df(row_col_subset1))
  expect_true(tibble::is_tibble(row_col_subset1))
  expect_equal(nrow(row_col_subset1), 1L)
  expect_equal(ncol(row_col_subset1), 1L)

  # Col subset with no time_value - shouldn't be an epi_df
  col_subset1 <- toy_epi_df[, c(1, 3)]

  expect_false(is_epi_df(col_subset1))
  expect_true(tibble::is_tibble(col_subset1))
  expect_equal(nrow(col_subset1), 10L)
  expect_equal(ncol(col_subset1), 2L)

  # Col subset with no geo_value - shouldn't be an epi_df
  col_subset2 <- toy_epi_df[, 2:3]

  expect_false(is_epi_df(col_subset2))
  expect_true(tibble::is_tibble(col_subset2))
  expect_equal(nrow(col_subset2), 10L)
  expect_equal(ncol(col_subset2), 2L)

  # Row and col subset that contains geo_value and time_value - should be epi_df
  row_col_subset2 <- toy_epi_df[2:3, c(1, 4)]
  att_row_col_subset2 <- attr(row_col_subset2, "metadata")

  expect_true(is_epi_df(row_col_subset2))
  expect_equal(nrow(row_col_subset2), 2L)
  expect_equal(ncol(row_col_subset2), 2L)
  expect_identical(att_row_col_subset2$geo_type, att_toy$geo_type)
  expect_identical(att_row_col_subset2$time_type, att_toy$time_type)
  expect_identical(att_row_col_subset2$as_of, att_toy$as_of)
})

test_that("When duplicate cols in subset should abort", {
  expect_error(toy_epi_df[, c(2, 2:3, 4, 4, 4)],
    "Duplicated column names: indic_var1, time_value",
    fixed = TRUE
  )
  expect_error(toy_epi_df[1:4, c(1, 2:4, 1)],
    "Duplicated column name: geo_value",
    fixed = TRUE
  )
})

test_that("Correct metadata when subset includes some of other_keys", {
  # Only include other_var of indic_var1
  only_indic_var1 <- toy_epi_df[, c(1:2, 4:6)]
  att_only_indic_var1 <- attr(only_indic_var1, "metadata")

  expect_true(is_epi_df(only_indic_var1))
  expect_equal(nrow(only_indic_var1), 10L)
  expect_equal(ncol(only_indic_var1), 5L)
  expect_identical(att_only_indic_var1$geo_type, att_toy$geo_type)
  expect_identical(att_only_indic_var1$time_type, att_toy$time_type)
  expect_identical(att_only_indic_var1$as_of, att_toy$as_of)
  expect_identical(att_only_indic_var1$other_keys, att_toy$other_keys[-2])

  # Only include other_var of indic_var2
  only_indic_var2 <- toy_epi_df[, c(1, 3:6)]
  att_only_indic_var2 <- attr(only_indic_var2, "metadata")

  expect_true(is_epi_df(only_indic_var2))
  expect_equal(nrow(only_indic_var2), 10L)
  expect_equal(ncol(only_indic_var2), 5L)
  expect_identical(att_only_indic_var2$geo_type, att_toy$geo_type)
  expect_identical(att_only_indic_var2$time_type, att_toy$time_type)
  expect_identical(att_only_indic_var2$as_of, att_toy$as_of)
  expect_identical(att_only_indic_var2$other_keys, att_toy$other_keys[-1])

  # Including both original other_keys was already tested above
})

test_that("Metadata is dropped by `as_tibble`", {
  grouped_converted <- toy_epi_df %>%
    group_by(geo_value) %>%
    as_tibble()
  expect_true(
    !any(c("metadata") %in% names(attributes(grouped_converted)))
  )
})

test_that("Grouping are dropped by `as_tibble`", {
  grouped_converted <- toy_epi_df %>%
    group_by(geo_value) %>%
    as_tibble()
  expect_true(
    !any(c("metadata", "groups") %in% names(attributes(grouped_converted)))
  )
  expect_s3_class(grouped_converted, class(tibble()), exact = TRUE)
})

test_that("Renaming columns gives appropriate colnames and metadata", {
  edf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age")
  # renaming using base R
  renamed_edf1 <- edf %>%
    `[`(c("geo_value", "time_value", "age", "value")) %>%
    `names<-`(c("geo_value", "time_value", "age_group", "value"))
  expect_identical(names(renamed_edf1), c("geo_value", "time_value", "age_group", "value"))
  expect_identical(attr(renamed_edf1, "metadata")$other_keys, c("age_group"))
  # renaming using select
  renamed_edf2 <- edf %>%
    as_epi_df(other_keys = "age") %>%
    select(geo_value, time_value, age_group = age, value)
  expect_identical(renamed_edf1, renamed_edf2)
})

test_that("Renaming columns while grouped gives appropriate colnames and metadata", {
  gedf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age") %>%
    group_by(geo_value)
  # renaming using base R
  renamed_gedf1 <- gedf %>%
    `[`(c("geo_value", "time_value", "age", "value")) %>%
    `names<-`(c("geo_value", "time_value", "age_group", "value"))
  # tets type preservation
  expect_true(inherits(renamed_gedf1, "epi_df"))
  expect_true(inherits(renamed_gedf1, "grouped_df"))
  # the names are right
  expect_identical(names(renamed_gedf1), c("geo_value", "time_value", "age_group", "value"))
  expect_identical(attr(renamed_gedf1, "metadata")$other_keys, c("age_group"))
  # renaming using select
  renamed_gedf2 <- gedf %>%
    select(geo_value, time_value, age_group = age, value)
  expect_identical(renamed_gedf1, renamed_gedf2)
})

test_that("Additional `select` on `epi_df` tests", {
  edf <- tibble::tibble(geo_value = "ak", time_value = as.Date("2020-01-01"), age = 1, value = 1) %>%
    as_epi_df(other_keys = "age")

  # Dropping a non-geo_value epikey column doesn't decay, though maybe it
  # should, since you'd expect that to possibly result in multiple rows per
  # epikey (though not in this toy case), and while we don't require that, we
  # sort of expect it:
  edf_not_decayed <- edf %>%
    select(geo_value, time_value, value)
  expect_class(edf_not_decayed, "epi_df")
  expect_identical(attr(edf_not_decayed, "metadata")$other_keys, character(0L))

  # Dropping geo_value does decay:
  edf_decayed <- edf %>%
    select(age, time_value, value)
  expect_false(inherits(edf_decayed, "epi_df"))
  expect_identical(attr(edf_decayed, "metadata"), NULL)
})

test_that("complete.epi_df works", {
  start_date <- as.Date("2020-01-01")
  daily_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 3, 3,
    2, start_date + 2, 2,
    2, start_date + 3, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  # Complete without grouping puts all the geo_values on the same min and max
  # time_value index
  expect_identical(
    daily_edf %>%
      complete(geo_value, time_value = full_seq(time_value, period = 1)),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 1, NA,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # Complete with grouping puts all the geo_values on individual min and max
  # time_value indices
  expect_identical(
    daily_edf %>%
      group_by(geo_value) %>%
      complete(time_value = full_seq(time_value, period = 1)),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3) %>%
      group_by(geo_value)
  )
  # Complete has explicit=TRUE by default, but if it's FALSE, then complete only fills the implicit gaps
  # not those that are explicitly NA
  daily_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 2, NA,
    1, start_date + 3, 3,
    2, start_date + 2, 2,
    2, start_date + 3, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  expect_identical(
    daily_edf %>%
      complete(geo_value, time_value = full_seq(time_value, period = 1), fill = list(value = 0), explicit = FALSE),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 2, NA,
      1, start_date + 3, 3,
      2, start_date + 1, 0,
      2, start_date + 2, 2,
      2, start_date + 3, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # Complete works for weekly data and can take a fill value
  # No grouping
  weekly_edf <- tibble::tribble(
    ~geo_value, ~time_value, ~value,
    1, start_date + 1, 1,
    1, start_date + 15, 3,
    2, start_date + 8, 2,
    2, start_date + 15, 3,
  ) %>%
    as_epi_df(as_of = start_date + 3)
  expect_identical(
    weekly_edf %>%
      complete(geo_value,
        time_value = full_seq(time_value, period = 7),
        fill = list(value = 0)
      ),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 8, 0,
      1, start_date + 15, 3,
      2, start_date + 1, 0,
      2, start_date + 8, 2,
      2, start_date + 15, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3)
  )
  # With grouping
  expect_identical(
    weekly_edf %>%
      group_by(geo_value) %>%
      complete(
        time_value = full_seq(time_value, period = 7),
        fill = list(value = 0)
      ),
    tibble::tribble(
      ~geo_value, ~time_value, ~value,
      1, start_date + 1, 1,
      1, start_date + 8, 0,
      1, start_date + 15, 3,
      2, start_date + 8, 2,
      2, start_date + 15, 3,
    ) %>%
      as_epi_df(as_of = start_date + 3) %>%
      group_by(geo_value)
  )
})

test_that("sum_groups_epi_df works", {
  out <- toy_epi_df %>% sum_groups_epi_df("x")
  expected_out <- toy_epi_df %>%
    group_by(time_value) %>%
    summarize(x = sum(x)) %>%
    mutate(geo_value = "total") %>%
    as_epi_df(as_of = attr(toy_epi_df, "metadata")$as_of)
  expect_equal(out, expected_out)
  out <- toy_epi_df %>% sum_groups_epi_df(x)
  expect_equal(out, expected_out)

  out <- toy_epi_df %>%
    sum_groups_epi_df(c(x, y), group_cols = c("time_value", "geo_value", "indic_var1"))
  expected_out <- toy_epi_df %>%
    group_by(time_value, geo_value, indic_var1) %>%
    summarize(x = sum(x), y = sum(y), .groups = "drop") %>%
    as_epi_df(as_of = attr(toy_epi_df, "metadata")$as_of, other_keys = "indic_var1") %>%
    arrange_canonical()
  expect_equal(out, expected_out)
  out <- toy_epi_df %>%
    sum_groups_epi_df(x:y, group_cols = c("time_value", "geo_value", "indic_var1"))
  expect_equal(out, expected_out)
})

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
    class = "epiprocess__merge_epi_df_join__missing_keys"
  )

  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "epi_df"))
})
test_that("merge_epi_df_join warns on metadata mismatch", {
  x_state <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val = 1) %>%
    as_epi_df() %>%
    epiprocess:::force_meta(geo_type = "state")

  y_nation <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val2 = 2) %>%
    as_epi_df() %>%
    epiprocess:::force_meta(geo_type = "nation")

  expect_warning(
    left_join(x_state, y_nation, by = c("geo_value", "time_value")),
    class = "epiprocess__merge_epi_df_join__metadata_mismatch"
  )

  x_day <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val = 1) %>%
    as_epi_df() %>%
    epiprocess:::force_meta(time_type = "day")

  y_week <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val2 = 2) %>%
    as_epi_df() %>%
    epiprocess:::force_meta(time_type = "week")

  expect_warning(
    left_join(x_day, y_week, by = c("geo_value", "time_value")),
    class = "epiprocess__merge_epi_df_join__metadata_mismatch"
  )
})
test_that("joins with richer key epi_df preserve class and update keys", {
  x <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val = 1) %>%
    as_epi_df()
  y <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), age = c("adult", "child"), val2 = 2:3) %>%
    as_epi_df(other_keys = "age")

  res <- left_join(x, y, by = c("geo_value", "time_value"))
  expect_s3_class(res, "epi_df")
  expect_true("age" %in% attr(res, "metadata")$other_keys)
  expect_equal(nrow(res), 2)
})

test_that("joins with richer key tibble decay silently", {
  x <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), val = 1) %>%
    as_epi_df()
  # y is a tibble (no metadata) but adds rows (implicit key)
  y <- tibble::tibble(geo_value = "ca", time_value = as.Date("2020-01-01"), age = c("adult", "child"), val2 = 2:3)

  # Should not warn about uniqueness, just decay
  expect_silent(res <- left_join(x, y, by = c("geo_value", "time_value")))
  expect_s3_class(res, "tbl_df")
  expect_false(inherits(res, "epi_df"))
})

test_that("print on 0-row edf does not malfunction", {
  expect_snapshot(as_epi_df(
    tibble(geo_value = character(), time_value = integer(), value = integer()),
    as_of = 5
  ))
})
