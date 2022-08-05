toy_epi_df <- tibble::tibble(
  x = 1:10,
  y = 1:10,
  time_value = rep(seq(
    as.Date("2020-01-01"),
    by = 1,
    length.out = 5
  ), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5),
  indicator_var = as.factor(rep(1:2, times = 5)), 
) %>% as_epi_df(additional_metadata = list(other_keys = "indicator_var"))

att_toy = attr(toy_epi_df, "metadata")

test_that("head and tail do not drop the epi_df class", {
  att_head = attr(head(toy_epi_df), "metadata")
  att_tail = attr(tail(toy_epi_df), "metadata")
  
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


test_that("subsetting drops or does not drop the epi_df class appropriately", {
  
  # Row subset - should be epi_df
  row_subset = toy_epi_df[1:2, ]
  att_row_subset = attr(row_subset, "metadata")
  
  expect_true(is_epi_df(row_subset))
  expect_equal(nrow(row_subset), 2L)
  expect_equal(ncol(row_subset), 5L)
  expect_identical(att_row_subset$geo_type, att_toy$geo_type)
  expect_identical(att_row_subset$time_type, att_toy$time_type)
  expect_identical(att_row_subset$as_of, att_toy$as_of)
  expect_identical(att_row_subset$other_keys, att_toy$other_keys)
  
  # Col subset - shouldn't be an epi_df
  col_subset = toy_epi_df[, 2:3]
  
  expect_false(is_epi_df(col_subset))
  expect_true(tibble::is_tibble(col_subset))
  expect_equal(nrow(col_subset), 10L)
  expect_equal(ncol(col_subset), 2L)
  
  # Row and col single value - shouldn't be an epi_df
  row_col_subset1 = toy_epi_df[1,2]
  expect_false(is_epi_df(row_col_subset1))
  expect_true(tibble::is_tibble(row_col_subset1))
  expect_equal(nrow(row_col_subset1), 1L)
  expect_equal(ncol(row_col_subset1), 1L)
  
  # Row and col subset that contains geo_value and time_value - should be epi_df
  row_col_subset2 = toy_epi_df[2:3,1:3]
  att_row_col_subset2 = attr(row_col_subset2, "metadata")
  
  expect_true(is_epi_df(row_col_subset2))
  expect_equal(nrow(row_col_subset2), 2L)
  expect_equal(ncol(row_col_subset2), 3L)
  expect_identical(att_row_col_subset2$geo_type, att_toy$geo_type)
  expect_identical(att_row_col_subset2$time_type, att_toy$time_type)
  expect_identical(att_row_col_subset2$as_of, att_toy$as_of)
  expect_identical(att_row_col_subset2$other_keys, att_toy$other_keys)
  
})