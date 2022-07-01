toy_epi_df <- tibble::tibble(
  x = 1:10,
  y = 1:10,
  time_value = rep(seq(
    as.Date("2020-01-01"),
    by = 1,
    length.out = 5
  ), times = 2),
  geo_value = rep(c("ca", "hi"), each = 5),
  additional_key = as.factor(rep(1:2, times = 5)),
) %>% epiprocess::as_epi_df()

attributes(toy_epi_df)$metadata$other_keys <- "additional_key"
att_toy = attr(toy_epi_df, "metadata")

test_that("head and tail do not drop the epi_df class", {

  att_head = attr(head(toy_epi_df), "metadata")
  att_tail = attr(tail(toy_epi_df), "metadata")
  
  expect_true(is_epi_df(head(toy_epi_df)))
  expect_true(is_epi_df(tail(toy_epi_df)))
  expect_identical(att_head$geo_type, att_toy$geo_type)
  expect_identical(att_head$time_type, att_toy$time_type)
  expect_identical(att_head$as_of, att_toy$as_of)
  expect_identical(att_tail$geo_type, att_toy$geo_type)
  expect_identical(att_tail$time_type, att_toy$time_type)
  expect_identical(att_tail$as_of, att_toy$as_of)
})


test_that("subsetting drops or does not drop the epi_df class appropriately", {
  
  # Row subset - should be epi_df
  row_subset = toy_epi_df[1:2, ]
  expect_true(is_epi_df(row_subset))
  expect_equal(nrow(row_subset), 2L)
  expect_equal(ncol(row_subset), 5L)
  expect_identical(attributes(row_subset)$metadata$geo_type, att_toy$geo_type)
  expect_identical(attributes(row_subset)$metadata$time_type, att_toy$time_type)
  expect_identical(attributes(row_subset)$metadata$as_of, att_toy$as_of)
  expect_identical(attributes(row_subset)$metadata$other_keys, att_toy$other_keys)
  
  # Col subset - shouldn't be an epi_df
  col_subset = toy_epi_df[, 2:3]
  expect_false(is_epi_df(col_subset))
  expect_true(tibble::is_tibble(col_subset))
  expect_equal(nrow(col_subset), 10L)
  expect_equal(ncol(col_subset), 2L)
  
  # Row and col single value - shouldn't be an epi_df
  row_col_subset = toy_epi_df[1,2]
  expect_false(is_epi_df(row_col_subset))
  expect_true(tibble::is_tibble(row_col_subset))
  expect_equal(nrow(row_col_subset), 1L)
  expect_equal(ncol(row_col_subset), 1L)
  
  # Row and col subset that contains geo_value and time_value - should be epi_df
  row_col_subset = toy_epi_df[2:3,1:3]
  expect_true(is_epi_df(row_col_subset))
  expect_equal(nrow(row_col_subset), 2L)
  expect_equal(ncol(row_col_subset), 4L)
  expect_identical(attributes(row_col_subset)$metadata$geo_type, att_toy$geo_type)
  expect_identical(attributes(row_col_subset)$metadata$time_type, att_toy$time_type)
  expect_identical(attributes(row_col_subset)$metadata$as_of, att_toy$as_of)
})