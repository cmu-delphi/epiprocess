test_that("head and tail do not drop the epi_df class", {
  toy_epi_df <- tibble::tibble(
    x = 1:10,
    y = 1:10,
    time_value = rep(seq(
      as.Date("2020-01-01"),
      by = 1,
      length.out = 5
    ), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5)
  ) %>% epiprocess::as_epi_df()
  
  expect_true(is_epi_df(head(toy_epi_df)))
  expect_true(is_epi_df(tail(toy_epi_df)))
  expect_identical(attributes(head(toy_epi_df))$metadata$geo_type, attributes(toy_epi_df)$metadata$geo_type)
  expect_identical(attributes(head(toy_epi_df))$metadata$time_type, attributes(toy_epi_df)$metadata$time_type)
  expect_identical(attributes(head(toy_epi_df))$metadata$as_of, attributes(toy_epi_df)$metadata$as_of)
  expect_identical(attributes(tail(toy_epi_df))$metadata$geo_type, attributes(toy_epi_df)$metadata$geo_type)
  expect_identical(attributes(tail(toy_epi_df))$metadata$time_type, attributes(toy_epi_df)$metadata$time_type)
  expect_identical(attributes(tail(toy_epi_df))$metadata$as_of, attributes(toy_epi_df)$metadata$as_of)
})
