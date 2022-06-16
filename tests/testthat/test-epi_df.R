test_that("head and tail do not drop the epi_df class", {
  tib <- tibble::tibble(
    x = 1:10,
    y = 1:10,
    time_value = rep(seq(
      as.Date("2020-01-01"),
      by = 1,
      length.out = 5
    ), times = 2),
    geo_value = rep(c("ca", "hi"), each = 5)
  ) %>% epiprocess::as_epi_df()
  
  expect_true(is_epi_df(head(tib)))
  expect_true(is_epi_df(tail(tib)))
  expect_identical(attributes(head(tib))$metadata$geo_type, attributes(tib)$metadata$geo_type)
  expect_identical(attributes(head(tib))$metadata$time_type, attributes(tib)$metadata$time_type)
  expect_identical(attributes(head(tib))$metadata$as_of, attributes(tib)$metadata$as_of)
  expect_identical(attributes(tail(tib))$metadata$geo_type, attributes(tib)$metadata$geo_type)
  expect_identical(attributes(tail(tib))$metadata$time_type, attributes(tib)$metadata$time_type)
  expect_identical(attributes(tail(tib))$metadata$as_of, attributes(tib)$metadata$as_of)
})
