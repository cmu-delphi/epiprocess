test_that("as_tibble checks an attr to avoid decay to tibble", {
  edf <- jhu_csse_daily_subset
  expect_s3_class(as_tibble(edf), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "no_decay_to_tibble") <- TRUE
  expect_s3_class(as_tibble(edf), c("epi_df", "tbl_df", "tbl", "data.frame"))
})
