test_that("as_tibble checks an attr to avoid decay to tibble", {
  edf <- jhu_csse_daily_subset
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "no_decay_to_tibble") <- TRUE
  expect_identical(class(as_tibble(edf)), c("epi_df", "tbl_df", "tbl", "data.frame"))
})

test_that("as_tibble ungroups if needed", {
  edf <- jhu_csse_daily_subset %>% group_by(geo_value)
  # removes the grouped_df class
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame")) 
  attr(edf, "no_decay_to_tibble") <- TRUE
  # removes grouped_df but not `epi_df`
  expect_identical(class(as_tibble(edf)), c("epi_df", "tbl_df", "tbl", "data.frame"))
})
