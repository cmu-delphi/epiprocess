test_that("as_tibble checks an attr to avoid decay to tibble", {
  edf <- cases_deaths_subset
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "decay_to_tibble") <- TRUE
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "decay_to_tibble") <- FALSE
  expect_identical(class(as_tibble(edf)), c("epi_df", "tbl_df", "tbl", "data.frame"))
})

test_that("as_tibble ungroups if needed", {
  # tsibble is doing some method piracy, and overwriting as_tibble.grouped_df as of 1.1.5
  skip_if(packageVersion("tsibble") > "1.1.4")
  edf <- cases_deaths_subset %>% group_by(geo_value)
  # removes the grouped_df class
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "decay_to_tibble") <- TRUE
  expect_identical(class(as_tibble(edf)), c("tbl_df", "tbl", "data.frame"))
  attr(edf, "decay_to_tibble") <- FALSE
  # removes grouped_df but not `epi_df`
  expect_identical(class(as_tibble(edf)), c("epi_df", "tbl_df", "tbl", "data.frame"))
})
