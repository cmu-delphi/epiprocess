
edf <- as_epi_df(tibble(
  geo_value = rep("nd", 10L),
  time_value = as.Date("2020-01-01") + 1:10 - 1L,
  value = 1:10
))

test_that("Forbidden epi_df methods catches omitted column names in slide comp", {
  for (f in list(mean, sum, prod, min, max, all, any, range)) {
    expect_error(edf %>% epi_slide(.window_size = 7L, ~ f(.x)),
                 class = "epiprocess__summarizer_on_entire_epi_df")
    expect_error(edf %>% group_by(geo_value) %>% epi_slide(.window_size = 7L, ~ f(.x)),
                 class = "epiprocess__summarizer_on_entire_epi_df")
  }
})

test_that("Forbidden epi_df methods have decent error messages", {
  expect_snapshot(error = TRUE, edf %>% epi_slide(.window_size = 7L, ~ mean(.x)))
  expect_snapshot(error = TRUE, edf %>% epi_slide(.window_size = 7L, ~ sum(.x)))
  expect_snapshot(error = TRUE, edf %>% epi_slide(.window_size = 7L, ~ min(.x)))
  expect_snapshot(error = TRUE, edf %>% epi_slide(.window_size = 7L, ~ range(.x)))
})
