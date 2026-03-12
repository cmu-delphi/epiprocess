test_date <- as.Date("2020-01-01")

test_df <- dplyr::tibble(
  geo_value = rep(letters[1:5], each = 10),
  time_value = rep(test_date + 1:10, 5),
  val = runif(50, 0, 15)
) %>% as_epi_df()

test_df_multi <- dplyr::tibble(
  geo_value = rep(c("ak", "al"), each = 10),
  age = rep(rep(c("adult", "child"), each = 5), 2),
  time_value = rep(test_date + 1:5, 4),
  val = runif(20, 0, 15)
) %>% as_epi_df(other_keys = "age")

test_df_many <- dplyr::tibble(
  geo_value = rep(as.character(1:100), each = 2),
  time_value = rep(test_date + 1:2, 100),
  val = runif(200, 0, 15)
) %>% as_epi_df()

test_that("plot_heatmap functionality (standard, multi-key, auto-select)", {
  # Standard heatmap
  p_std <- plot_heatmap(test_df, val)
  expect_s3_class(p_std, "ggplot")

  # Multiple keys on y-axis
  p_multi <- plot_heatmap(test_df_multi, val)
  expect_s3_class(p_multi, "ggplot")
  expect_snapshot(unique(p_multi$data$.y_axis))

  # Auto-select fill column
  expect_warning(p_auto <- plot_heatmap(test_df), class = "epiprocess__unspecified_plot_var")
  expect_snapshot(invisible(plot_heatmap(test_df)))
  expect_s3_class(p_auto, "ggplot")
})

test_that("plot_heatmap edge cases (subsampling and errors)", {
  # Key subsampling
  set.seed(123)
  expect_warning(
    p_sub <- plot_heatmap(test_df_many, val, .max_keys = 10),
    class = "epiprocess__autoplot__max_keys_exceeded"
  )
  expect_snapshot(invisible(plot_heatmap(test_df_many, val, .max_keys = 10)))
  expect_equal(length(unique(p_sub$data$geo_value)), 10)

  # Invalid input error
  df_invalid <- data.frame(a = 1, b = 2)
  expect_error(plot_heatmap(df_invalid), class = "epiprocess__plot_heatmap__invalid_x")
  expect_snapshot(plot_heatmap(df_invalid), error = TRUE)
})
