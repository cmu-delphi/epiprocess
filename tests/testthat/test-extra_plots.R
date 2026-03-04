test_that("plot_heatmap returns a ggplot object", {
  test_date <- as.Date("2020-01-01")
  df <- dplyr::tibble(
    geo_value = rep(letters[1:5], each = 10),
    time_value = rep(test_date + 1:10, 5),
    val = runif(50, 0, 15)
  ) %>% as_epi_df()

  # Default (Viridis)
  p <- plot_heatmap(df, val)
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles multiple keys on y-axis", {
  test_date <- as.Date("2020-01-01")
  df <- dplyr::tibble(
    geo_value = rep(c("ak", "al"), each = 10),
    age = rep(rep(c("adult", "child"), each = 5), 2),
    time_value = rep(test_date + 1:5, 4),
    val = runif(20, 0, 15)
  ) %>% as_epi_df(other_keys = "age")

  p <- plot_heatmap(df, val)
  expect_s3_class(p, "ggplot")
  # Check if y-axis uses interaction
  expect_setequal(
    unique(p$data$.y_axis),
    c("ak / adult", "ak / child", "al / adult", "al / child")
  )
})

test_that("plot_heatmap automatically selects fill column", {
  test_date <- as.Date("2020-01-01")
  df <- dplyr::tibble(
    geo_value = rep(letters[1:5], each = 10),
    time_value = rep(test_date + 1:10, 5),
    value1 = runif(50, 0, 15)
  ) %>% as_epi_df()

  expect_warning(p <- plot_heatmap(df), "unspecified")
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap subsamples keys", {
  set.seed(123)
  test_date <- as.Date("2020-01-01")
  # Create 100 geo_values
  df <- dplyr::tibble(
    geo_value = rep(as.character(1:100), each = 2),
    time_value = rep(test_date + 1:2, 100),
    val = runif(200, 0, 15)
  ) %>% as_epi_df()

  expect_warning(p <- plot_heatmap(df, val, .max_keys = 10), "Subsampling to 10 keys")
  expect_equal(length(unique(p$data$geo_value)), 10)
})

test_that("plot_heatmap errors on non-epi_df input", {
  df <- data.frame(a = 1, b = 2)
  expect_error(plot_heatmap(df), class = "epiprocess__epi_plot_heatmap_invalid_x")
})
