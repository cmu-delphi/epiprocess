set.seed(42)
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

test_df_multi_var <- dplyr::tibble(
  geo_value = rep(letters[1:5], each = 10),
  time_value = rep(test_date + 1:10, 5),
  val = runif(50, 0, 15),
  val2 = runif(50, 0, 5)
) %>% as_epi_df()


withr::with_rng_version("3.5.0", withr::with_seed(1410852, {
  test_that("plot_heatmap functionality (standard, multi-key, auto-select, multi-var)", {
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

    # Multiple response variables (facets)
    p_multi_var <- plot_heatmap(test_df_multi_var, val, val2)
    expect_s3_class(p_multi_var, "ggplot")
    expect_true(inherits(p_multi_var$facet, "FacetWrap"))
    expect_snapshot(head(p_multi_var$data, 10))
  })

  test_that("plot_heatmap edge cases (subsampling and errors)", {
    # Key subsampling
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
}))
