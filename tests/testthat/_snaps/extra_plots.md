# plot_heatmap functionality (standard, multi-key, auto-select)

    Code
      unique(p_multi$data$.y_axis)
    Output
      [1] ak / adult ak / child al / adult al / child
      Levels: ak / adult al / adult ak / child al / child

---

    Code
      invisible(plot_heatmap(test_df))
    Condition
      Warning in `plot_heatmap()`:
      Plot variable was unspecified. Automatically selecting `val`.

# plot_heatmap edge cases (subsampling and errors)

    Code
      invisible(plot_heatmap(test_df_many, val, .max_keys = 10))
    Condition
      Warning:
      Plotting 100 keys can be slow and hard to read. Subsampling to 10 keys.
      i To plot all keys, use `autoplot(..., .max_keys = Inf)`.
      i To explore all keys interactively, use `autoplot(..., .interactive = TRUE)`.

---

    Code
      plot_heatmap(df_invalid)
    Condition
      Error in `plot_heatmap()`:
      ! x must be an `epi_df` object.

