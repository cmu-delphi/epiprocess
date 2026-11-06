# plot_heatmap functionality (standard, multi-key, auto-select, multi-var)

    Code
      unique(p_multi$data$.key_interaction)
    Output
      [1] ak; adult ak; child al; adult al; child
      Levels: ak; adult al; adult ak; child al; child

---

    Code
      invisible(plot_heatmap(test_df))
    Condition
      Warning in `plot_heatmap()`:
      Plot variable was unspecified. Automatically selecting `val`.

---

    Code
      head(as_tibble(p_multi_var$data), 10)
    Output
      # A tibble: 10 x 5
         time_value geo_value .response_name .response .key_interaction
         <date>     <chr>     <chr>              <dbl> <fct>           
       1 2020-01-02 a         val              -1.21   a               
       2 2020-01-02 a         val2             -0.426  a               
       3 2020-01-03 a         val               1.46   a               
       4 2020-01-03 a         val2             -0.565  a               
       5 2020-01-04 a         val               0.0945 a               
       6 2020-01-04 a         val2             -1.53   a               
       7 2020-01-05 a         val              -1.21   a               
       8 2020-01-05 a         val2             -1.00   a               
       9 2020-01-06 a         val               1.87   a               
      10 2020-01-06 a         val2              0.0873 a               

# plot_heatmap edge cases (subsampling and errors)

    Code
      invisible(plot_heatmap(test_df_many, val, .max_keys = 10))
    Condition
      Warning:
      Too many key combinations to display clearly. Showing 10 of 100.
      > To plot all keys, use `plot_heatmap(..., .max_keys = Inf)`.

---

    Code
      plot_heatmap(df_invalid)
    Condition
      Error in `plot_heatmap()`:
      ! x must be an `epi_df` object.

# plot_heatmap standardization logic

    Code
      head(as_tibble(p_norm$data), 10)
    Output
      # A tibble: 10 x 5
         time_value geo_value .response_name .response .key_interaction
         <date>     <chr>     <chr>              <dbl> <fct>           
       1 2020-01-02 a         val              -1.21   a               
       2 2020-01-02 a         val2             -0.426  a               
       3 2020-01-03 a         val               1.46   a               
       4 2020-01-03 a         val2             -0.565  a               
       5 2020-01-04 a         val               0.0945 a               
       6 2020-01-04 a         val2             -1.53   a               
       7 2020-01-05 a         val              -1.21   a               
       8 2020-01-05 a         val2             -1.00   a               
       9 2020-01-06 a         val               1.87   a               
      10 2020-01-06 a         val2              0.0873 a               

