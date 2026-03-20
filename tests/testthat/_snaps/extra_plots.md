# plot_heatmap functionality (standard, multi-key, auto-select, multi-var)

    Code
      unique(p_multi$data$.y_axis)
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
      head(p_multi_var$data, 10)
    Output
      # A tibble: 10 x 5
         time_value geo_value .response_name .response .y_axis
         <date>     <chr>     <chr>              <dbl> <fct>  
       1 2020-01-02 a         val                2.04  a      
       2 2020-01-02 a         val2               2.15  a      
       3 2020-01-03 a         val               11.8   a      
       4 2020-01-03 a         val2               1.97  a      
       5 2020-01-04 a         val                6.80  a      
       6 2020-01-04 a         val2               0.710 a      
       7 2020-01-05 a         val                2.04  a      
       8 2020-01-05 a         val2               1.40  a      
       9 2020-01-06 a         val               13.3   a      
      10 2020-01-06 a         val2               2.82  a      

# plot_heatmap edge cases (subsampling and errors)

    Code
      invisible(plot_heatmap(test_df_many, val, .max_keys = 10))
    Condition
      Warning:
      Plotting 100 keys can be slow and hard to read. Subsampling to 10 keys.
      > To plot all keys, use `autoplot(..., .max_keys = Inf)`.
      > To explore all keys interactively, use `autoplot(..., .interactive = TRUE)`.

---

    Code
      plot_heatmap(df_invalid)
    Condition
      Error in `plot_heatmap()`:
      ! x must be an `epi_df` object.

