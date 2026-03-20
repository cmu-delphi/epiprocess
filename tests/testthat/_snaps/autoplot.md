# autoplot_subsample_keys warning/hints

    Code
      epiprocess:::autoplot_subsample_keys(df_many_keys, "geo_value", .max_keys = 10,
        .facet_used = TRUE, .interactive = FALSE)
    Condition
      Warning:
      Plotting 20 keys can be slow and hard to read. Subsampling to 10 keys.
      > To plot all keys, use `autoplot(..., .max_keys = Inf)`.
      > To explore all keys interactively, use `autoplot(..., .interactive = TRUE)`.
      > To plot specific keys, use `autoplot(..., .facet_filter = ...)`.
    Output
      An `epi_df` object, 30 x 3 with metadata:
      * geo_type  = custom
      * time_type = day
      * as_of     = 2023-01-04
      
      # A tibble: 30 x 3
         geo_value time_value   cases
         <fct>     <date>       <dbl>
       1 a         2023-01-01  1.37  
       2 c         2023-01-01  0.363 
       3 i         2023-01-01  2.02  
       4 j         2023-01-01 -0.0627
       5 k         2023-01-01  1.30  
       6 m         2023-01-01 -1.39  
       7 o         2023-01-01 -0.133 
       8 q         2023-01-01 -0.284 
       9 s         2023-01-01 -2.44  
      10 t         2023-01-01  1.32  
      # i 20 more rows

---

    Code
      epiprocess:::autoplot_subsample_keys(df_many_keys, "geo_value", .max_keys = 10,
        .facet_used = FALSE, .interactive = FALSE)
    Condition
      Warning:
      Plotting 20 keys can be slow and hard to read. Subsampling to 10 keys.
      > To plot all keys, use `autoplot(..., .max_keys = Inf)`.
      > To explore all keys interactively, use `autoplot(..., .interactive = TRUE)`.
    Output
      An `epi_df` object, 30 x 3 with metadata:
      * geo_type  = custom
      * time_type = day
      * as_of     = 2023-01-04
      
      # A tibble: 30 x 3
         geo_value time_value  cases
         <fct>     <date>      <dbl>
       1 b         2023-01-01 -0.565
       2 d         2023-01-01  0.633
       3 f         2023-01-01 -0.106
       4 l         2023-01-01  2.29 
       5 o         2023-01-01 -0.133
       6 p         2023-01-01  0.636
       7 q         2023-01-01 -0.284
       8 r         2023-01-01 -2.66 
       9 s         2023-01-01 -2.44 
      10 t         2023-01-01  1.32 
      # i 20 more rows

# autoplot interactive dropdown logic (epi_df and epi_archive)

    Code
      purrr::map_lgl(pb2$x$data, ~ .x$visible)
    Output
      [1]  TRUE FALSE

---

    Code
      pb2$x$layout$yaxis
    Output
      $domain
      [1] 0 1
      
      $automargin
      [1] TRUE
      
      $title
      [1] "cases"
      
      $fixedrange
      [1] TRUE
      

---

    Code
      pb3$x$layout$title$text
    Output
      [1] "All: ak"

# interactive plot sampling warning

    Code
      purrr::map_chr(plotly::plotly_build(p)$x$data, ~ .x$visible %||% "TRUE")
    Output
       [1] "legendonly" "legendonly" "legendonly" "legendonly" "legendonly"
       [6] "legendonly" "legendonly" "legendonly" "TRUE"       "legendonly"
      [11] "legendonly" "TRUE"       "legendonly" "legendonly" "legendonly"
      [16] "TRUE"       "TRUE"       "legendonly" "TRUE"       "legendonly"

