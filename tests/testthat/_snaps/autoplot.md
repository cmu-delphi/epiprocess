# autoplot_subsample_keys warning/hints

    Code
      epiprocess:::autoplot_subsample_keys(df_facets, .max_keys = 10, .interactive = FALSE)
    Condition
      Warning:
      Too many key combinations to display clearly. Showing 10 of 20.
      > To plot all keys, use `autoplot(..., .max_keys = Inf)`.
      > To explore all keys interactively, use `autoplot(..., .interactive = TRUE)`.
      > To plot specific keys, use `autoplot(..., .facet_filter = ...)`.
    Output
      An `epi_df` object, 30 x 4 with metadata:
      * geo_type  = custom
      * time_type = day
      * as_of     = 2023-01-04
      Latency (lag between last available observation and epi_df's as_of, by time series):
      * lag  = 1 days
      
      # A tibble: 30 x 4
         geo_value time_value  cases .facets
         <fct>     <date>      <dbl> <fct>  
       1 b         2023-01-01 -0.565 b      
       2 e         2023-01-01  0.404 e      
       3 f         2023-01-01 -0.106 f      
       4 l         2023-01-01  2.29  l      
       5 m         2023-01-01 -1.39  m      
       6 o         2023-01-01 -0.133 o      
       7 p         2023-01-01  0.636 p      
       8 q         2023-01-01 -0.284 q      
       9 r         2023-01-01 -2.66  r      
      10 s         2023-01-01 -2.44  s      
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
      [1] "Geo: ak"

# interactive plot sampling warning

    Code
      purrr::map_chr(plotly::plotly_build(p)$x$data, ~ .x$visible %||% "TRUE")
    Output
       [1] "legendonly" "legendonly" "legendonly" "legendonly" "legendonly"
       [6] "legendonly" "legendonly" "legendonly" "TRUE"       "legendonly"
      [11] "legendonly" "TRUE"       "legendonly" "legendonly" "legendonly"
      [16] "TRUE"       "TRUE"       "legendonly" "TRUE"       "legendonly"

