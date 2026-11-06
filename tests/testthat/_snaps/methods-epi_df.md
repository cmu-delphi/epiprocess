# print on 0-row edf does not malfunction

    Code
      as_epi_df(tibble(geo_value = character(), time_value = integer(), value = integer()),
      as_of = 5)
    Output
      An `epi_df` object, 0 x 3 with metadata:
      * geo_type  = state
      * time_type = integer
      * as_of     = 5
      # A tibble: 0 x 3
      # i 3 variables: geo_value <chr>, time_value <int>, value <int>

