# as_epi_df errors on nonunique epikeytime

    Code
      as_epi_df(tibble::tibble(geo_value = 1, time_value = 1, value = 1:2), as_of = 5)
    Condition
      Error:
      ! Assertion on 'x' failed: There cannot be more than one row with the same combination of geo_value and time_value.  Problematic rows:
      # A tibble: 2 x 3
        geo_value time_value value
            <dbl>      <dbl> <int>
      1         1          1     1
      2         1          1     2
      i Common fixes:
      > If this is line list data, aggregate to counts or rates first.
      > If rows differ by a grouping column (e.g. age group), add it to `other_keys`.
      > If rows differ by signal name, use `signal_format = "long"` or `"wide"` (see `?as_epi_df`).

