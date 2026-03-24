# as_epi_df auto-detects long format and pivots

    Code
      df <- as_epi_df(raw)
    Message
      as_epi_df: pivoting long to wide based on `signal` column.

# as_epi_df supports explicit long format via input_format and signal_var

    Code
      df <- as_epi_df(raw, input_format = "long", signal_var = "custom_signal")
    Message
      as_epi_df: pivoting long to wide based on `custom_signal` column.

# as_epi_df errors if signal_var or value column is missing in long format

    Code
      as_epi_df(raw, input_format = "long", signal_var = "signal")
    Condition
      Error in `pivot_epi_data()`:
      ! Pivoting long to wide requires a `value` column.

---

    Code
      as_epi_df(raw, input_format = "long", signal_var = "nonexistent")
    Condition
      Error in `pivot_epi_data()`:
      ! Column `nonexistent` not found in `x`.

# as_epi_archive auto-detects long format and pivots

    Code
      arch <- as_epi_archive(raw)
    Message
      as_epi_archive: converting long to wide by turning each signal into an archive separately and merging.

# as_epi_df drops extra metadata columns during auto-pivot

    Code
      edf <- as_epi_df(df)
    Message
      as_epi_df: pivoting long to wide based on `name` column.

# as_epi_archive handles long-format with LOCF during pivot

    Code
      arch <- as_epi_archive(tib)
    Message
      as_epi_archive: converting long to wide by turning each signal into an archive separately and merging.

# auto-pivot aborts when multiple signal candidates exist

    Code
      as_epi_df(raw)
    Condition
      Error in `pivot_epi_data()`:
      ! Multiple signal variable candidates found; please specify a single `signal_var`.

---

    Code
      as_epi_archive(raw_arch)
    Condition
      Error in `pivot_epi_archive()`:
      ! Multiple signal variable candidates found; please specify a single `signal_var`.

