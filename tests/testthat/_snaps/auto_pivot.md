# as_epi_df and as_epi_archive handle auto-detection and pivoting

    Code
      df <- as_epi_df(raw)
    Message
      Pivoting long to wide based on `signal` column.

---

    Code
      arch <- as_epi_archive(raw_arch)
    Message
      Pivoting long to wide based on `signal` column.

---

    Code
      edf <- as_epi_df(raw_extra)
    Message
      Pivoting long to wide based on `signal` column.

---

    Code
      arch_locf <- as_epi_archive(tib)
    Message
      Pivoting long to wide based on `signal` column.

# Explicit signal formats (long/wide) and guessing

    Code
      df_long <- as_epi_df(raw, signal_format = "long", signal_var = "custom_signal")
    Message
      Adding `custom_signal` to `other_keys`.

---

    Code
      df_long_guess <- as_epi_df(raw_guess, signal_format = "long")
    Message
      Adding `signal` to `other_keys`.

---

    Code
      df_wide <- as_epi_df(raw, signal_format = "wide", signal_var = "custom_signal")
    Message
      Pivoting long to wide based on `custom_signal` column.

---

    Code
      df_wide_guess <- as_epi_df(raw_guess, signal_format = "wide")
    Message
      Pivoting long to wide based on `signal` column.

